;;; emaci.el --- scheduler for compilations in emacs

;; Copyright (C) 2016 by David ZUBER

;; Author: David ZUBER <zuber.david@gmx.de>
;; URL: https://github.com/storax/emaci
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emaci provides a scheduler to queue compilation jobs and execute them automatically.

;;; Code:

(eval-when-compile (require 'cl-lib))

(cl-defstruct emaci-job buildno status statusmsg datecreated datefinished buffer dir command mode highlight-regexp)

(defvar emaci-queue nil
  "An alist of queue names as car and a list of `emaci-job' structs as cdr.
Jobs in the queue might already be running.")
(defvar emaci-history nil
  "An alist of queue names as car and a list of `emaci-job' structs as cdr.
Jobs in the history are finished or cancled.")
(defvar emaci--buffer-job-alist nil
  "A mapping of buffers to jobs.")
(defvar emaci--build-counter nil
  "The global job counter.
It is an alist where the queue names as car and the counter number as cdr.")
(defvar emaci-mode-history nil
  "History for selecting modes.")

(define-error 'emaci-error "Something went wrong with emaci, sry.")
(define-error 'emaci-error-job-running "Job is already running." 'emaci-error)

(defun emaci//get-buildno (&optional queue)
  "Get new build number and increase the counter `emaci--build-counter'.
If QUEUE is non-nil, use the counter for that queue.
If QUEUE is not in the counter, it is added to it, starting with 1."
  (let ((queue (if queue queue "*default*")))
    (unless (assoc queue emaci--build-counter)
      (add-to-list 'emaci--build-counter (cons queue 0)))
    (let ((count (cdr (assoc queue emaci--build-counter))))
      (setf (cdr (assoc queue emaci--build-counter)) (+ 1 count)))))

(defun emaci//new-job (queue dir command mode highlight-regexp)
  "Create a new job which gets executed in DIR.

The job is creaed for QUEUE. If QUEUE is nil, use default queue.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'."
  (let ((buildno (emaci//get-buildno queue)))
    (make-emaci-job
     :buildno buildno
     :status 'queued
     :statusmsg nil
     :datecreated (current-time)
     :datefinished nil
     :buffer nil
     :dir dir
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun emaci//running-job-p ()
  "Return t if there is a running job."
  (let ((job (car emaci-queue)))
    (and job (eq (emaci-job-status job) 'running))))

(defun emaci//queue-job (job &optional queue)
  "Add JOB to QUEUE in `emaci-queue'."
  (let ((queue (if queue queue "*default*")))
    (if (assoc queue emaci-queue)
        (setf (cdr (assoc queue emaci-queue)) (append (cdr (assoc queue emaci-queue)) (list job)))
      (add-to-list 'emaci-queue (cons queue (list job))))))

(defun emaci//schedule (dir command &optional mode highlight-regexp deferred)
  "Create and schedule a new job.

The job will get executed in DIR.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

If DEFERRED is non-nil, don't execute the job right away if queue is empty."
  (let ((job (emaci//new-job dir command mode highlight-regexp)))
    (emaci//queue-job job)
    (unless (or (emaci//running-job-p) deferred)
      (emaci/execute-next))))

(defun emaci//compilation-finished (buffer msg)
  "Callback when compilation buffer finishes in BUFFER with MSG.

Calls `emaci//job-finished'."
  (let ((job (cdr (assoc buffer emaci--buffer-job-alist))))
    (when job
      (emaci//job-finished job 'finished msg))))

(defun emaci//job-finished (job status statusmsg)
  "Callback when JOB finished with STATUS and STATUSMSG and execute the next."
  (setf (emaci-job-status job) status)
  (setf (emaci-job-statusmsg job) statusmsg)
  (setf (emaci-job-datefinished job) (current-time))
  (emaci//move-job-to-history job)
  (emaci/execute-next))

(defun emaci/execute-next ()
  "Execute the next job in the queue."
  (interactive)
  (let ((job (car emaci-queue)))
    (when job
      (let ((status (emaci-job-status job)))
        (cond
         ((eq (emaci-job-status job) 'queued)
          (emaci//execute job))
         ((eq (emaci-job-status job) 'running)
          (signal 'emaci-error-job-running job)))))))

(defun emaci//create-buffer-name (job)
  "Return a buffer name for JOB."
  (format "Build #%s" (emaci-job-buildno job)))

(defun emaci//create-buffer (job)
  "Create a buffer name for JOB."
  (let ((buffer (get-buffer-create (emaci//create-buffer-name job))))
    (add-to-list 'emaci--buffer-job-alist (cons buffer job))
    buffer))

(defun emaci//execute (job)
  "Execute the next JOB."
  (setf (emaci-job-status job) 'running)
  (setf (emaci-job-buffer job) (emaci//create-buffer job))
  (let ((default-directory (emaci-job-dir job)))
    (compilation-start
     (emaci-job-command job)
     (emaci-job-mode job)
     `(lambda (mode) (emaci//create-buffer-name ,job))
     (emaci-job-highlight-regexp job))))

(defun emaci//signal-job (sigcode job)
  "Send SIGCODE to JOB.

SIGCODE may be an integer, or a symbol whose name is a signal name."
  (let* ((job-buffer (emaci-job-buffer job))
         (job-status (emaci-job-status job))
         (job-proc (get-buffer-process job-buffer)))
    (when (and job-proc (eq job-status 'running))
      (signal-process job-proc sigcode))))

(defun emaci//cancel-job1 (job)
  "Move JOB to history and set the status to `canceled'."
  (let ((job-status (emaci-job-status job)))
    (when (or (eq job-status 'running) (eq job-status 'queued))
      (setf (emaci-job-status job) 'canceled)
      (emaci//move-job-to-history job))))

(defun emaci/cancel-job (job)
  "Cancel JOB by interrupting the process if it is running."
  (interactive (list (car emaci-queue)))
  (let ((job-status (emaci-job-status job)))
    (when (eq job-status 'running)
      (emaci//signal-job 2 job))
    (emaci//cancel-job1 job)))

(defun emaci/kill-job (job)
  "Cancel JOB by killing the process if it is running."
  (interactive (list (car emaci-queue)))
  (let ((job-status (emaci-job-status job)))
    (when (eq job-status 'running)
      (emaci//signal-job 9 job))
    (emaci//cancel-job1 job)))

(defun emaci//move-job-to-history (job)
  "Remove JOB from queue and put it in history."
  (add-to-list 'emaci-history job t)
  (setq emaci-queue (delete job emaci-queue)))

(defun emaci/get-dir-command ()
  "Interactively return a directory and a command for emaci."
  (list (read-directory-name
         "Working directory: "
         (projectile-project-p)
         nil t)
        (read-shell-command "Command: " (car shell-command-history))))

(defun emaci/list-major-modes ()
  "Returns list of potential major mode names.
From Tobias Zawada (http://stackoverflow.com/questions/5536304/emacs-stock-major-modes-list)"
  (interactive)
  (let (l)
    (mapatoms #'(lambda (f) (and
                        (commandp f)
                        (string-match "-mode$" (symbol-name f))
                        ;; auto-loaded
                        (or (and (autoloadp (symbol-function f))
                              (let ((doc (documentation f)))
                                (when doc
                                  (and
                                   (let ((docSplit (help-split-fundoc doc f)))
                                     (and docSplit ;; car is argument list
                                        (null (cdr (read (car docSplit)))))) ;; major mode starters have no arguments
                                   (if (string-match "[mM]inor" doc) ;; If the doc contains "minor"...
                                       (string-match "[mM]ajor" doc) ;; it should also contain "major".
                                     t) ;; else we cannot decide therefrom
                                   ))))
                           (null (help-function-arglist f)))
                        (setq l (cons (symbol-name f) l)))))
    l))

(defun emaci/select-mode ()
  (interactive)
  (list
   (intern
    (completing-read
     "Select mode for compilation buffer: "
     (emaci/list-major-modes)
     nil t nil emaci-mode-history "comint-mode"))))

(defun emaci/submit-job (dir command &optional mode highlight-regexp)
  (interactive (append
                (emaci/get-dir-command)
                (emaci/select-mode)))
  (emaci//schedule dir command mode highlight-regexp))

(defun emaci/submit-job-comint (dir command &optional highlight-regexp)
  (interactive (emaci/get-dir-command))
  (emaci//schedule dir command t highlight-regexp))

(add-hook 'compilation-finish-functions 'emaci//compilation-finished)

(provide 'emaci)

;;; emaci.el ends here
