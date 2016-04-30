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

(defgroup emaci nil
  "Customization group for emaci."
  :prefix "emaci-"
  :group 'emacs
  :package-version '(emaci . "0.1"))

(defcustom emaci-save-dir "~/.emaci/"
   "Directory where emaci saves history and logs."
   :type 'string
   :group 'emaci
   :package-version '(emaci . "0.1"))

(cl-defstruct emaci-job
  buildno queue status statusmsg exitcode datecreated datefinished
  oldref ref stashes buffer dir command mode highlight-regexp)

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

The job is created for QUEUE. If QUEUE is nil, use default queue.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'."
  (let ((buildno (emaci//get-buildno queue))
        (queue (if queue queue "*default*")))
    (make-emaci-job
     :buildno buildno
     :queue queue
     :status 'queued
     :statusmsg nil
     :exitcode nil
     :datecreated (current-time)
     :datefinished nil
     :buffer nil
     :dir dir
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun emaci//running-job-p (&optional queue)
  "Return t if there is a running job."
  (let* ((queue (if queue queue "*default*"))
         (job (cadr (assoc queue emaci-queue))))
    (and job (eq (emaci-job-status job) 'running))))

(defun emaci//queue-job (job)
  "Add JOB to QUEUE in `emaci-queue'."
  (let ((queue (emaci-job-queue job)))
    (if (assoc queue emaci-queue)
        (setf (cdr (assoc queue emaci-queue)) (append (cdr (assoc queue emaci-queue)) (list job)))
      (add-to-list 'emaci-queue (cons queue (list job))))))

(defun emaci//schedule (queue dir command &optional mode highlight-regexp deferred)
  "Create and schedule a new job.

Schedule the job in QUEUE.
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
  (let ((job (emaci//new-job queue dir command mode highlight-regexp)))
    (emaci//queue-job job)
    (unless (or (emaci//running-job-p queue) deferred)
      (emaci/execute-next queue))))

(defun storax//compilation-exit-function (status code msg)
  "Set the exitcode on the job with STATUS exit CODE and MSG."
  (let ((job (cdr (assoc buffer emaci--buffer-job-alist))))
    (when job
      (setf (emaci-job-exitcode job) code))))

(defun emaci//compilation-finished (buffer msg)
  "Callback when compilation buffer finishes in BUFFER with MSG.

Calls `emaci//job-finished'."
    (let ((job (cdr (assoc buffer emaci--buffer-job-alist))))
    (when job
      (emaci//job-finished job 'finished msg))))

(defun emaci//get-log-filepath (job)
  "Return filepath to a file for the output of JOB."
  (let* ((expanded
          (directory-file-name
           (expand-file-name emaci-save-dir)))
         (directory (directory-file-name
                     (concat (file-name-as-directory expanded)
                             (file-name-as-directory "logs")
                             (file-name-as-directory (emaci-job-queue job)))))
         (fullpath (concat (file-name-as-directory directory)
                           (format "build_%04d.log" (emaci-job-buildno job)))))
    (make-directory directory t)
    fullpath))

(defun emaci//save-log (job)
  "Save the output of the given job."
  (let ((buffer (emaci-job-buffer job)))
    (when buffer
      (with-current-buffer buffer
          (save-excursion
            (write-region (point-min) (point-max) (emaci//get-log-filepath job)))))))

(defun emaci//job-finished (job status statusmsg)
  "Callback when JOB finished with STATUS and STATUSMSG and execute the next."
  (setf (emaci-job-status job) status)
  (setf (emaci-job-statusmsg job) statusmsg)
  (setf (emaci-job-datefinished job) (current-time))
  (emaci//move-job-to-history job)
  (emaci//save-vars)
  (emaci//save-log job)
  (emaci/execute-next (emaci-job-queue job)))

(defun emaci/execute-next (&optional queue)
  "Execute the next job in the queue."
  (interactive)
  (let* ((queue (if queue queue "*default*"))
         (job (cadr (assoc queue emaci-queue))))
    (when job
      (let ((status (emaci-job-status job)))
        (cond
         ((eq (emaci-job-status job) 'queued)
          (emaci//execute job))
         ((eq (emaci-job-status job) 'running)
          (signal 'emaci-error-job-running job)))))))

(defun emaci//create-buffer-name (job)
  "Return a buffer name for JOB."
  (format "%s: Build #%s" (emaci-job-queue job) (emaci-job-buildno job)))

(defun emaci//create-buffer (job)
  "Create a buffer name for JOB."
  (let ((buffer (get-buffer-create (emaci//create-buffer-name job))))
    (add-to-list 'emaci--buffer-job-alist (cons buffer job))
    buffer))

(defun emaci//execute (job)
  "Execute the next JOB."
  (setf (emaci-job-status job) 'running)
  (setf (emaci-job-buffer job) (buffer-name (emaci//create-buffer job)))
  (let ((default-directory (emaci-job-dir job)))
    (compilation-start
     (emaci-job-command job)
     (emaci-job-mode job)
     `(lambda (mode) (emaci//create-buffer-name ,job))
     (emaci-job-highlight-regexp job))))

(defun emaci//stashes ()
  "Return a list of stashes."
  (let ((stashesstr (vc-git--run-command-string nil "stash" "list" "--pretty=format:%H")))
    (if (or (not stashesstr) (equal "" stashesstr))
        nil
      (split-string stashesstr "\n"))))

(defun emaci//current-commit ()
  "Return the current commit."
  (vc-git--rev-parse "HEAD"))

(defun emaci//branches ()
  "Return a list with all branches."
  (let ((branches (vc-git-branches)))
    (if (car branches)
        branches
      nil)))

(defun emaci//signal-job (sigcode job)
  "Send SIGCODE to JOB.

SIGCODE may be an integer, or a symbol whose name is a signal name."
  (let* ((job-buffer (get-buffer (emaci-job-buffer job)))
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
  (let ((queue (emaci-job-queue job)))
    (if (assoc queue emaci-history)
        (setf (cdr (assoc queue emaci-history)) (append (cdr (assoc queue emaci-history)) (list job)))
      (add-to-list 'emaci-history (cons queue (list job))))
    (setf (cdr (assoc queue emaci-queue)) (delete job (cdr (assoc queue emaci-queue))))))

(defun emaci//save-var (var file)
  "Save the given VAR to FILE in `emaci-save-dir'."
  (let ((expanded
         (directory-file-name
          (expand-file-name emaci-save-dir))))
    (make-directory expanded t)
    (with-temp-buffer
      (prin1 (symbol-value var) (current-buffer))
      (write-file (concat (file-name-as-directory expanded) file)))))

(defun emaci//save-history ()
  "Save the history to file."
  (emaci//save-var 'emaci-history "history.el"))

(defun emaci//save-queue ()
  "Save the queue to file."
  (emaci//save-var 'emaci-queue "queue.el"))

(defun emaci//save-build-counter ()
  "Save the build-counter to file."
  (emaci//save-var 'emaci--build-counter "build-counter.el"))

(defun emaci//save-vars ()
  "Save history, queue and other variables to the `emaci-save-dir'."
  (emaci//save-history)
  (emaci//save-queue)
  (emaci//save-build-counter))

(defun emaci//load-var (var file)
  "Load the given VAR from FILE in `emaci-load-dir'."
  (let* ((expanded
          (directory-file-name
           (expand-file-name emaci-save-dir)))
         (fullpath (concat (file-name-as-directory expanded) file)))
    (when (file-exists-p fullpath)
      (with-temp-buffer
        (insert-file-contents fullpath)
        (goto-char (point-min))
        (set var (read (current-buffer)))))))

(defun emaci//load-history ()
  "Load the history from file."
  (emaci//load-var 'emaci-history "history.el"))

(defun emaci//load-queue ()
  "Load the queue from file."
  (emaci//load-var 'emaci-queue "queue.el"))

(defun emaci//load-build-counter ()
  "Load the build-counter from file."
  (emaci//load-var 'emaci--build-counter "build-counter.el"))

(defun emaci/load-vars ()
  "Load history, queue and other variables from `emaci-save-dir'."
  (emaci//load-history)
  (emaci//load-queue)
  (emaci//load-build-counter))

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

(defun emaci/list-queues ()
  (mapcar 'car emaci-queue))

(defun emaci/select-queue ()
  (interactive)
  (list
   (completing-read
    "Select mode for compilation buffer: "
    (emaci/list-queues)
    nil nil nil nil "*default*")))

(defun emaci/submit-job (queue dir command &optional mode highlight-regexp)
  (interactive (append
                (emaci/select-queue)
                (emaci/get-dir-command)
                (emaci/select-mode)))
  (emaci//schedule queue dir command mode highlight-regexp))

(defun emaci/submit-job-comint (queue dir command &optional highlight-regexp)
  (interactive (append
                (emaci/select-queue)
                (emaci/get-dir-command)))
  (emaci//schedule queue dir command t highlight-regexp))

(add-hook 'compilation-finish-functions 'emaci//compilation-finished)

(provide 'emaci)

;;; emaci.el ends here
