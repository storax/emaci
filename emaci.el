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

(require 'cl-lib)

(cl-defstruct emaci-job buildno status statusmsg buffer dir command mode highlight-regexp)

(defvar emaci-queue nil
  "A list of `emaci-job' structs. Jobs in the queue might already be running.")
(defvar emaci-history nil
  "A list of `emaci-job' structs. Jobs in the history are finished or cancled.")
(defvar emaci--buffer-job-alist nil
  "A mapping of buffers to jobs.")
(defvar emaci--build-counter 0
  "The global job counter.")

(defun emaci//get-buildno ()
  "Get new build number and increase the counter `emaci--build-counter'."
  (setq emaci--build-counter (+ 1 emaci--build-counter)))

(defun emaci//new-job (dir command mode highlight-regexp)
  "Create a new job which gets executed in DIR.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'."
  (let ((buildno (emaci//get-buildno)))
    (make-emaci-job
     :buildno buildno
     :status 'queued
     :statusmsg nil
     :buffer nil
     :dir dir
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun emaci//running-job-p ()
  "Return t if there is a running job."
  (let ((job (car emaci-queue)))
    (and job (eq (emaci-job-status job) 'running))))

(defun emaci//queue-job (job)
  "Add JOB to `emaci-queue'."
  (add-to-list 'emaci-queue job t))

(defun emaci//schedule (dir command &optional mode highlight-regexp)
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
global value of `compilation-highlight-regexp'."
  (let ((job (emaci//new-job dir command mode highlight-regexp)))
    (emaci//queue-job job)
    (unless (emaci//running-job-p)
      (emaci/execute-next))))

(defun emaci//compilation-finished (buffer msg)
  "Callback when compilation buffer finishes in BUFFER with MSG.

Calls `emaci//job-finished'."
  (let ((job (cdr (assoc buffer emaci--buffer-job-alist))))
    (when job
      (emaci//job-finished job 'finished msg))))

(defun emaci//job-finished (job status statusmsg)
  "Callback when JOB finished with STATUS and STATUSMSG and execute the next."
  (message "Job finished %s: %s; %s" job status statusmsg)
  (setf (emaci-job-status job) status)
  (setf (emaci-job-statusmsg job) statusmsg)
  (add-to-list 'emaci-history job t)
  (setq emaci-queue (delete job emaci-queue))
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
          (message "There is still a job running!")))))))

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
  (message "Execute: %s" job)
  (setf (emaci-job-status job) 'running)
  (setf (emaci-job-buffer job) (emaci//create-buffer job))
  (let ((default-directory (emaci-job-dir job)))
    (compilation-start
     (emaci-job-command job)
     (emaci-job-mode job)
     `(lambda (mode) (emaci//create-buffer-name ,job))
     (emaci-job-highlight-regexp job))))

(add-hook 'compilation-finish-functions 'emaci//compilation-finished)

(provide 'emaci)

;;; emaci.el ends here
