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

(require 'vc-git)
(require 'compile)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl))


(defgroup emaci nil
  "Customization group for emaci."
  :prefix "emaci-"
  :group 'emacs)

(defcustom emaci-save-dir "~/.emaci/"
   "Directory where emaci saves history and logs."
   :type 'string
   :group 'emaci)

(defcustom emaci-max-history-len-status 50
  "Show max number of jobs from history in status buffer."
  :type 'int
  :group 'emaci)

(defface emaci-statbar-success-face
  '((t :background "DarkGreen"))
  "Face for a successful job in the status bar."
  :group 'emaci)

(defface emaci-statbar-fail-face
  '((t :background "red"))
  "Face for a failed job in the status bar."
  :group 'emaci)

(defface emaci-statbar-cancled-face
  '((t :background "grey"))
  "Face for a cancled job in the status bar."
  :group 'emaci)

(defface emaci-statbar-running-face
  '((t :background "yellow"))
  "Face for a running job in the status bar."
  :group 'emaci)

(defface emaci-statbar-queued-face
  '((t :background "khaki"))
  "Face for a cancled job in the status bar."
  :group 'emaci)

(defface emaci-statbar-unknown-face
  '((t :background "blue"))
  "Face for a job of unknown status in the status bar."
  :group 'emaci)

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

(defun emaci//new-job (queue dir command branch stashes mode highlight-regexp)
  "Create a new job for QUEUE which gets executed in DIR.

The job is created for QUEUE.  If QUEUE is nil, use default queue.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

BRANCH is a git branch name or a ref.  It will be checkout out before
running COMMAND.

STASHES is a list of stashes to apply before running COMMAND or nil.

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
     :datecreated (current-time)
     :dir dir
     :ref branch
     :stashes stashes
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun emaci//running-job-p (&optional queue)
  "Return t if there is a running job in QUEUE."
  (let* ((queue (if queue queue "*default*"))
         (job (cadr (assoc queue emaci-queue))))
    (and job (eq (emaci-job-status job) 'running))))

(defun emaci//queue-job (job)
  "Add JOB to QUEUE in `emaci-queue'."
  (let ((queue (emaci-job-queue job)))
    (if (assoc queue emaci-queue)
        (setf (cdr (assoc queue emaci-queue)) (append (cdr (assoc queue emaci-queue)) (list job)))
      (add-to-list 'emaci-queue (cons queue (list job))))))

(defun emaci//schedule (queue dir command &optional branch stashes mode highlight-regexp deferred)
  "Create and schedule a new job.

Schedule the job in QUEUE.
The job will get executed in DIR.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

BRANCH is a git branch name or a ref.  It will be checkout out before
running COMMAND.

STASHES is a list of stashes to apply before running COMMAND or nil.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

If DEFERRED is non-nil, don't execute the job right away if queue is empty."
  (let ((job (emaci//new-job queue dir command branch stashes mode highlight-regexp)))
    (emaci//queue-job job)
    (if (or (emaci//running-job-p queue) deferred)
        (emaci//mgmt-buffer-update)
      (emaci/execute-next queue))))

(defun storax//compilation-exit-function (status code msg)
  "Set the exitcode on the job with STATUS exit CODE and MSG."
  (let ((job (cdr (assoc (current-buffer) emaci--buffer-job-alist))))
    (when job
      (setf (emaci-job-exitcode job) code)))
  (cons msg code))

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
  "Save the output of the given JOB."
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
  (emaci//git-revert job)
  (emaci/execute-next (emaci-job-queue job)))

(defun emaci/execute-next (&optional queue)
  "Execute the next job in the QUEUE."
  (interactive)
  (let* ((queue (if queue queue "*default*"))
         (job (cadr (assoc queue emaci-queue))))
    (when job
      (let ((status (emaci-job-status job)))
        (cond
         ((eq (emaci-job-status job) 'queued)
          (emaci//execute job))
         ((eq (emaci-job-status job) 'running)
          (signal 'emaci-error-job-running job)))))
    (emaci//mgmt-buffer-update)))

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
  (setf (emaci-job-oldref job) (emaci//current-commit (emaci-job-dir job)))
  (setf (emaci-job-buffer job) (buffer-name (emaci//create-buffer job)))
  (let ((default-directory (emaci-job-dir job)))
    (emaci//git-apply job)
    (compilation-start
     (emaci-job-command job)
     (emaci-job-mode job)
     `(lambda (mode) (emaci//create-buffer-name ,job))
     (emaci-job-highlight-regexp job))))

(defun emaci//stashes (dir)
  "Return a list of stashes of repo in DIR."
  (let* ((default-directory dir)
         (stashesstr (vc-git--run-command-string nil "stash" "list" "--pretty=format:%H")))
    (if (or (not stashesstr) (equal "" stashesstr))
        nil
      (split-string stashesstr "\n"))))

(defun emaci//stashes-human (dir)
  "Return a list of human readable stashes of repo in DIR."
  (let* ((default-directory dir)
         (stashesstr (vc-git--run-command-string nil "stash" "list")))
    (if (or (not stashesstr) (equal "" stashesstr))
        nil
      (split-string stashesstr "\n" t))))

(defun emaci//current-commit (dir)
  "Return the current commit of repo in DIR."
  (let ((default-directory dir))
    (vc-git-working-revision dir)))

(defun emaci//branches (dir)
  "Return a list with all branches of repo in DIR."
  (let* ((default-directory dir)
         (branches (vc-git-branches)))
    (if (car branches)
        branches
      nil)))

(defun emaci//switch-to-branch (branch dir)
  "Switch to BRANCH of repo in DIR."
  (let ((default-directory dir))
    (when (and branch (vc-git-responsible-p default-directory))
      (vc-git-checkout nil nil branch))))

(defun emaci//apply-stashes (stashes dir)
  "Apply STASHES to repo in DIR."
  (let ((default-directory dir))
    (when (vc-git-responsible-p default-directory)
      (dolist (stash stashes)
        (vc-git-stash-apply stash)))))

(defun emaci//revert-stashes (stashes dir)
  "Reverse apply STASHES to repo in DIR."
  (let ((default-directory dir))
    (when (vc-git-responsible-p default-directory)
      (dolist (stash (reverse stashes))
        (shell-command (format "git stash show -p %s | git apply -R" stash))))))

(defun emaci//git-revert (job)
  "Revert repo of JOB to the old status.
Checkout the branch it was one before the job got executed.
Revert all stashes that were applied by JOB."
  (let ((dir (emaci-job-dir job)))
    (emaci//revert-stashes (emaci-job-stashes job) dir)
    (emaci//switch-to-branch (emaci-job-oldref job) dir)))

(defun emaci//git-apply (job)
  "Switch to ref stored in JOB and apply the stashes."
  (let ((dir (emaci-job-dir job)))
    (emaci//switch-to-branch (emaci-job-ref job) dir)
    (emaci//apply-stashes (emaci-job-stashes job) dir)))

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
  (interactive (list (cadr (assoc (emaci//select-queue) emaci-queue))))
  (when job
    (let ((job-status (emaci-job-status job)))
      (when (eq job-status 'running)
        (emaci//signal-job 2 job))
      (emaci//cancel-job1 job))
    (emaci//mgmt-buffer-update)))

(defun emaci/kill-job (job)
  "Cancel JOB by killing the process if it is running."
  (interactive (list (cadr (assoc (emaci//select-queue) emaci-queue))))
  (let ((job-status (emaci-job-status job)))
    (when (eq job-status 'running)
      (emaci//signal-job 9 job))
    (emaci//cancel-job1 job))
  (emaci//mgmt-buffer-update))

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

(defun emaci/get-dir ()
  "Interactively return a directory."
  (let ((default (if (boundp 'projectile-project-p) (projectile-project-p) default-directory)))
    (read-directory-name
     "Working directory: "
     default
     nil t)))

(defun emaci/get-command ()
  "Interactively return a command for emaci."
  (read-shell-command "Command: " (car shell-command-history)))

(defun emaci//list-major-modes ()
  "Return list of potential major mode names.
From Tobias Zawada (http://stackoverflow.com/questions/5536304/emacs-stock-major-modes-list)"
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

(defun emaci//select-mode ()
  "Select a major mode to use in the compilation buffer."
  (intern
   (completing-read
    "Select mode for compilation buffer: "
    (emaci//list-major-modes)
    nil t nil emaci-mode-history "comint-mode")))

(defun emaci//list-queues ()
  "Return a list of queues."
  (mapcar 'car emaci-queue))

(defun emaci//select-queue ()
  "Select a queue from `emaci-queue'."
  (completing-read
   "Select mode for compilation buffer: "
   (emaci//list-queues)
   nil nil nil nil "*default*"))

(defun emaci//select-branch (dir)
  "Select a branch of repo in DIR."
  (when (vc-git-responsible-p dir)
    (completing-read
     "Select git branch: " (emaci//branches dir) nil 'confirm nil nil (emaci//current-commit dir))))

(defun emaci//select-stashes (dir)
  "Select stashes of repoin DIR."
  (when (vc-git-responsible-p dir)
    (let* ((stashes-human (emaci//stashes-human dir))
           (stashes (completing-read-multiple
                     "Select git stashes: " stashes-human nil t nil nil nil))
           (hashes (emaci//stashes dir)))
      (mapcar
       (lambda (stash) (nth (cl-position stash stashes-human :test 'equal) hashes))
       stashes))))

(defun emaci/submit-job (queue dir command &optional branch stashes mode highlight-regexp)
  "Submit a job to emaci.

Select a QUEUE for the job.  Each queue is consumed serially.
Multiple queues are executed in parallel.

DIR is the working directory for the COMMAND to execute.

If BRANCH is non-nil, switch to the branch before execution and switch back
to the branch that was checked out when execution started afterwards.

If STASHES is a list of stash hashes,
apply stashes before executing the command.
The changes will be reverted after the execution.

MODE and HIGHLIGHT-REGEXP are for the compilation buffer.
See `compilation-start'."
  (interactive (let ((dir (emaci/get-dir)))
                 (list
                  (emaci//select-queue)
                  dir
                  (emaci/get-command)
                  (emaci//select-branch dir)
                  (emaci//select-stashes dir)
                  (emaci//select-mode))))
  (emaci//schedule queue dir command branch stashes mode highlight-regexp))

(defun emaci/submit-job-comint (queue dir command &optional branch stashes highlight-regexp)
  "Submit a job to emaci.

Select a QUEUE for the job.  Each queue is consumed serially.
Multiple queues are executed in parallel.

DIR is the working directory for the COMMAND to execute.

If BRANCH is non-nil, switch to the branch before execution and switch back
to the branch that was checked out when execution started afterwards.

If STASHES is a list of stash hashes,
apply stashes before executing the command.
The changes will be reverted after the execution.

HIGHLIGHT-REGEXP is for the compilation buffer.
See `compilation-start'.  For mode, t will be used."
  (interactive (let ((dir (emaci/get-dir)))
                 (list
                  (emaci//select-queue)
                  dir
                  (emaci/get-command)
                  (emaci//select-branch dir)
                  (emaci//select-stashes dir))))
  (emaci//schedule queue dir command branch stashes t highlight-regexp))

(defun emaci/init ()
  "Set callbacks for compilation."
  (setq compilation-exit-message-function 'storax//compilation-exit-function)
  (add-hook 'compilation-finish-functions 'emaci//compilation-finished))

(emaci/init)

(defun emaci//mgmt-buffer-heading ()
  "Return a heading for the emaci management buffer."
  "* EMACI:\nQueues:\n")

(defun emaci//mgmg-buffer-format-job-for-statusbar (job)
  (when (emaci-job-p job)
    (let ((tick (propertize
                 " " 'face
                 (cond
                  ((and (emaci-job-exitcode job) (zerop (emaci-job-exitcode job)))
                   'emaci-statbar-success-face)
                  ((emaci-job-exitcode job)
                   'emaci-statbar-fail-face)
                  ((eq (emaci-job-status job) 'canceled)
                   'emaci-statbar-canceled-face)
                  ((eq (emaci-job-status job) 'running)
                   'emaci-statbar-running-face)
                  ((eq (emaci-job-status job) 'queued)
                   'emaci-statbar-queued-face)
                  (t
                   'emaci-statbar-unknown-face)))))
      (if (eq (emaci-job-status job) 'running)
          (concat " " tick " ")
        tick))))

(defun emaci//mgmt-buffer-queue (queue)
  "Return the formated QUEUE for the emaci management buffer."
  (let* ((queuename (car queue))
         (queueitems (cdr queue))
         (historyitems (subseq (cdr (assoc queuename emaci-history))
                               (* -1 emaci-max-history-len-status))))
    (format
     "** %s:\n%s\n"
     queuename
     (mapconcat
      (lambda (job)
        (emaci//mgmg-buffer-format-job-for-statusbar job))
      (append historyitems queueitems)
      ""))))

(defun emaci//mgmt-buffer-update ()
  "Initialize the management buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Emaci*"))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (emaci//mgmt-buffer-heading))
      (dolist (queue emaci-queue)
        (insert (emaci//mgmt-buffer-queue queue)))
      (setq buffer-read-only t))))

(provide 'emaci)

;;; emaci.el ends here
