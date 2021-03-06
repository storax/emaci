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

(cl-defstruct emaci-job
  buildno queue status statusmsg exitcode datestarted datefinished
  oldref ref stashes buffer dir command mode highlight-regexp metadata)

(cl-defstruct emaci-section arglist)

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
(defvar emaci-finished-hook nil
  "Hooks are run after the job has finished.
They should take an `emaci-job' as argument.")
(defvar emaci-started-hook nil
  "Hooks are run after the job has started.
They should take an `emaci-job' as argument.")
(defvar emaci-mode-hook nil
  "Hooks for the emaci management buffer mode.")

(defvar emaci-rms-data
  (split-string
   (with-temp-buffer
     (insert-file-contents
      (concat (file-name-as-directory (file-name-directory load-file-name)) "rms.data"))
     (buffer-string))
   "\n" t)
  "A list of Richard Stallman quotes.")

(defvar emaci-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "TAB") 'emaci/toggle-section)
    (define-key map (kbd "RET") 'emaci/mgmt-ret)
    (define-key map (kbd "k") 'emaci/kill-job)
    (define-key map (kbd "c") 'emaci/cancel-job)
    map)
  "Keymap for emaci major mode.")

(defvar-local emaci--sections nil
  "Internal list for emaci sections in the management browser.")

(defcustom emaci-save-dir "~/.emaci/"
   "Directory where emaci saves history and logs."
   :type 'string
   :group 'emaci)

(defcustom emaci-max-history-len-status 50
  "Show max number of jobs from history in status buffer."
  :type 'int
  :group 'emaci)

(defcustom emaci-enable-rms nil
  "Enable Richard Stallman quotes."
  :type 'boolean
  :group 'emaci)

(defface emaci-statbar-success-face
  '((t :background "#4de137"))
  "Face for a successful job in the status bar."
  :group 'emaci)

(defface emaci-statbar-fail-face
  '((t :background "#ff4444"))
  "Face for a failed job in the status bar."
  :group 'emaci)

(defface emaci-statbar-canceled-face
  '((t :background "grey"))
  "Face for a cancled job in the status bar."
  :group 'emaci)

(defface emaci-statbar-running-face
  '((t :background "#fff574"))
  "Face for a running job in the status bar."
  :group 'emaci)

(defface emaci-statbar-queued-face
  '((t :background "#51b8e1"))
  "Face for a cancled job in the status bar."
  :group 'emaci)

(defface emaci-statbar-unknown-face
  '((t :background "blue"))
  "Face for a job of unknown status in the status bar."
  :group 'emaci)

(defface emaci-mgmt-success-face
  '((t :foreground "#4de137"))
  "Face for a successful job in then mgmt buffer."
  :group 'emaci)

(defface emaci-mgmt-fail-face
  '((t :foreground "#ff4444"))
  "Face for a failed job in the mgmt buffer."
  :group 'emaci)

(defface emaci-mgmt-canceled-face
  '((t :foreground "grey"))
  "Face for a cancled job in the mgmt buffer."
  :group 'emaci)

(defface emaci-mgmt-running-face
  '((t :foreground "#fff574"))
  "Face for a running job in the mgmt buffer."
  :group 'emaci)

(defface emaci-mgmt-queued-face
  '((t :foreground "#51b8e1"))
  "Face for a cancled job in the mgmt buffer."
  :group 'emaci)

(defface emaci-mgmt-unknown-face
  '((t :foreground "blue"))
  "Face for a job of unknown status in the mgmt buffer."
  :group 'emaci)

(define-error 'emaci-error "Something went wrong with emaci, sry.")
(define-error 'emaci-error-job-running "Job is already running." 'emaci-error)

(defun emaci//get-buildno (&optional queue)
  "Get new build number and increase the counter `emaci--build-counter'.
If QUEUE is non-nil, use the counter for that queue.
If QUEUE is not in the counter, it is added to it, starting with 1."
  (let ((queue (or queue "*default*")))
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
        (queue (or queue "*default*")))
    (make-emaci-job
     :buildno buildno
     :queue queue
     :status 'queued
     :dir dir
     :ref branch
     :stashes stashes
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun emaci//running-job-p (&optional queue)
  "Return t if there is a running job in QUEUE."
  (let* ((queue (or queue "*default*"))
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

If DEFERRED is non-nil, don't execute the job right away if queue is empty.

Return the schedules job."
  (let ((job (emaci//new-job queue dir command branch stashes mode highlight-regexp)))
    (emaci//queue-job job)
    (if (or (emaci//running-job-p queue) deferred)
        (emaci//mgmt-buffer-update)
      (emaci/execute-next queue))
    job))

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
  (run-hook-with-args 'emaci-finished-hook job)
  (emaci//git-revert job)
  (emaci/execute-next (emaci-job-queue job)))

(defun emaci/execute-next (&optional queue)
  "Execute the next job in the QUEUE."
  (interactive (list (emaci//select-queue)))
  (let* ((queue (or queue "*default*"))
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
  (setf (emaci-job-datestarted job) (current-time))
  (let ((default-directory (emaci-job-dir job)))
    (emaci//git-apply job)
    (compilation-start
     (emaci-job-command job)
     (emaci-job-mode job)
     `(lambda (mode) (emaci//create-buffer-name ,job))
     (emaci-job-highlight-regexp job)))
  (run-hook-with-args 'emaci-started-hook job))

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
  "Set JOB status to `canceled'."
  (let ((job-status (emaci-job-status job)))
    (when (or (eq job-status 'running) (eq job-status 'queued))
      (setf (emaci-job-status job) 'canceled)
      (emaci//move-job-to-history job))))

(defun emaci/cancel-job (job)
  "Cancel JOB by interrupting the process if it is running."
  (interactive (list (emaci//select-job "Cancel Job: " (emaci//select-queue))))
  (when job
    (let ((job-status (emaci-job-status job)))
      (when (eq job-status 'running)
        (emaci//signal-job 2 job))
      (emaci//cancel-job1 job))
    (emaci//mgmt-buffer-update)))

(defun emaci/kill-job (job)
  "Cancel JOB by killing the process if it is running."
  (interactive (list (emaci//select-job "Kill Job: " (emaci//select-queue))))
  (let ((job-status (emaci-job-status job)))
    (when (eq job-status 'running)
      (emaci//signal-job 9 job))
    (emaci//cancel-job1 job))
  (emaci//mgmt-buffer-update))

(defun emaci//move-job-to-history (job)
  "Remove JOB from queue and put it in history."
  (let ((queue (emaci-job-queue job)))
    (when (member job (assoc queue emaci-queue))
      (progn
        (if (assoc queue emaci-history)
            (setf (cdr (assoc queue emaci-history)) (append (cdr (assoc queue emaci-history)) (list job)))
          (add-to-list 'emaci-history (cons queue (list job))))
        (setf (cdr (assoc queue emaci-queue)) (delete job (cdr (assoc queue emaci-queue))))))))

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
See `compilation-start'.

Return the submitted job."
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
See `compilation-start'.  For mode, t will be used.

Return the submitted job."
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
  (propertize "Queues:\n" 'face 'outline-1))

(defun emaci//mgmt-buffer-queue-heading (queuename)
  "Return a heading for QUEUENAME for the emaci management buffer."
   (propertize (concat queuename ":") 'face 'outline-2))

(defun emaci//mgmt-buffer-format-job-for-statusbar (job)
  "Return a statusbar segment for JOB."
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
                   'emaci-statbar-unknown-face))
                 'statjob job)))
      (if (eq (emaci-job-status job) 'running)
          (concat " " tick " ")
        tick))))

(defun emaci//mgmt-propface-label (str)
  "Return STR with `outline-4'."
  (propertize str 'face 'outline-4))

(defun emaci//mgmt-format-status-detail (job)
  "Return formatted status detail for JOB."
  (format "%s   %-11s"
          (emaci//mgmt-propface-label "Status:")
          (emaci-job-status job)))

(defun emaci//mgmt-format-exitcode-detail (job)
  "Return formatted code detail for JOB."
  (let ((code (emaci-job-exitcode job)))
    (format "%s %s"
            (emaci//mgmt-propface-label "Exitcode:")
            (if code (emaci//mgmt-propface-for-status (number-to-string code) job) "--"))))

(defun emaci//mgmt-format-duration-detail (job)
  "Return formatted duration detail for JOB."
  (let* ((started (emaci-job-datestarted job))
         (ended (emaci-job-datefinished job))
         (duration (when (and started ended) (time-to-seconds (time-subtract ended started)))))
    (format "%s %-11s"
          (emaci//mgmt-propface-label "Duration:")
          (if duration (format-seconds "%yy %dd %hh %mm %z%ss" duration) "--"))))

(defun emaci//mgmt-format-started-detail (job)
  "Return formatted started detail for JOB."
  (let ((started (emaci-job-datestarted job)))
    (format "%s %s"
            (emaci//mgmt-propface-label "Started:")
            (if started (format-time-string "%d/%m/%Y %H:%M" started) "--"))))

(defun emaci//mgmt-format-branch-detail (job)
  "Return a formatted branch detail for JOB."
  (let ((branch (emaci-job-ref job)))
    (format "%s   %s"
          (emaci//mgmt-propface-label "Branch:")
          (if branch branch "--"))))

(defun emaci//mgmt-format-command-detail (job &optional len)
  "Return formatted command detail for JOB.

LEN is the maximum length of the command until it gets cut off.
Defaults to 80."
  (let ((len (or len 80))
        (cmd (emaci-job-command job)))
    (format "%s\n%s"
            (emaci//mgmt-propface-label "Command:")
            (if (> (length cmd) len)
                (concat "..." (substring cmd (* -1 len)))
              cmd))))

(defun emaci//mgmt-format-metadata-detail (job)
  "Return formatted metadata for JOB."
  (let ((metadata (emaci-job-metadata job)))
    (if metadata
        (concat
         "\n"
         (mapconcat
          (lambda (metadata)
            (let ((title (car metadata))
                  (data (cdr metadata)))
              (format
               "%s\n%s"
               (emaci//mgmt-propface-label (concat title ":"))
               data)))
          metadata
          "\n"))
      "")))

(defun emaci//mgmt-format-details (job)
  "Return formated JOB details."
  (format
   "\n%s %s\n%s %s\n%s\n%s%s%s"
   (emaci//mgmt-format-status-detail job)
   (emaci//mgmt-format-exitcode-detail job)
   (emaci//mgmt-format-duration-detail job)
   (emaci//mgmt-format-started-detail job)
   (emaci//mgmt-format-branch-detail job)
   (emaci//mgmt-format-command-detail job)
   (emaci//mgmt-format-metadata-detail job)
   (if emaci-enable-rms
       (concat
        "\n" (emaci//mgmt-propface-label "Richard Stallman: ")
        (nth (random (length emaci-rms-data)) emaci-rms-data))
     "")))

(defun emaci//mgmt-buffer-format-job (job)
  "Return a formated JOB."
  (when (emaci-job-p job)
    (let* ((queue (emaci-job-queue job))
          (status (emaci-job-status job))
          (code (emaci-job-exitcode job))
          (started (emaci-job-datestarted job))
          (ended (emaci-job-datefinished job))
          (duration (when (and started ended) (time-to-seconds (time-subtract ended started)))))
      (concat
       (emaci//mgmt-propertize
        (emaci//mgmt-propface-for-status (format "\n#%s:" (emaci-job-buildno job)) job)
        (list queue job) (list 'field job))
       (emaci//mgmt-propertize
        (emaci//mgmt-format-details job)
        (list queue job 'details) (list 'field job))
       ""))))

(defun emaci//mgmt-buffer-queue (queue)
  "Return the formated QUEUE for the emaci management buffer."
  (let* ((queuename (car queue))
         (queueitems (cdr queue))
         (historyitems (cl-subseq (cdr (assoc queuename emaci-history))
                                  (* -1 emaci-max-history-len-status)))
         (jobs (append historyitems queueitems)))
    (format
     "\n%s\n%s%s\n"
     (emaci//mgmt-buffer-queue-heading queuename)
     (emaci//mgmt-add-properties
      (concat
       (mapconcat
        (lambda (job) (emaci//mgmt-buffer-format-job-for-statusbar job))
        jobs "")
       "\n"
       (propertize "Jobs:" 'face 'outline-3))
      (list queuename))
     (mapconcat
      (lambda (job) (emaci//mgmt-buffer-format-job job))
      (reverse jobs) ""))))

(defun emaci//mgmt-buffer-update ()
  "Initialize the management buffer."
  (let ((buffer? (get-buffer "*Emaci*"))
        (buffer (get-buffer-create "*Emaci*"))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (unless buffer?
        (emaci-mode))
      (save-excursion
        (erase-buffer)
        (insert (emaci//mgmt-buffer-heading))
        (dolist (queue (reverse emaci-queue))
          (insert (emaci//mgmt-buffer-queue queue)))
        (unless buffer?
          (setq buffer-read-only t)
          (setq buffer-invisibility-spec nil)
          (mapcar
           (lambda (section)
             (if (> (length (emaci-section-arglist section)) 1)
                 (add-to-invisibility-spec (cons section t))))
           emaci--sections))))))

(defun emaci//mgmt-get-section-ident (args)
  "Return the section identifier for ARGS."
  (let (secargs sections)
    (dolist (arg args)
      (add-to-list 'secargs arg t)
      (add-to-list
       'sections
       (let* ((section (make-emaci-section :arglist secargs))
              (pos (cl-position section emaci--sections :test 'equal)))
         (if pos
             (nth pos emaci--sections)
           (progn
             (add-to-list 'emaci--sections section)
             section)))))
    sections))

(defun emaci//mgmt-propertize (string sechierarchy &optional props)
  "Propertize STRING.

SECHIERARCHY is used for the `invisible' property.
PROPS are also added."
  (apply
   'propertize string
   (append (list 'invisible (emaci//mgmt-get-section-ident sechierarchy)) props)))

(defun emaci//mgmt-add-properties (string sechierarchy)
  "Add properties to STRING.

SECHIERARCHY is used for the `invisible' property."
  (progn
    (add-text-properties
     0 (length string) (list 'invisible (emaci//mgmt-get-section-ident sechierarchy)) string)
    string))

(defun emaci//mgmt-propface-for-status (string job)
  "Return a STRING with a face according to status of JOB."
  (propertize
   string 'face
   (cond
    ((and (emaci-job-exitcode job) (zerop (emaci-job-exitcode job)))
     'emaci-mgmt-success-face)
    ((emaci-job-exitcode job)
     'emaci-mgmt-fail-face)
    ((eq (emaci-job-status job) 'canceled)
     'emaci-mgmt-canceled-face)
    ((eq (emaci-job-status job) 'running)
     'emaci-mgmt-running-face)
    ((eq (emaci-job-status job) 'queued)
     'emaci-mgmt-queued-face)
    (t
     'emaci-mgmt-unknown-face))))

(defun emaci//format-job-for-selection (job)
  "Return a concise description of JOB."
  (format
   "%s #%s: %s"
   (emaci-job-queue job)
   (emaci-job-buildno job)
   (emaci//mgmt-format-command-detail job)))

(defun emaci//select-job (prompt queue)
  "Return a job from QUEUE."
  (let* ((jobs (reverse (cdr (assoc queue emaci-queue))))
         (jobs-human (mapcar
                      (lambda (job) (emaci//format-job-for-selection job))
                      jobs))
         (seljob (completing-read prompt jobs-human nil t)))
    (nth (cl-position seljob jobs-human :test 'equal) jobs)))

(defun emaci//get-children-section (section all)
  "Get all children sections of SECTION.

If ALL is non-nil return also grandchildren."
  (let* ((arglist (emaci-section-arglist section))
         children)
    (mapc
     (lambda (cand)
       (let ((secarglist (emaci-section-arglist cand))
             (larg (length arglist)))
         (when (and (if all t (equal (+ 1 larg) (length secarglist)))
                    (equal (cl-subseq secarglist 0 larg) arglist)
                    (not (eq cand section)))
           (add-to-list 'children cand))))
     emaci--sections)
    children))

(defun emaci/toggle-section (arg)
  "Open and close sections.

If ARG is non-nil, Apply show/close recursively."
  (interactive "P")
  (let* ((prop (car (get-text-property (point) 'invisible)))
         (arglist (when (emaci-section-p prop) (emaci-section-arglist prop))))
    (when arglist
      (let ((children (emaci//get-children-section prop arg)))
        (if children
            (let ((func (if (member (cons (car children) t) buffer-invisibility-spec)
                            'remove-from-invisibility-spec
                          'add-to-invisibility-spec)))
              (mapc
               `(lambda (child)
                  (,func (cons child t)))
               children))
          (if (member (cons prop t) buffer-invisibility-spec)
              (remove-from-invisibility-spec (cons prop t))
            (add-to-invisibility-spec (cons prop t))))
        (force-window-update (get-buffer-window (current-buffer)))))))

(defun emaci//show-log (job)
  "Show the buffer of JOB if it exists or try to open the log file."
  (when (emaci-job-p job)
    (let ((buffer (get-buffer (emaci-job-buffer job))))
      (if buffer
          (display-buffer buffer)
        (let ((path (emaci//get-log-filepath job)))
          (when (file-exists-p path)
            (find-file-other-window path)))))))

(defun emaci//find-job-field (job)
  "Goto the field with JOB as property value."
  (when (emaci-job-p job)
    (goto-char (point-min))
    (let* ((buffer (current-buffer))
           (pos (next-property-change (point-min) buffer)))
      (while pos
        (if (eq job (get-text-property pos 'field buffer))
            (progn (goto-char (+ 1 pos))
                   (setq pos nil))
          (setq pos (next-property-change pos buffer))))
      (let* ((sections (get-text-property (point) 'invisible))
             (childs (emaci//get-children-section (car sections) nil))
             (children (emaci//get-children-section (cadr sections) nil))
             (allsections (append sections childs children)))
        (dolist (section allsections)
          (remove-from-invisibility-spec (cons section t)))
        (force-window-update (get-buffer-window (current-buffer)))))))

(defun emaci/mgmt-ret ()
  "When on status bar, goto job, when on job show log."
  (interactive)
  (let ((statjob (get-text-property (point) 'statjob (current-buffer)))
        (field (get-text-property (point) 'field (current-buffer))))
    (if statjob
        (emaci//find-job-field statjob)
      (emaci//show-log field))))

(define-derived-mode emaci-mode special-mode "Emaci"
  "Major mode for emaci management buffer.

\\{emaci-mode-map}

Entry to this mode calls the value of `emaci-mode-hook'
if that value is non-nil.")

(defun emaci/mgmt-buffer ()
  "Show the emaci management buffer."
  (interactive)
  (emaci//mgmt-buffer-update)
  (display-buffer "*Emaci*"))

(provide 'emaci)

;;; emaci.el ends here
