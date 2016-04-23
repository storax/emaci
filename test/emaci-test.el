;;; test-emaci.el --- test emaci main functions

;; Copyright (C) 2016 by David ZUBER

;; Author: David ZUBER <zuber.david@gmx.de>

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

;;; Code:

(require 'ert)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'cl-lib)
(require 'emaci)

(defvar ert-async-timeout 10
  "Number of seconds to wait for callbacks before failing.")

(setq emaci-save-dir (make-temp-file "emaci" t))

(ert-deftest comp-finished-hook ()
    "Test if hook is installed"
    (should (member 'emaci//compilation-finished compilation-finish-functions)))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY with all variables let bound."
  `(let ((emaci-queue nil)
         (emaci-history nil)
         (emaci--buffer-job-alist nil)
         (emaci--build-counter nil)
         (emaci-save-dir (make-temp-file "emaci" t)))
     ,@body
     (delete-directory emaci-save-dir t)))

(defmacro with-advice (args &rest body)
  "Replace a function (car of ARGS) with function (cdr of ARGS) while executing BODY."
  (declare (indent 1))
  (let ((fun-name (car args))
        (advice   (cadr args))
        (orig-sym (make-symbol "orig")))
    `(cl-letf* ((,orig-sym  (symbol-function ',fun-name))
                ((symbol-function ',fun-name)
                 (lambda (&rest args)
                   (apply ,advice ,orig-sym args))))
       ,@body)))

(defun dummy-callback ()
  "Dummy callback so we make sure to wait for async methods."
  t)

(defmacro ert-deftest-async (name callbacks &rest body)
  "Like `ert-deftest' but with support for async.

NAME is the name of the test, which is the first argument to
`ert-deftest'.

CALLBACKS is a list of callback functions that all must be called
before `ert-async-timeout'.  If all callback functions have not
been called before the timeout, the test fails.

The callback functions should be called without any argument.  If
a callback function is called with a string as argument, the test
will fail with that error string.

BODY is the actual test."
  (unless callbacks
    (setq callbacks (list 'dummy-callback)))
  (declare (indent 2))
  (let ((cbs
         (mapcar
          (lambda (callback)
             `(lambda (&rest args)
                (,callback)
                (if (member ',callback callbacked)
                    (unless (eq 'dummy-callback ',callback)
                      (ert-fail (format "Callback %s called multiple times" ',callback)))
                  (push ',callback callbacked))))
          callbacks)))
    `(ert-deftest ,name ()
       (let ((callbacks ',cbs)
             (callbacked nil)
             (emaci-queue nil)
             (emaci-history nil)
             (emaci--buffer-job-alist nil)
             (emaci--build-counter nil)
             (emaci-save-dir (make-temp-file "emaci" t))
             (compilation-finish-functions (list 'emaci//compilation-finished)))
         (setq compilation-finish-functions (append compilation-finish-functions callbacks))
         (with-timeout
             (ert-async-timeout
              (ert-fail (format "Timeout of %ds exceeded. Expected the functions [%s] to be called, but was [%s]."
                                ert-async-timeout
                                ,(mapconcat 'symbol-name callbacks " ")
                                (mapconcat 'symbol-name callbacked " "))))
           ,@body
           (while (not (equal (sort (mapcar 'symbol-name callbacked) 'string<)
                              (sort (mapcar 'symbol-name ',callbacks) 'string<)))
             (accept-process-output nil 0.05))
           (delete-directory emaci-save-dir t))))))

(ert-deftest get-buildno-default ()
  "Test getting buildno for default queue."
  (with-sandbox
  (let ((result (emaci//get-buildno)))
    (should (equal result 1)))))

(ert-deftest get-buildno-increase-default ()
  "Test getting another buildno for default queue."
  (with-sandbox
   (let ((emaci--build-counter '(("*default*" . 1))))
    (emaci//get-buildno)
    (should (equal (cdr (assoc "*default*" emaci--build-counter)) 2)))))

(ert-deftest get-buildno-queue ()
  "Test getting buildno for a new queue."
  (with-sandbox
   (let ((result (emaci//get-buildno "testqueue")))
     (should (equal result 1)))))

(ert-deftest build-counter-new-queue ()
  "Test getting a buildno for a new queue."
  (with-sandbox
   (let ((emaci--build-counter '(("*default*" . 1))))
     (emaci//get-buildno "testqueue")
     (should (equal emaci--build-counter '(("testqueue" . 1) ("*default*" . 1)))))))

(ert-deftest build-counter-increase-queue ()
  "Test getting a buildno for an existing queue."
  (with-sandbox
   (let ((emaci--build-counter '(("*default*" . 1) ("testqueue" . 1))))
     (should (equal (emaci//get-buildno "testqueue") 2))
     (should (equal emaci--build-counter '(("*default*" . 1) ("testqueue" . 2)))))))

(defun test-job ()
  "Create a test job."
  (emaci//new-job "testqueue" "~" "echo Tis but a scratch" 'comint-mode "$.*^"))

(defun default-test-job ()
  "Create a test job."
  (emaci//new-job nil "~" "echo Tis but a scratch" 'comint-mode "$.*^"))


(defun assert-job
    (job queue buildno status statusmsg code buffer dir command mode highlight-regexp)
  "Assert job has right attributes."
  (should (equal (emaci-job-queue job) queue))
  (should (equal (emaci-job-buildno job) buildno))
  (should (equal (emaci-job-status job) status))
  (should (equal (emaci-job-statusmsg job) statusmsg))
  (should (equal (emaci-job-exitcode job) code))
  (should (equal (emaci-job-buffer job) buffer))
  (should (equal (emaci-job-dir job) dir))
  (should (equal (emaci-job-command job) command))
  (should (equal (emaci-job-mode job) mode))
  (should (equal (emaci-job-highlight-regexp job) highlight-regexp)))

(ert-deftest new-job-buildno-first ()
  "Test if first job has right build-no."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-buildno job) 1)))))

(ert-deftest new-job-buildno-second ()
  "Test if first job has right build-no."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (should (equal (emaci-job-buildno job2) 2)))))

(ert-deftest new-job-build-counter ()
  "Test if counter is increased after job creation."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal emaci--build-counter '(("testqueue" . 1)))))))

(ert-deftest new-job-status ()
  "Test initial job status."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-status job) 'queued)))))

(ert-deftest new-job-statusmsg ()
  "Test initial job statusmsg."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-statusmsg job) nil)))))

(ert-deftest new-job-buffer ()
  "Test initial job buffer."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-buffer job) nil)))))

(ert-deftest new-job-dir ()
  "Test job dir attribute."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-dir job) "~")))))

(ert-deftest new-job-command ()
  "Test job command attribute."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-command job)
                    "echo Tis but a scratch")))))

(ert-deftest new-job-mode ()
  "Test job mode attribute."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-mode job) 'comint-mode)))))

(ert-deftest new-job-highlight-regexp ()
  "Test job highlight regexp attribute."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal (emaci-job-highlight-regexp job) "$.*^")))))

(ert-deftest new-job-queue ()
  "Test if queue stays empty when creating a job."
  (with-sandbox
   (let ((job (test-job)))
     (should (equal emaci-queue nil)))))

(ert-deftest queue-job-one ()
  "Test if a job gets queued."
  (with-sandbox
   (let ((job (test-job)))
     (emaci//queue-job job)
     (should (equal emaci-queue (list (cons "testqueue" (list job))))))))

(ert-deftest queue-job-two ()
  "Test if job gets appended to queue."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job job2))))))))

(ert-deftest queue-job-one-default ()
  "Test if a job gets queued to default queue."
  (with-sandbox
   (let ((job (default-test-job)))
     (emaci//queue-job job)
     (should (equal emaci-queue (list (cons "*default*" (list job))))))))

(ert-deftest queue-job-two-default ()
  "Test if job gets appended to default queue."
  (with-sandbox
   (let ((job (default-test-job))
         (job2 (default-test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (equal emaci-queue (list (cons "*default*" (list job job2))))))))

(ert-deftest queue-move-to-history ()
  "Test moving job to history."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci//move-job-to-history job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2))))))))

(ert-deftest queue-move-to-history-default ()
  "Test moving job to default history."
  (with-sandbox
   (let ((job (default-test-job))
         (job2 (default-test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci//move-job-to-history job2)
     (should (equal emaci-queue (list (cons "*default*" (list job)))))
     (should (equal emaci-history (list (cons "*default*" (list job2))))))))

(ert-deftest running-job-p-empty ()
  "Test there is no running job in an empty queue."
  (with-sandbox
     (should-not (emaci//running-job-p))))

(ert-deftest running-job-p-not-running ()
  "Test if queued job is not running."
  (with-sandbox
   (let ((job (test-job)))
     (emaci//queue-job job)
     (should-not (emaci//running-job-p "testqueue")))))

(ert-deftest running-job-p-running ()
  "Test if there is a running job in queue."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (setf (emaci-job-status job) 'running)
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (emaci//running-job-p "testqueue")))))

(ert-deftest running-job-p-running-default ()
  "Test if there is a running job in default queue."
  (with-sandbox
   (let ((job (default-test-job))
         (job2 (default-test-job)))
     (setf (emaci-job-status job) 'running)
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (emaci//running-job-p)))))

(defun assert-history-one-default ()
  (assert-job
   (cadr (assoc "*default*" emaci-history))
   "*default*" 1 'finished "finished\n" nil "*default*: Build #1" "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-history-default (assert-history-one-default)
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'"))

(defun assert-history-one-queue ()
  (assert-job
   (cadr (assoc "testqueue" emaci-history))
   "testqueue" 1 'finished "finished\n" nil "testqueue: Build #1" "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-history-queue (assert-history-one-queue)
 (emaci//schedule "testqueue" "~" "echo 'Come on, you pansy!'"))

(defun assert-queue-empty-default ()
  (should-not (cdr (assoc "*default*" emaci-queue))))

(ert-deftest-async
 schedule-queue-default (assert-queue-empty-default)
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'"))

(defun assert-queue-empty-queue ()
  (should-not (cdr (assoc "testqueue" emaci-queue))))

(ert-deftest-async
 schedule-queue-queue (assert-queue-empty-queue)
 (emaci//schedule "testqueue" "~" "echo 'Come on, you pansy!'"))

(ert-deftest-async
 schedule-running-default ()
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'")
 (assert-job
  (cadr (assoc "*default*" emaci-queue))
  "*default*" 1 'running nil nil "*default*: Build #1" "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-running ()
 (emaci//schedule "testqueue" "~" "echo 'Come on, you pansy!'")
 (assert-job
  (cadr (assoc "testqueue" emaci-queue))
  "testqueue" 1 'running nil nil "testqueue: Build #1" "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-two-first-default ()
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'")
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'")
 (assert-job
  (cadr (assoc "*default*" emaci-queue))
  "*default*" 1 'running nil nil "*default*: Build #1" "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-two-second-default ()
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'")
 (emaci//schedule nil "~" "echo 'Come on, you pansy!'")
 (assert-job
  (nth 1 (cdr (assoc "*default*" emaci-queue)))
  "*default*" 2 'queued nil nil nil "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest schedule-deferred ()
  "Test queuing with DEFERRED arg."
  (with-sandbox
   (emaci//schedule "testqueue" "~" "echo 'Come on, you pansy!'" nil nil t)
   (assert-job
    (cadr (assoc "testqueue" emaci-queue))
    "testqueue" 1 'queued nil nil nil "~" "echo 'Come on, you pansy!'" nil nil)))

(ert-deftest cancel-queued ()
  "Test canceling a queued job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci/cancel-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest cancel-running ()
  "Test canceling a running job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (setf (emaci-job-buffer job2) (buffer-name (emaci//create-buffer job2)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'running)
     (emaci/cancel-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest cancel-canceled ()
  "Test canceling a canceled job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'canceled)
     (emaci//move-job-to-history job2)
     (emaci/cancel-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest kill-queued ()
  "Test killing a queued job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci/kill-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest kill-running ()
  "Test killing a running job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (setf (emaci-job-buffer job2) (buffer-name (emaci//create-buffer job2)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'running)
     (emaci/kill-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest kill-canceled ()
  "Test killing a canceled job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'canceled)
     (emaci//move-job-to-history job2)
     (emaci/kill-job job2)
     (should (equal emaci-queue (list (cons "testqueue" (list job)))))
     (should (equal emaci-history (list (cons "testqueue" (list job2)))))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest compilation-finished-no-queue ()
  "Test compilation finished callback with empty queue."
  (with-sandbox
   (emaci//compilation-finished "some buffer" "test")))

(defmacro with-double-running-queue (&rest body)
  "Execute body with a queue with two running jobs."
  `(with-sandbox
    (get-buffer-create "some buffer")
    (get-buffer-create "test buffer")
    (let* ((emaci-queue
            (list
             (cons
              "testqueue"
              (list
               (make-emaci-job
                   :buildno 1 :status 'running :statusmsg nil
                   :buffer "test buffer" :dir "~"
                   :command "echo test1" :mode nil :highlight-regexp nil)
                  (make-emaci-job
                   :buildno 2 :status 'running :statusmsg nil
                   :buffer "some buffer" :dir "~"
                   :command "echo test2" :mode nil :highlight-regexp nil)))))
           (emaci--buffer-job-alist
            (list (cons (get-buffer-create "some buffer") (cadr (assoc "testqueue" emaci-queue))))))
      ,@body)))

(ert-deftest compilation-finished-cb ()
  "Test compilation finished callback with empty queue."
  (with-double-running-queue
   (let ((job-finished-called nil))
     (with-advice
      (emaci//job-finished
       (lambda (orig-fun job status statusmsg)
         (setq job-finished-called t)
         (should (eq job (cadr (assoc "testqueue" emaci-queue))))
         (should (eq status 'finished))
         (should (equal statusmsg "test"))))
      (emaci//compilation-finished (get-buffer-create "some buffer") "test"))
     (should job-finished-called))))

(ert-deftest job-finished ()
  "Test job finished"
  (with-sandbox
   (let* ((emaci-history (list (cons "testqueue" (list "dummy"))))
          (job (test-job))
          (emaci-queue (list (cons "testqueue" (list job)))))
     (emaci//job-finished job 'finished "finished\n")
     (should (eq (emaci-job-status job) 'finished))
     (should (equal (emaci-job-statusmsg job) "finished\n"))
     (should (equal emaci-history (list (cons "testqueue" (list "dummy" job)))))
     (should (equal emaci-queue (list (cons "testqueue" nil)))))))

(ert-deftest job-finished-default ()
  "Test job finished with default job"
  (with-sandbox
   (let* ((emaci-history (list (cons "*default*" (list "dummy"))))
          (job (default-test-job))
          (emaci-queue (list (cons "*default*" (list job)))))
     (emaci//job-finished job 'finished "finished\n")
     (should (eq (emaci-job-status job) 'finished))
     (should (equal (emaci-job-statusmsg job) "finished\n"))
     (should (equal emaci-history (list (cons "*default*" (list "dummy" job)))))
     (should (equal emaci-queue (list (cons "*default*" nil)))))))

(ert-deftest execute-next-empty-queue ()
  "Test execute-next with empty queue."
  (with-sandbox
   (emaci/execute-next "testqueue")))

(ert-deftest execute-next-empty-default-queue ()
  "Test execute-next with empty default queue."
  (with-sandbox
   (emaci/execute-next)))

(ert-deftest execute-next-running ()
  "Test execute-next with running job."
  (with-sandbox
   (let ((job (test-job)))
     (setf (emaci-job-status job) 'running)
     (setq emaci-queue (list (cons "testqueue" (list job))))
     (should (eq (cdr (should-error (emaci/execute-next "testqueue")
                                    :type 'emaci-error-job-running))
                 job)))))

(ert-deftest execute-next-running-default ()
  "Test execute-next with running job and default queue."
  (with-sandbox
   (let ((job (default-test-job)))
     (setf (emaci-job-status job) 'running)
     (setq emaci-queue (list (cons "*default*" (list job))))
     (should (eq (cdr (should-error (emaci/execute-next)
                                    :type 'emaci-error-job-running))
                 job)))))

(ert-deftest execute-next-queued ()
  "Test execute-next with queued job."
  (with-sandbox
   (let (emaci//execute-called-p)
     (with-advice
      (emaci//execute
       (lambda (orig-fun job) (assert-job job "testqueue" 1 'queued nil nil nil "~" "echo Tis but a scratch" 'comint-mode "$.*^")
         (setq emaci//execute-called-p t)))
      (let ((job (test-job)))
        (setq emaci-queue (list (cons "testqueue" (list job))))
        (emaci/execute-next "testqueue")
        (should emaci//execute-called-p))))))

(ert-deftest create-buffer-name ()
  "Test creating buffer names"
  (with-sandbox
   (should (equal (emaci//create-buffer-name (test-job)) "testqueue: Build #1"))))

(ert-deftest create-buffer ()
  "Test creating buffer names"
  (with-sandbox
   (let ((job (test-job)))
    (should (equal (emaci//create-buffer job) (get-buffer "testqueue: Build #1")))
    (should (get-buffer "testqueue: Build #1"))
    (should
     (equal
      emaci--buffer-job-alist
      (list (cons (get-buffer "testqueue: Build #1") job)))))))

(defun assert-execute-job ()
  (assert-job
   (cadr (assoc "testqueue" emaci-history)) "testqueue" 1 'finished "finished\n" nil "testqueue: Build #1"
   "~" "echo Tis but a scratch" 'comint-mode "$.*^"))

(ert-deftest-async
 execute (assert-execute-job)
 (let ((job (test-job)))
   (setq emaci-queue (list (cons "testqueue" (list job))))
   (emaci//execute job)
   (assert-job
    (cadr (assoc "testqueue" emaci-queue)) "testqueue" 1 'running nil nil "testqueue: Build #1"
    "~" "echo Tis but a scratch" 'comint-mode "$.*^")))

(defun assert-log ()
  (let* ((job (cadr (assoc "testqueue" emaci-history)))
         (buffer (get-buffer (emaci-job-buffer job)))
         (output (with-current-buffer buffer (buffer-string)))
         (fullpath (emaci//get-log-filepath job)))
    (should (equal output
                   (with-temp-buffer
                     (insert-file-contents (emaci//get-log-filepath job))
                     (buffer-string))))))

(ert-deftest-async
 log (assert-log)
 (let ((job (test-job)))
   (setq emaci-queue (list (cons "testqueue" (list job))))
   (emaci//execute job)))

(ert-deftest peristent ()
  "Test persistent variables."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci//move-job-to-history job2)
     (emaci//save-vars)
     (let ((old-history emaci-history)
           (old-queue emaci-queue)
           (old-build-counter emaci--build-counter))
       (setq emaci-history nil
             emaci-queue nil
             emaci--build-counter nil)
       (emaci/load-vars)
       (should (equal (type-of emaci-queue) (type-of old-queue)))
       (should (equal emaci-queue old-queue))
       (should (equal emaci-history old-history))
       (should (equal emaci--build-counter old-build-counter))))))

(ert-deftest load-empty ()
  "Test loading non-existent variables."
  (with-sandbox
   (let ((emaci-save-dir "/NONEXISTENTDIRECTORY/")
         (emaci-queue t))
     (emaci/load-vars)
     (should emaci-queue))))

;;; test-emaci.el ends here
