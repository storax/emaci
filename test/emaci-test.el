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

(ert-deftest comp-finished-hook ()
    "Test if hook is installed"
    (should (member 'emaci//compilation-finished compilation-finish-functions)))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY with all variables let bound."
  `(let ((emaci-queue nil)
        (emaci-history nil)
        (emaci--buffer-job-alist nil)
        (emaci--build-counter 0))
     ,@body))

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
             (emaci--build-counter 0)
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
             (accept-process-output nil 0.05)))))))

(ert-deftest get-buildno ()
  "Test getting buildno."
  (with-sandbox
  (let ((result (emaci//get-buildno)))
    (should (equal result 1)))))

(ert-deftest get-buildno-increase ()
  "Test getting buildno."
  (with-sandbox
   (let ((emaci--build-counter 1))
    (emaci//get-buildno)
    (should (equal emaci--build-counter 2)))))

(defun test-job ()
  "Create a test job."
  (emaci//new-job "~" "echo Tis but a scratch" 'comint-mode "$.*^"))

(defun assert-job
    (job buildno status statusmsg buffer dir command mode highlight-regexp)
  "Assert job has right attributes."
  (should (equal (emaci-job-buildno job) buildno))
  (should (equal (emaci-job-status job) status))
  (should (equal (emaci-job-statusmsg job) statusmsg))
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
     (should (equal emaci--build-counter 1)))))

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
     (should (equal emaci-queue (list job))))))

(ert-deftest queue-job-two ()
  "Test if job gets appended to queue."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (equal emaci-queue (list job job2))))))

(ert-deftest queue-move-to-history ()
  "Test moving job to history."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci//move-job-to-history job2)
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2))))))

(ert-deftest running-job-p-empty ()
  "Test there is no running job in an empty queue."
  (with-sandbox
     (should-not (emaci//running-job-p))))

(ert-deftest running-job-p-not-running ()
  "Test if queued job is not running."
  (with-sandbox
   (let ((job (test-job)))
     (emaci//queue-job job)
     (should-not (emaci//running-job-p)))))

(ert-deftest running-job-p-running ()
  "Test if there is a running job in queue."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (setf (emaci-job-status job) 'running)
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (should (emaci//running-job-p)))))

(defun assert-history-one ()
  (assert-job
   (car emaci-history)
   1 'finished "finished\n" (get-buffer "Build #1") "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-history (assert-history-one)
 (emaci//schedule "~" "echo 'Come on, you pansy!'"))

(defun assert-queue-empty ()
  (should-not emaci-queue))

(ert-deftest-async
 schedule-queue (assert-queue-empty)
 (emaci//schedule "~" "echo 'Come on, you pansy!'"))

(ert-deftest-async
 schedule-running ()
 (emaci//schedule "~" "echo 'Come on, you pansy!'")
 (assert-job
  (car emaci-queue)
  1 'running nil (get-buffer "Build #1") "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-two-first ()
 (emaci//schedule "~" "echo 'Come on, you pansy!'")
 (emaci//schedule "~" "echo 'Come on, you pansy!'")
 (assert-job
  (car emaci-queue)
  1 'running nil (get-buffer "Build #1") "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest-async
 schedule-two-second ()
 (emaci//schedule "~" "echo 'Come on, you pansy!'")
 (emaci//schedule "~" "echo 'Come on, you pansy!'")
 (assert-job
  (cadr emaci-queue)
  2 'queued nil nil "~" "echo 'Come on, you pansy!'" nil nil))

(ert-deftest schedule-deferred ()
  "Test queuing with DEFERRED arg."
  (with-sandbox
   (emaci//schedule "~" "echo 'Come on, you pansy!'" nil nil t)
   (assert-job
    (car emaci-queue)
    1 'queued nil nil "~" "echo 'Come on, you pansy!'" nil nil)))

(ert-deftest cancel-queued ()
  "Test canceling a queued job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci/cancel-job job2)
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest cancel-running ()
  "Test canceling a running job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'running)
     (emaci/cancel-job job2)
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
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
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest kill-queued ()
  "Test killing a queued job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (emaci/kill-job job2)
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest kill-running ()
  "Test killing a running job."
  (with-sandbox
   (let ((job (test-job))
         (job2 (test-job)))
     (emaci//queue-job job)
     (emaci//queue-job job2)
     (setf (emaci-job-status job2) 'running)
     (emaci/kill-job job2)
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
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
     (should (equal emaci-queue (list job)))
     (should (equal emaci-history (list job2)))
     (should (eq (emaci-job-status job2) 'canceled)))))

(ert-deftest compilation-finished-no-queue ()
  "Test compilation finished callback with empty queue."
  (emaci//compilation-finished "some buffer" "test"))

(defmacro with-double-running-queue (&rest body)
  "Execute body with a queue with two running jobs."
  `(with-sandbox
    (let* ((emaci-queue
            (list (make-emaci-job
                   :buildno 1 :status 'running :statusmsg nil
                   :buffer (get-buffer-create "test buffer") :dir "~"
                   :command "echo test1" :mode nil :highlight-regexp nil)
                  (make-emaci-job
                   :buildno 2 :status 'running :statusmsg nil
                   :buffer (get-buffer-create "some buffer") :dir "~"
                   :command "echo test2" :mode nil :highlight-regexp nil)))
           (emaci--buffer-job-alist
            (list (cons (get-buffer-create "some buffer") (cadr emaci-queue)))))
      ,@body)))

(ert-deftest compilation-finished-cb ()
  "Test compilation finished callback with empty queue."
  (with-double-running-queue
   (let ((job-finished-called nil))
     (with-advice
      (emaci//job-finished
       (lambda (orig-fun job status statusmsg)
         (setq job-finished-called t)
         (should (eq job (cadr emaci-queue)))
         (should (eq status 'finished))
         (should (equal statusmsg "test"))))
      (emaci//compilation-finished (get-buffer-create "some buffer") "test"))
     (should job-finished-called))))

(ert-deftest job-finished ()
  "Test job finished"
  (with-sandbox
   (let* ((emaci-history (list "dummy"))
          (job (test-job))
          (emaci-queue (list job)))
     (emaci//job-finished job 'finished "finished\n")
     (should (eq (emaci-job-status job) 'finished))
     (should (equal (emaci-job-statusmsg job) "finished\n"))
     (should (equal emaci-history (list "dummy" job)))
     (should-not emaci-queue))))

(ert-deftest execute-next-empty-queue ()
  "Test execute-next with empty."
  (with-sandbox
   (emaci/execute-next)))

(ert-deftest execute-next-running ()
  "Test execute-next with running job."
  (with-sandbox
   (let ((job (test-job)))
     (setf (emaci-job-status job) 'running)
     (add-to-list 'emaci-queue job)
     (should (eq (cdr (should-error (emaci/execute-next)
                                    :type 'emaci-error-job-running))
                 job)))))

(ert-deftest execute-next-queued ()
  "Test execute-next with queued job."
  (with-sandbox
   (let (emaci//execute-called-p)
     (with-advice
      (emaci//execute
       (lambda (orig-fun job) (assert-job job 1 'queued nil nil "~" "echo Tis but a scratch" 'comint-mode "$.*^")
         (setq emaci//execute-called-p t)))
      (let ((job (test-job)))
        (add-to-list 'emaci-queue job)
        (emaci/execute-next)
        (should emaci//execute-called-p))))))

(ert-deftest create-buffer-name ()
  "Test creating buffer names"
  (with-sandbox
   (should (equal (emaci//create-buffer-name (test-job)) "Build #1"))))

(ert-deftest create-buffer ()
  "Test creating buffer names"
  (with-sandbox
   (let ((job (test-job)))
    (should (equal (emaci//create-buffer job) (get-buffer "Build #1")))
    (should (get-buffer "Build #1"))
    (should
     (equal
      emaci--buffer-job-alist
      (list (cons (get-buffer "Build #1") job)))))))

(defun assert-execute-job ()
  (assert-job
   (car emaci-history) 1 'finished "finished\n" (get-buffer "Build #1")
   "~" "echo Tis but a scratch" 'comint-mode "$.*^"))

(ert-deftest-async
 execute (assert-execute-job)
 (let* ((job (test-job))
        (emaci-queue (list job)))
   (emaci//execute job)
   (assert-job
    (car emaci-queue) 1 'running nil (get-buffer "Build #1")
    "~" "echo Tis but a scratch" 'comint-mode "$.*^")))

;;; test-emaci.el ends here
