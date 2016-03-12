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
(require 'ert-async)
(require 'emaci)

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

(defun async-cb (cb)
  `(lambda (&rest args) (funcall ,cb)))

(defmacro ert-deftest/async (name hooks &rest body)
  `(ert-deftest-async
   ,name ,hooks
   (with-sandbox-async ',hooks ,@body)))

(defmacro with-sandbox-async (hooks &rest body)
  "Evaluate BODY with all variables let bound."
  `(let ((emaci-queue nil)
         (emaci-history nil)
         (emaci--buffer-job-alist nil)
         (emaci--build-counter 0)
         (compilation-finish-functions (list 'emaci//compilation-finished)))
     (setq compilation-finish-functions
           (append
            compilation-finish-functions
            (mapcar 'async-cb ,hooks)))
     ,@body (sit-for 1)))

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

(defun assert-queue-empty ()
  (should-not emaci-queue))

(defun assert-history-one ()
  (assert-job
   (car emaci-history)
   1 'finished (get-buffer "Build #1") "~" "echo 'Come on, you pansy'" t nil))

(ert-deftest/async
 schedule-history (assert-history-one)
 (emaci//schedule "~" "echo 'Come on, you pansy!'"))

(ert-deftest/async
 schedulte-queue (assert-queue-empty)
 (emaci//schedule "~" "echo 'Come on, you pansy!'"))

;;; test-emaci.el ends here
