;;; graph-test.el --- test graph drawing

;; Copyright (C) 2017 by David ZUBER

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
  (require 'edebug)
  (def-edebug-spec setf (&rest [place form]))
  (undercover "*.el"))

(require 'graph)

(ert-deftest half ()
  "Test dividing things by two."
  (should (equal (/ 3 2.0) (graph//half 3))))

(ert-deftest fill-default ()
  "Test fill."
  (should (equal "a" (graph//fill ?a)))
  (should (equal "aaa" (graph//fill ?a 3))))

(ert-deftest integer-shapes ()
  "Test rounding off shapes."
  (let ((shapes (list
                 (make-graph-shape :x 0 :y 0 :width 10 :height 20 :type "type")
                 (make-graph-shape :x 3.2 :y 1.6 :width 4.7 :height 9.5 :type "foo")))
        (expected (list
                   (make-graph-shape :x 0 :y 0 :width 10 :height 20 :type "type")
                   (make-graph-shape :x 3 :y 1 :width 4 :height 10 :type "foo"))))
    (should (equal expected (graph//integer-shapes shapes)))))

(ert-deftest rect-relation ()
  "Test calculating rectangle relation."
  (should (equal 'on (graph//rect-relation 3 3 10)))
  (should (equal 'on (graph//rect-relation 7 3 5)))
  (should (equal 'in (graph//rect-relation 4 3 5)))
  (should (equal nil (graph//rect-relation 1 3 5)))
  (should (equal nil (graph//rect-relation 10 3 5))))

(ert-deftest shapes-height ()
  "Test calculating the maximum shape height."
  (let ((shapes (list
                 (make-graph-shape :y 10 :height 3)
                 (make-graph-shape :y 1 :height 6))))
    (should (equal 13 (graph//shapes-height shapes)))))

(ert-deftest shapes-width ()
  "Test calculating the maximum shape width."
  (let ((shapes (list
                 (make-graph-shape :x 10 :width 3)
                 (make-graph-shape :x 1 :width 6))))
    (should (equal 13 (graph//shapes-width shapes)))))

(ert-deftest numbered ()
  "Test generating numbered sequences."
  (should (equal '((0 . "a") (1 . "b") (2 . "c") (3 . "d"))
                 (graph//numbered '("a" "b" "c" "d")))))

(ert-deftest iter-height ()
  "Test iterating over ypos."
  (should (equal '(0 1 2 3 4 5)
                 (graph//iter-height (list (make-graph-shape :y 2 :height 3))))))

(ert-deftest filter-shapes-at-ypos ()
  "Test filtering shapes that don't cover ypos."
  (let* ((shapes (list
                  (make-graph-shape :y 3 :height 4)
                  (make-graph-shape :y 1 :height 5)
                  (make-graph-shape :y 1 :height 100)))
         (expected (list (car shapes) (caddr shapes))))
    (should (equal expected (graph//filter-shapes-at-ypos 6 shapes)))))

(ert-deftest x-sort-shapes ()
  "Test sorting shapes."
  (let ((assert (lambda (expected input)
                  (should (equal expected (graph//x-sort-shapes input)))
                  (should (equal expected (graph//x-sort-shapes (reverse input))))
                  (should (equal expected (graph//x-sort-shapes expected))))))
    (let ((a (make-graph-shape :x 3))
          (b (make-graph-shape :x 1)))
      (assert (list b a) (list a b)))
    (let ((a (make-graph-shape :x 1 :width 3))
          (b (make-graph-shape :x 1 :width 1)))
      (assert (list b a) (list a b)))
    (let ((a (make-graph-shape :x 1 :width 1 :height 1))
          (b (make-graph-shape :x 1 :width 1 :height 10)))
      (assert (list b a) (list a b)))
    (let ((a (make-graph-shape :x 1 :width 1 :height 10))
          (b (make-graph-shape :x 1 :width 1 :height 1)))
      (assert (list a b) (list a b)))
    (let ((a (make-graph-shape :x 1 :width 1 :height 1))
          (b (make-graph-shape :x 1 :width 1 :height 1)))
      (assert (list a b) (list a b)))))

(ert-deftest draw-shapes-frame ()
  "Test the frame around the main loop in `grap//draw-shapes'."
  (cl-letf (((symbol-function 'graph//draw-shapes-pos)
             (lambda (ypos xcur shapes node-padding)
               (list (cons 'xcur (+ xcur 1))
                     (cons 'drawn (number-to-string xcur))
                     (cons 'shapes (cdr shapes))))))
    (let ((shapes (list
                   (make-graph-shape :x 2 :y 3 :height 4 :width 4)
                   (make-graph-shape :x 4 :y 1 :height 4 :width 3)))
          (expected "\n0\n0\n01\n01\n0\n0\n\n"))
      (should (equal expected (graph//draw-shapes shapes 1))))))

(ert-deftest new-xcur ()
  "Test moving the xcursor"
  (should (equal 8 (graph//new-xcur 5 2 "123456")))
  (should (equal 8 (graph//new-xcur 8 2 "12")))
  (should (equal 8 (graph//new-xcur 8 2 "123456"))))

(ert-deftest crop-already-drawn ()
  "Test cropping a string so it doesn't overlap with what we've already drawn."
  (should (equal "3456" (graph//crop-already-drawn 5 3 "123456")))
  (should (equal "123456" (graph//crop-already-drawn 2 5 "123456"))))

(ert-deftest get-next-shapes-to-draw ()
  "Test getting the next shapes."
  (should (equal '(1 0 2 3) (graph//get-next-shapes-to-draw t 0 '(1 2 3))))
  (should (equal '(1 2 3) (graph//get-next-shapes-to-draw nil 0 '(1 2 3)))))

;; graph-test.el ends here
