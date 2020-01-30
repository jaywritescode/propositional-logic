(defpackage propositional-logic/tests
  (:use :cl
        :propositional-logic
        :fiveam))
(in-package :propositional-logic/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :propositional-logic)' in your Lisp.

(def-suite all-tests
  :description "The master suite of all propositional-logic tests.")

(in-suite all-tests)

(defun test-quasi ()
  (run! 'all-tests))

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
