(in-package :cl-user)
(defpackage propositional-logic/tests/main
  (:use :cl
        :rove)
  (:import-from :propositional-logic :tt-entails?))
(in-package :propositional-logic/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :propositional-logic)' in your Lisp.

(deftest test-tt-entails?
  (let ((kb '((:not p1-1)
              (b1-1 :iff (p1-2 :or p2-1))
              (b2-1 :iff ((p1-1 :or p2-2) :or p3-1))
              (:not b1-1)
              (b2-1))))
    (testing "knowledge base entails the given sentence"
      (ok (tt-entails? kb '(p2-2))))))

(deftest eliminate-biconditionals
  (let ((sentence '(b1-1 :iff (p1-2 :or p2-1))))
    (testing "biconditional elimination"
      (ok (equal (propositional-logic::eliminate-biconditionals sentence)
                 '((b1-1 :implies (p1-2 :or p2-1)) :and ((p1-2 :or p2-1) :implies b1-1)))))))

(deftest eliminate-implications
  (let ((sentence '((b1-1 :implies (p1-2 :or p2-1)) :and ((p1-2 :or p2-1) :implies b1-1))))
    (testing "implication elimination"
      (ok (equal (propositional-logic::eliminate-implications sentence)
                 '(((:not b1-1) :or (p1-2 :or p2-1)) :and ((:not (p1-2 :or p2-1)) :or b1-1)))))))
