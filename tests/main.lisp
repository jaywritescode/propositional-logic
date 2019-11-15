(in-package :cl-user)
(defpackage propositional-logic/tests/main
  (:use :cl
        :rove)
  (:import-from :propositional-logic :pl-true?))
(in-package :propositional-logic/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :propositional-logic)' in your Lisp.

(deftest test-pl-true?
  (let ((model '((p . t) (q . nil) (r . t) (s . nil))))
    (testing "a symbol resolves to its value in the model"
      (ok (pl-true? 'p model))
      (ng (pl-true? 'q model)))
    (testing "not p is the negation of p"
      (ok (pl-true? '(:not q) model))
      (ng (pl-true? '(:not p) model)))
    (testing "p and q is the conjunction of p and q"
      (ok (pl-true? '(p :and r) model))
      (ng (pl-true? '(p :and q) model)))
    (testing "p or q is the disjunction of p and q"
      (ok (pl-true? '(p :or q) model))
      (ng (pl-true? '(q :or s) model)))
    (testing "p implies q is the implication of p concludes q"
      (ok (pl-true? '(q :implies r) model))
      (ok (pl-true? '(p :implies r) model))
      (ng (pl-true? '(p :implies q) model)))
    (testing "p iff q is the biconditional of p and q"
      (ok (pl-true? '(q :iff s) model))
      (ng (pl-true? '(p :iff s) model)))))

(deftest test-tt-entails?
  (let ((kb '((:not p1-1)
              (b1-1 :iff (p1-2 :or p2-1))
              (b2-1 :iff ((p1-1 :or p2-2) :or p3-1))
              (:not b1-1)
              (b2-1))))
    (testing "knowledge base entails the given sentence"
      (ok (tt-entails? kb '(p2-2))))))
