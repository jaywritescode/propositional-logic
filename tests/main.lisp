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

(deftest test-transform-to-cnf
  (let ((sentence '(b1-1 :iff (p1-2 :or p2-1)))
        (expected '((((:not b1-1) :or (p1-2 :or p2-1)))
                    :and
                    ((((:not p1-2) :or b1-1)
                      :and ((:not p2-1) :or b1-1))))))
    (testing "transform sentence to conjunctive normal form"
      (ok
       (equal expected (propositional-logic::transform-to-cnf sentence))))))
