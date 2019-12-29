(in-package :cl-user)
(defpackage propositional-logic/tests/main
  (:use :cl
        :fiveam)
  (:export #:run! #:all-tests))
(in-package :propositional-logic/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :propositional-logic)' in your Lisp.

(def-suite all-tests
  :description "The master suite of all propositional-logic tests.")

(in-suite all-tests)

(test tt-entails?
  :description "Test the `tt-entails?` function."
  (let ((kb '((:not a)
              (b :iff (c :or d))
              (d :iff ((a :or e) :or f))
              (:not b)
              (d))))
    (is-true (propositional-logic::tt-entails? kb '(e)))))

(test transform-to-cnf
  :description "Test transforming a sentence into conjunctive normal form."
  (let ((sentence '(a :iff (b :or c)))
        (expected '(((:not a) :or (b :or c))
                    :and
                    (((:not b) :or a)
                     :and ((:not c) :or a)))))
    (is-true (equal expected (propositional-logic::transform-to-cnf sentence)))))

(test condensed-cnf
  :description "Test transforming a tree of clauses into a list of conjunctions of disjunctions"
  (let ((sentence '(((:not a) :or (b :or c))
                    :and
                    (((:not b) :or a)
                     :and ((:not c) :or a))))
        (expected '(((:not a) b c) ((:not b) a) ((:not c) a))))
    (is-true (equal expected (propositional-logic::condensed-cnf sentence)))))
