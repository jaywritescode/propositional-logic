(defpackage propositional-logic
  (:use :cl :trivia)
  (:export :tt-entails?))
(in-package :propositional-logic)

(defun tt-entails? (kb alpha)
  "Returns t if the knowledge base kb entails the sentence alpha."
  (let ((symbols (capture-symbols-in kb alpha)))
    (tt-check-all kb alpha symbols nil)))

(defun model-lookup (symbol model)
  (cdr (assoc symbol model)))

(defun pl-true? (sentence model)
  "Returns t if the given sentence is true in the given model."
  (match sentence
    ((type symbol)
     (model-lookup sentence model))
    ((list :not p)
     (not (pl-true? p model)))
    ((list p :and q)
     (and (pl-true? p model) (pl-true? q model)))
    ((list p :or q)
     (or (pl-true? p model) (pl-true? q model)))
    ((list p :implies q)
     (or (not (pl-true? p model)) (pl-true? q model)))
    ((list p :iff q)
     (eq (pl-true? p model) (pl-true? q model)))))

(defun extend (symbol value model)
  (push (cons symbol value) model))

(defun tt-check-all (kb alpha symbols model)
  "Returns t if alpha is satisfiable, recursively iterates through all possible assignments to the model."
  (if (null symbols)
      (if (pl-true? kb model)
          (pl-true? alpha model)
          t)
      (let ((p (first symbols))
            (rest (rest symbols)))
        (and (tt-check-all kb alpha rest (extend p t model))
             (tt-check-all kb alpha rest (extend p nil model))))))

(defun capture-symbols-in (&rest sentences)
  "Returns a list of all symbols in the given collection of sentences."
  (delete-duplicates
   (reduce #'(lambda (accumulator value)
               (cond ((null value) nil)
                     ((member value '(:not :and :or :implies :iff)) nil)
                     ((atom value) (nconc accumulator (list value)))
                     (t (nconc accumulator
                               (capture-symbols-in (first value))
                               (capture-symbols-in (rest value))))))
           sentences :initial-value nil)))

;; (defun convert-to-cnf (sentence)
;;   (let* ((x (eliminate-biconditionals sentence))
;;          (y (eliminate-implication x))
;;          (z (apply-negation-to-literals y))
;;          (w (distribute-disjunction-over-conjunction z)))
;;     w))

(defun eliminate-biconditionals (sentence)
  (match sentence
    ((list p :iff q)
     `((,p :implies ,q) :and (,q :implies ,p)))
    ((list :not p)
     `(:not ,(eliminate-biconditionals p)))
    ((list p op q)
     `(,(eliminate-biconditionals p) ,op ,(eliminate-biconditionals q)))
    ((list (@ _) x) x)))

(defun eliminate-implications (sentence)
  (match sentence
    ((list p :implies q)
     `((:not ,p) :or ,q))
    ((list :not p)
     `(:not ,(eliminate-implications p)))
    ((list p op q)
     `(,(eliminate-implications p) ,op ,(eliminate-implications q)))
    ((list (@ _) x) x)))
