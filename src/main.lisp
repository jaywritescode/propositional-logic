(defpackage propositional-logic
  (:use :cl :trivia)
  (:export :tt-entails?))
(in-package :propositional-logic)

(defun tt-entails? (kb alpha)
  (let ((symbols (capture-symbols-in kb alpha)))
    (tt-check-all kb alpha symbols nil)))

(defun model-lookup (symbol model)
  (cdr (assoc symbol model)))

(defun pl-true? (sentence model)
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
  (if (null symbols)
      (if (pl-true? kb model)
          (pl-true? alpha model)
          t)
      (let ((p (first symbols))
            (rest (rest symbols)))
        (and (tt-check-all kb alpha rest (extend p t model))
             (tt-check-all kb alpha rest (extend p nil model))))))

(defun capture-symbols-in (&rest sentences)
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
