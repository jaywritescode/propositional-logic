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

(defun transform-to-cnf (sentence)
  "Transform a sentence into conjunctive normal form"
  (or
   (and (atom sentence) sentence)
   (match sentence
     ;; biconditional elimination
     ((list alpha :iff beta)
      (format t "~S -> biconditional elimination~%" sentence)
      `(,(transform-to-cnf `(,alpha :implies ,beta))
        :and
        ,(transform-to-cnf `(,beta :implies ,alpha))))
     ;; implication elimination
     ((list alpha :implies beta)
      (format t "~S -> implication elimination~%" sentence)
      `(,(transform-to-cnf `((:not ,alpha) :or ,beta))))
     ;; double-negation elimination
     ((list :not (list :not alpha))
      (format t "~S -> double-negation elmination~%" sentence)
      alpha)
     ;; De Morgan's laws
     ((list :not (list alpha :and beta))
      (format t "~S -> de morgan's law~%" sentence)
      `((:not ,alpha) :or (:not ,beta)))
     ((list :not (list alpha :or beta))
      (format t "~S -> de morgan's law~%" sentence)
      `((:not ,alpha) :and (:not ,beta)))
     ;; distributivity of disjunction over conjunction
     ((or (list alpha :or (list beta :and gamma))
          (list (list alpha :and beta) :or gamma))
      (format t "~S -> distribute disjunction over conjunction~%" sentence)
      `((,alpha :or ,beta) :and (,alpha :or ,gamma)))
     ((list alpha :or beta)
      (format t "~S~%" sentence)
      `(,(transform-to-cnf alpha) :or ,(transform-to-cnf beta)))
     ((list alpha :and beta)
      (format t "~S~%" sentence)
      `(,(transform-to-cnf alpha) :and ,(transform-to-cnf beta))))
   sentence))


(defun eliminate-biconditionals (sentence)
  (or
   (match sentence
     ((list p :iff q)
      `((,(eliminate-biconditionals p) :implies
         ,(eliminate-biconditionals q))
        :and
        (,(eliminate-biconditionals q) :implies
         ,(eliminate-biconditionals p))))
     ((list :not p)
      `(:not ,(eliminate-biconditionals p)))
     ((list p op q)
      `(,(eliminate-biconditionals p) ,op ,(eliminate-biconditionals q)))
     ((list (@ _) x) x))
   sentence))

(defun eliminate-implications (sentence)
  (or
   (match sentence
     ((list p :implies q)
      `((:not ,(eliminate-implications p)) :or ,(eliminate-implications q)))
     ;; i'm not sure if these are correct
     ;; shouldn't the calling function be responsible for the recursion?
     ((list :not p)
      `(:not ,(eliminate-implications p)))
     ((list p op q)
      `(,(eliminate-implications p) ,op ,(eliminate-implications q)))
     ((list (@ _) x) x))
   sentence))

(defun move-negation-inward (sentence)
  (or
   (match sentence
     ((list :not (list :not p)) (move-negation-inward p))
     ((list :not (list p :and q))
      `(,(move-negation-inward `(:not ,p)) :or ,(move-negation-inward `(:not ,q))))
     ((list :not (list p :or q))
      `(,(move-negation-inward `(:not ,p)) :and ,(move-negation-inward `(:not ,q))))
     ((list (@ _) x) x))
   sentence))
