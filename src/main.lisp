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
      (transform-to-cnf `((,alpha :implies ,beta) :and (,beta :implies ,alpha))))
     ;; implication elimination
     ((list alpha :implies beta)
      (format t "~S -> implication elimination~%" sentence)
      (transform-to-cnf `((:not ,alpha) :or ,beta)))
     ;; double-negation elimination
     ((list :not (list :not alpha))
      (format t "~S -> double-negation elmination~%" sentence)
      (transform-to-cnf alpha))
     ;; De Morgan's laws
     ((list :not (list alpha :and beta))
      (format t "~S -> de morgan's law (not-and)~%" sentence)
      `((:not ,(transform-to-cnf alpha)) :or (:not ,(transform-to-cnf beta))))
     ((list :not (list alpha :or beta))
      (format t "~S -> de morgan's law (not-or)~%" sentence)
      (transform-to-cnf `((:not ,alpha) :and (:not ,beta))))
     ;; distributivity of disjunction over conjunction
     ((list alpha :or (list beta :and gamma))
      (format t "~S -> distribute disjunction over conjunction (or first)~%" sentence)
      `(,(transform-to-cnf `(,alpha :or ,beta))
        :and
        ,(transform-to-cnf `(,alpha :or ,gamma))))
     ((list (list alpha :and beta) :or gamma)
      (format t "~S -> distribute disjunction over conjunction (and first)~%" sentence)
      `(,(transform-to-cnf `(,alpha :or ,gamma))
        :and
        ,(transform-to-cnf `(,beta :or ,gamma))))
     ((list alpha :or beta)
      (format t "~S -> clauses joined by OR~%" sentence)
      ;; the sub-clauses can only have :or in them
      (let* ((lhs (transform-to-cnf alpha))
             (rhs (transform-to-cnf beta))
             (new-sentence `(,lhs :or ,rhs)))
        (match lhs
          ((list _ :and _)
           (setf new-sentence (transform-to-cnf new-sentence))))
        (match rhs
          ((list _ :and _)
           (setf new-sentence (transform-to-cnf new-sentence))))
        new-sentence))
     ((list alpha :and beta)
      (format t "~S -> clauses joined by AND~%" sentence)
      `(,(transform-to-cnf alpha) :and ,(transform-to-cnf beta))))
   sentence))

(defun condensed-cnf (sentence)
  (or
   (and (atom sentence) (list sentence))
   (match sentence
     ((list :not _) (list sentence))
     ((list p :and q)
      (nconc (or (match p ((list _ :and _)
                           (condensed-cnf p)))
                 (list (condensed-cnf p)))
             (or (match q ((list _ :and _)
                           (condensed-cnf q)))
                 (list (condensed-cnf q)))))
     ((list p :or q)
      (nconc (condensed-cnf p) (condensed-cnf q))))))
