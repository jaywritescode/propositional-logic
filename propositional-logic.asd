(defsystem "propositional-logic"
  :version "0.1.0"
  :author "jay harris"
  :license ""
  :depends-on ("trivia")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "propositional-logic/tests"))))

(defsystem "propositional-logic/tests"
  :author "jay harris"
  :license ""
  :depends-on ("propositional-logic"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for propositional-logic"
  :perform (test-op (op c) (symbol-call :rove :run c)))
