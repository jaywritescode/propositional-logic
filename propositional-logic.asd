(defsystem "propositional-logic"
  :version "0.1.0"
  :author "Jay Harris"
  :license "MIT"
  :depends-on ("trivia")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "propositional-logic/tests"))))

(defsystem "propositional-logic/tests"
  :author "Jay Harris"
  :license "MIT"
  :depends-on ("propositional-logic"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for propositional-logic"
  :perform (test-op (op c) (symbol-call :fiveam '#:run! 'propositional-logic/tests::all-tests)))
