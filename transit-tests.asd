(in-package :asdf-user)

(defsystem "transit-tests"
  :description "Test suite for transit."
  :author "Jan Sulmont <lisp@janks.dev>"
  :depends-on (#:transit
               #:marshal
               #:dexador
               #:fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "test-transit")
                             (:file "examplars-test")))))
