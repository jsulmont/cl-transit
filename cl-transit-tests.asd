(in-package :asdf-user)

(defsystem "cl-transit-tests"
  :description "Test suite for cl-transit."
  :author "Jan Sulmont <modality@protonmail.ch>"
  :version "0.0.1"
  :depends-on (:cl-transit
               :marshal :dexador
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "test-cl-transit")
                             (:file "examplars-test")))))
