(in-package :asdf-user)

(defsystem "cl-transit"
  :author "Jan Sulmont <modality@protonmail.ch>"
  :version "0.0.1"
  :license "MIT"
  :description "Transit library for Common Lisp"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "https://github.com/jsulmont/cl-transit")
  :depends-on ("alexandria" "fset"
               "serapeum" "com.inuoe.jzon"
               "cl-messagepack" "local-time"
               "quri" "bit-smasher" "uuid" ; "arrow-macros"
               "parse-float" "flexi-streams")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "cl-transit")
     (:file "config")
     (:file "types")
     (:file "cache")
     (:file "decode")
     (:file "encode"))))
  :in-order-to ((test-op (test-op cl-transit-tests))))
