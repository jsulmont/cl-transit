;;;; transit.asd ++++
(in-package :asdf-user)

(defsystem #:transit
  :author "Jan Sulmont <lisp@janks.dev>"
  :description "Transit library for Common Lisp"
  :serial t
  :license "MIT"
  :source-control (:git "https://github.com/jsulmont/cl-transit")
  :depends-on (#:alexandria
               #:fset
               #:serapeum
               #:com.inuoe.jzon
               #:cl-messagepack
               #:local-time
               #:quri
               #:bit-smasher
               #:frugal-uuid
               #:parse-float
               #:flexi-streams)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "transit")
     (:file "config")
     (:file "types")
     (:file "cache")
     (:file "decode")
     (:file "encode"))))
  :in-order-to ((test-op (test-op cl-transit-tests))))
