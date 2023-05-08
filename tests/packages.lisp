(in-package :asdf-user)

(defpackage :cl-transit-tests
  (:use :cl :fiveam
        :cl-transit)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:alex #:alexandria))
  (:export :tr-equalp))
