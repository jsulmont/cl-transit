(in-package :asdf-user)

(defpackage :transit-tests
  (:use :cl :fiveam
        :transit)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:alex #:alexandria))
  (:export :tr-equalp))
