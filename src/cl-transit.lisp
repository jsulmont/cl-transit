(in-package :cl-transit)

(intern "MSGPACK")
(intern "JSON")

;; Constants
(defparameter *ESC* "~")
(defparameter *SUB* "^")
(defparameter *RES* "`")
(defparameter *TAG* "~#")
(defparameter *QUOTE* "'")
(defparameter *MAP-AS-CHAR* "^ ")

