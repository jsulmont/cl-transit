(in-package :cl-transit)

(intern "MSGPACK")

(intern "JSON")

(defvar *encode-target* 'JSON
  "default graound serialization.")

(defvar *encode-alist-as-map* nil
  "alist will be encoded as maps")

(defvar *encode-plist-as-map* nil
  "plist will be encoded as maps")

(defvar *encode-json-maxint* 9007199254740992
  "ground-integer")

(defun (setf encode-target) (format)
  (unless (or (eq format'JSON) (eq format'MSGPACK))
    (error "ground format  must be 'JSON (default) or 'MSGPACK"))
  (setf *encode-target* format))

(defun encode-target ()
  *encode-target*)
