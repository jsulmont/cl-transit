(in-package :cl-transit)

(intern "MSGPACK")

(intern "JSON")

(defvar *encode-target* 'JSON
  "default ground encoding.")

(defvar *encode-alist-as-map* nil
  "alist will be encoded as maps")

(defvar *encode-plist-as-map* nil
  "plist will be encoded as maps")

(defvar *json-max-int* (1- (expt 2 53))
  "ground-integer")

(defvar *json-min-int* (- (1- (expt 2 53)))
  "ground-integer")

(defvar *msgpack-max-int* (1- (expt 2 63))
  "ground-integer")

(defvar *msgpack-min-int* (- (expt 2 63))
  "ground-integer")

(defun (setf encode-target) (format)
  (unless (or (eq format'JSON) (eq format'MSGPACK))
    (error "ground format  must be 'JSON (default) or 'MSGPACK"))
  (setf *encode-target* format))

(defun encode-target ()
  *encode-target*)
