(in-package :cl-transit)

(defun alistp (l)
   "Alist predicate"
   (and (consp l) (consp (car l)) (atom (caar l))))

 (defun plistp (l)
   "Plist predicate."
   (and (consp l) (keywordp (car l)) (consp (cdr l))))

(defun is-byte-array (data-type)
  (and (vectorp data-type)
       (equal '(unsigned-byte 8)
              (array-element-type data-type))))

(defun encode-string (data cache map-key?)
  (declare (type string data))
  (if (plusp (length data))
      (let ((s (if (or (char= (char data 0) #\~)
                       (char= (char data 0) #\^))
                   (format nil "~~~a" data)
                   data)))
        (cache-write cache s map-key?))
      data))

(defun encode-integer (data)
  (declare (integer data))
  (let ((data% (abs data)))
    (cond
      ((< data% *encode-json-maxint*) data)
      ((typep data 'fixnum)
       (if (eq *encode-target* 'JSON)
           (format nil "~~i~a" data)
           data))
      (t (format nil "~~n~a" data)))))

(defun encode-uri (data)
  (declare (quri:uri data))
  (format nil "~~r~a" data))

;; TODO idiomatic?
(defun encode-tr-link (data)
  (declare (tr-link data))
  (if (eq *encode-target* 'MSGPACK)
      (let ((rc (make-hash-table :test #'equalp)))
        (with-slots (href rel) data
          (setf (gethash "href" rc) (encode-uri href))
          (setf (gethash "rel" rc) rel))
        (when (slot-boundp data 'name)
          (setf (gethash "name" rc) (slot-value data 'name)))
        (when (slot-boundp data 'prompt)
          (setf (gethash "prompt" rc) (slot-value data 'prompt)))
        (when (slot-boundp data 'render)
          (setf (gethash "render" rc) (slot-value data 'render)))
        (list "~#link" rc))
      (let ((rc '("^ ")))
        (with-slots (href rel) data
          (setf rc (nconc rc (list "href" (encode-uri href))))
          (nconc rc (list "rel" rel)))
        (when (slot-boundp data 'name)
          (nconc rc (list "name" (slot-value data 'name))))
        (when (slot-boundp data 'prompt)
          (nconc rc (list "prompt" (slot-value data 'prompt))))
        (when (slot-boundp data 'render)
          (nconc rc (list "render" (slot-value data 'render))))
        (list "~#link" rc))))

;; TODO alist/plist
(defun encode-hash-table (data cache map-key?)
  (declare (ignore map-key?)
           (hash-table data))
  (if (every #'atom (alex:hash-table-keys data))
      (if (eq *encode-target* 'MSGPACK)
          (let ((rc (make-hash-table
                     :test #'equalp
                     :size (hash-table-count data))))
            (maphash (lambda (k v)
                       (let ((k% (encode k cache t))
                             (v% (encode v cache nil)))
                         (setf (gethash k% rc) v%)))
                     data)
            rc)
          (cons "^ "
                (loop for k being the hash-key in data
                        using (hash-value v)
                      collect (encode k cache t)
                      collect (encode v cache nil))))
      (list (cache-write cache "~#cmap" nil)
            (loop for k being the hash-key in data
                    using (hash-value v)
                  collect (encode k cache)
                  collect (encode v cache))

            )
      ))

(defun encode-hash-cons (data cache map-key?)
  (declare (cons data))
  (cond
    ((plistp data)
     (encode-hash-table (alex:plist-hash-table data) cache map-key?))
    ((alistp data)
     (encode-hash-table (alex:alist-hash-table data) cache map-key?))))

(defun encode-keyword (data cache map-key?)
  (declare (keyword data))
  (cache-write cache (format nil "~~:~a" data) map-key?))

(defun encode-symbol (data cache map-key?)
  (declare (symbol data))
  (cache-write cache (format nil "~~$~a" data) map-key?))

(defun encode-array (data cache map-key?) ;; TODO rewrite?
  (declare (vector data))
  (apply #'vector (loop for x across data
             collect (encode x cache map-key?))))

(defun encode-list (data cache map-key?)
  (declare (cons data))
  (mapcar (lambda (x) (encode x cache map-key?)) data))

(defun special-numberp (data)
  (member data (list 'INF '-INF 'NAN)))

(defun encode-special-number (data)
  (declare (symbol data))
  (if (eql data 'NAN)
      "~zNaN"
      (format nil "~~z~a" data)))

(defun encode-rfc3339 (data)
  (declare (local-time:timestamp data))
  (format nil "~~t~a" data))

(defun encode-null (data cache map-key?)
  (declare (ignore data cache))
  (if (eq *encode-target* 'msgpack)
      nil
      (if map-key? "~_" 'NULL)))

(defun encode-tr-set (data cache map-key?)
  (declare (tr-set data))
  (list "~#set"
        (mapcar (lambda (x) (encode x cache map-key?))
                (slot-value data 'rep))))

(defun encode-tr-timestamp (data)
  (declare (tr-timestamp data))
  (if (eq *encode-target* 'MSGPACK)
      (list "~#m" (slot-value data 'm))
      (format nil "~~m~a" (slot-value data 'm))))

;; stolen from https://github.com/EuAndreh/cl-intbytes/blob/master/src/cl-intbytes.lisp
(defun octets->uint (array n-bytes &optional (start 0))
  "Interprets `N-BYTES` of a given `ARRAY` as an unsigned integer."
  (let ((int 0))
    (loop
       for byte-position from 0 below (* 8 n-bytes) by 8
       for array-position from 0 below n-bytes
       do (setf (ldb (byte 8 byte-position) int)
                (aref array (+ start array-position))))
    int))

(defun encode-uuid (data)
  (declare (uuid:uuid data))
  (if (eq *encode-target* 'MSGPACK)
      (let* ((rep (reverse (uuid:uuid-to-byte-array data)))
            (lo64 (octets->uint rep 8))
            (hi64 (octets->uint rep 8 8)))
        (list "~#u" (list hi64 lo64)))
      (format nil "~~u~a" data)))

(defun encode-tagged-value (data cache map-key?)
  (declare (tagged-value data))
  (with-slots (tag rep) data
    (list (format nil "~~#~a" tag)
          (encode rep cache map-key?))))

(declaim (inline encode-false))
(defun encode-false ()
  (if (eq *encode-target* 'MSGPACK)
      :false
      nil))

(declaim (inline encode-ratio))
(defun encode-ratio (data)
  (declare (type ratio data))
  (list "~#ratio"
           (list (encode (numerator data))
                 (encode (denominator data)))))

(declaim (inline pair-p))
(defun pair-p (data)
  (and (consp data)
       (not (listp (cdr data)))))

(defun encode (data &optional (cache nil) (map-key? nil))
  (when (null cache)
    (setf cache (make-instance 'write-cache)))
  (cond
    ((integerp data) (encode-integer data))
    ((typep data 'float) data)
    ((stringp data) (encode-string data cache map-key?))
    ((eql 'null data) (encode-null data cache map-key?))
    ((eql nil data) (encode-false))
    ((eql T data) T)
    ((vectorp data) (encode-array data cache map-key?))
    ((hash-table-p data) (encode-hash-table data cache map-key?))
    ((pair-p data) (encode-list (list (car data) (cdr data)) cache map-key?))
    ((consp data) (encode-list data cache map-key?))
    ((typep data 'quri:uri) (encode-uri data))
    ((special-numberp data) (encode-special-number data))
    ((keywordp data ) (encode-keyword data cache map-key?))
    ((symbolp data) (encode-symbol data cache map-key?))
    ((tr-linkp data) (encode-tr-link data))
    ((tr-setp data) (encode-tr-set data cache map-key?))
    ((typep data 'local-time:timestamp) (encode-rfc3339 data))
    ((tr-timestampp data) (encode-tr-timestamp data))
    ((typep data 'uuid:uuid) (encode-uuid data))
    ((tagged-valuep data) (encode-tagged-value data cache map-key?))
    ((typep data 'ratio) (encode-ratio data))
    (t data)))

(defun encode* (data &optional (cache nil) (map-key? nil))
  (let ((mpk:*encode-alist-as-map* nil)
        (result (encode data cache map-key?)))
    (if (eq *encode-target* 'JSON)
        (jzon:stringify result)
        (mpk:encode result))))

(defun encode-json (data &optional (cache nil) (map-key? nil))
  (let ((*encode-target* 'JSON))
    (encode* data cache map-key?)))

(defun encode-mp (data &optional (cache nil) (map-key? nil))
  (let ((*encode-target* 'MSGPACK))
    (encode* data cache map-key?)))

;; (defparameter *examples-dir*
;;   (asdf:system-relative-pathname "cl-transit" "../transit-format/examples/0.8/simple"))

;; (defparameter *fixture-dir*
;;   (asdf:system-relative-pathname "cl-transit" "tests/fixtures"))

;; (defun example-json (example)
;;   (let ((fn (format nil "~a/~a.json" *examples-dir* example)))
;;     (alex:read-file-into-string fn)))

;; (defun example-verbose (example)
;;   (let ((fn (format nil "~a/~a.verbose.json" *examples-dir* example)))
;;     (alex:read-file-into-string fn)))

;; (defun example-mp (example)
;;   (let ((fn (format nil "~a/~a.mp" *examples-dir* example)))
;;     (alex:read-file-into-byte-vector fn)))

;; (defun fixture (name)
;;   (format nil "~a/~a.cl" *fixture-dir* name))

;; (defun spit (name value)
;;   (with-open-file (f name
;;                      :direction :output
;;                      :if-exists :supersede
;;                      :if-does-not-exist :create)
;;     (write-string value f)))

;; (defun slurp (name)
;;   (with-open-file (f (fixture name)
;;                      :direction :input
;;                      :if-exists :supersede)
;;     (read f)))

;; (decode-mp (example-mp "cmap_pathological"))

;; (defvar val)

;; (defvar cache)

;; (setf val (ms:unmarshal(slurp "cmap_pathological")))

;; (setf val (ms:unmarshal(slurp "map_vector_keys")))

;; (setf cache (make-instance 'write-cache))

;; (encode-json val )

;; (format t "~a~%" cache)

;; (spit "/tmp/t1.json" (encode-json val cache))
