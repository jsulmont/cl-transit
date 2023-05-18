(in-package :cl-transit)

;(setq *block-compile-default* t)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defun make-keyword (str)
  (intern str #.(find-package :keyword)))

(defun make-uuid (rep)
  (etypecase rep
    (string
     (uuid:make-uuid-from-string rep))
    ;; (cons
    ;;  (assert (= 2 (length rep)))
    ;;  (uuid:byte-array-to-uuid
    ;;   (concatenate
    ;;    '(array (unsigned-byte 8) (16))
    ;;    (int->octets (car rep))
    ;;    (int->octets (cadr rep)))))
    (vector
     (assert (= 2 (length rep)))
     (uuid:byte-array-to-uuid
      (concatenate
       '(array (unsigned-byte 8) (16))
       (int->octets (alex:first-elt rep))
       (int->octets (alex:last-elt rep)))))
    ))

(defun make-special-number (s)
  (declare (string s))
  (cond
    ((string= s "NaN") 'NAN)
    ((string= s "INF") 'INF)
    ((string= s "-INF") '-INF)))

(defstruct tag tag)

(defun make-tr-link (rep)
  (let ((link (make-instance
               'tr-link
               :href (gethash "href" rep)
               :rel (gethash "rel" rep)))
        (name (gethash "name" rep))
        (render (gethash "render" rep))
        (prompt (gethash "prompt" rep)))
    (when name (setf (name link) name))
    (when render (setf (render link) render))
    (when prompt (setf (prompt link) prompt))
    link))

(defun make-tr-timestamp (rep)
  (let ((m (if (stringp rep) ;; JSON vs MSGPACK for "m"
               (parse-integer rep) rep)))
    (make-instance 'tr-timestamp :m m)))

(defun make-tr-set (rep)
  (make-instance 'tr-set :rep (if (eql rep 'NULL) '() rep)))

(defun try-make-ratio (rep)
  (declare (list rep))
  (or #+sbcl (sb-kernel:build-ratio (car rep) (cadr rep))
      #-sbcl (make-instance 'tagged-value :tag "ratio" :rep rep)))

;; TODO should we decode the rep? (the python impl doesn't)
(defun make-cmap-hash (rep)
  (declare (vector rep))
  (unless (evenp (length rep))
    (error "CMAP rep lenght must be even"))
  (let* ((hash-size (/ (length rep) 2))
         (hash (make-hash-table :test #'equal :size hash-size)))
    (loop for i from 0 below hash-size do
      (setf (gethash (aref rep (* i 2)) hash)
            (aref rep (1+ (* i 2)))))
    hash))

#|
local-time seems bogus: the doc says timestamps are based on 2000-01-01:00:00:00Z
(local-time:make-timestamp :nsec 0) returns "2000-03-01T00:00:00.000000Z"
also, it doesn't seem to handle correctly times before epoch;
We return a `tr-timestamp' carrying the number of millisecs since epoch
|#


(defparameter *decoders*
  (dict "_" (lambda (x) (declare (ignore x)) 'NULL)
        ":" #'make-keyword
        "$" #'make-symbol
        "?" (lambda (x) (if (string= x "t") t nil))
        "i" #'parse-integer
        "d" (lambda (x) (parse-float:parse-float x :type 'single-float))
        "f" (lambda (x) (parse-float:parse-float x :type 'double-float))
        "u" #'make-uuid
        "r" #'quri:uri
        "m" #'make-tr-timestamp
        "t" #'local-time:parse-rfc3339-timestring
        "n" #'parse-integer
        "z" #'make-special-number
        "link" #'make-tr-link
        "list" (lambda (v) (coerce v 'list))
        "set" #'make-tr-set
        "cmap" #'make-cmap-hash
        "ratio" #'try-make-ratio
        "'" #'identity))

(defun default-decoder (tag rep)
  (declare (string tag rep))
  (make-instance 'tagged-value :tag tag :rep rep))

(defun parse-string (s)
  ;(declare ((simple-array character(*)) s))
  (declare (string s))
  (if (and (> (length s) 0)
           (string= (subseq s 0 1) *ESC*))
      (let* ((m (subseq s 1 2))
             (decoder (gethash m *decoders*)))
        (cond ((functionp decoder)
               (funcall decoder (subseq s 2)))
              ((member m (list *SUB* *ESC* *RES*) :test #'string=)
               (subseq s 1))
              ((string= m "#")
               (make-tag :tag (subseq s 2)))
              (t (default-decoder m (subseq s 2)))))
      s))

(defun decode-string (str cache map-key?)
  (declare (string str))
  (parse-string (cache-read cache str map-key?)))

(defun decode-tag (tag-str rep)
  (declare (string tag-str))
  (let ((decoder (gethash tag-str *decoders*)))
    (if (functionp  decoder)
        (funcall decoder rep)
        (make-instance 'tagged-value :tag tag-str :rep rep))))

(defun decode-vector (data cache map-key?)
  (declare (vector data))
  (if (plusp (length data))
      (let ((first (alex:first-elt data)))
        (if (equal *MAP-AS-CHAR* first)
            (progn
              (unless (oddp (length data))
                (error "*MAP-AS-CHAR* evenp sequence length"))
              (let* ((rest (subseq data 1))
                     (hash (make-hash-table :test #'equal
                                            :size (/ (length rest) 2))))
                (dolist (pair (serapeum:batches rest 2))
                  (setf (gethash (decode (alex:first-elt pair) cache t) hash)
                        (decode (alex:last-elt pair) cache nil)))
                hash))
            (let ((decoded (decode first cache map-key?)))
              (if (tag-p decoded)
                  (decode-tag (tag-tag decoded)
                              (decode (alex:last-elt data) cache map-key?))
                  (progn
                    (setf (alex:first-elt data) decoded)
                    (loop for i from 1 below (length data) do
                      (setf (aref data i)
                            (decode (aref data i) cache map-key?)))
                    data)))))
      data))

(defun decode-hash (data cache map-key?)
  (declare (hash-table data))
  (if (> (hash-table-count data) 1)
      (let ((hash (make-hash-table :test #'equal
                                   :size (hash-table-count data))))
        (loop for k being each hash-key of data
              using (hash-value v)
              do (setf (gethash (decode k cache t) hash)
                       (decode v cache nil)))
        hash)
      (let* ((hash-list (car (alex:hash-table-alist data)))
             (key (decode (car hash-list) cache t)))
        (if (tag-p key)
            (decode-tag (tag-tag key) (decode (cdr hash-list) cache map-key?))
            (dict key (decode (cdr hash-list) cache nil))))))

(defun decode (data &optional (cache nil) (map-key? nil))
  (when (null cache)
    (setf cache (make-instance 'read-cache)))
  (cond
    ((hash-table-p data) (decode-hash data cache map-key?))

    ((stringp data) (decode-string data cache map-key?))

    ((vectorp data) (decode-vector data cache map-key?))

    (t data)))  ;TODO atom?

(defun decode* (data &optional (cache nil) (map-key? nil))
  (let ((cl:*read-default-float-format* 'long-float))
    (if (eq *encode-target* 'JSON)
        (decode (jzon:parse data) cache map-key?)
        (decode (mpk:decode data) cache map-key?))))

(defun decode-json (data &optional (cache nil) (map-key? nil))
  (let ((*encode-target* 'JSON))
    (decode* data cache map-key?)))

(defun decode-mp (data &optional (cache nil) (map-key? nil))
  (let ((*encode-target* 'MSGPACK))
    (decode* data cache map-key?)))

