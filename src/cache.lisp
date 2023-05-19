(in-package :cl-transit)

(defparameter *base-char-idx* 48)
(defparameter *cache-code-digits* 44)
(defparameter *min-size-cacheable* 4)
(defparameter *max-cache-entries* (expt *cache-code-digits* 2))

(defclass write-cache ()
  ((index
    :initform 0
    :accessor index)
   (cache
    :initform (make-hash-table :size *max-cache-entries*
                               :test 'equal)
    :accessor cache)))

(defmethod print-object ((this write-cache) stream)
  (with-slots (index cache) this
    (error "BINGO")
    (format t "<WC index=~a cache=~a>" index cache)))

(defclass read-cache ()
  ((index
    :initform 0
    :accessor index)
   (cache
    :initform (make-array *max-cache-entries*
                          :element-type 'string)
    :accessor cache)))

(defun code-to-index (code)
  (if (= 2 (length code))
      (- (char-code (aref code 1))
         *base-char-idx*)
      (+ (- (char-code (aref code 2)) *base-char-idx*)
         (* (- (char-code (aref code 1)) *base-char-idx*)
            *cache-code-digits*))))

(defun index-to-code (index)
   (multiple-value-bind (hi lo)
       (floor index *cache-code-digits*)
     (if (zerop hi)
         (format nil "^~a"
                 (code-char (+ lo *base-char-idx*)))
         (format nil "^~a~a"
                 (code-char (+ hi *base-char-idx*))
                 (code-char (+ lo *base-char-idx*))))))

(defun cacheable-p (str map-key?)
  (and (>= (length str) *min-size-cacheable*)
       (or map-key?
           (member (subseq str 0 2)
                   (list *TAG* "~$" "~:") :test #'string=))))

(defun cache-key-p (str)
  (and (/= (length str) 0)
       (char= (aref str 0) #\^)
       (not (string= str *MAP-AS-CHAR*))))

(defmethod cache-read ((this read-cache) str map-key?)
  (if (plusp (length str))
      (with-slots (index cache) this
        (cond
         ((cache-key-p str)
          (aref cache (code-to-index str)))
         ((cacheable-p str map-key?)
          (when (= index *max-cache-entries*)
            (setf index 0))
          (setf (aref cache index) str)
          (setf index (1+ index))
          str)
         (t str)))
      str))

(defmethod cache-write ((this write-cache) str map-key?)
  (if (plusp (length str))
      (with-slots (index cache) this
        (let ((code (gethash str cache)))
          (cond
            (code code)
            ((cacheable-p str map-key?)
             (when (= (hash-table-count cache) *max-cache-entries*)
               (clrhash cache)
               (setf index 0))
             (setf (gethash str cache) (index-to-code index))
             (setf index (1+ index))
             str)
            (t str))))
      str))
