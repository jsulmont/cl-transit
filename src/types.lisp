(in-package :cl-transit)

(intern "-INF")
(intern "INF")
(intern "NAN")

(defclass tr-link ()
  ((href
    :initarg :href
    :type quri:uri
    :initform (error "Must supply a QURI:QURI value for 'href")
    :reader href)
   (rel
    :initarg :rel
    :type string
    :initform (error "Must supply a string value for 'rel")
    :reader rel)
   (name
    :initarg :name
    :type string
    :accessor name)
   (render
    :initarg :render
    :type string
    :accessor render)
   (prompt
    :initarg :prompt
    :type string
    :accessor prompt)))

;; TODO  more idiomatic way?
(defmethod initialize-instance :after ((link tr-link) &key)
  (unless (typep (slot-value link 'href) 'quri:uri)
    (error "'href must be quri:uri"))
  (unless (typep (slot-value link 'rel) 'string)
    (error "'rel must be string"))
  (when (slot-boundp link 'name)
    (unless (typep (slot-value link 'name) 'string)
      (error "'name must be string")))
  (when (slot-boundp link 'render)
    (let ((render (slot-value link 'render)))
      (unless (or (equal render "image")
                  (equal render "link"))
        (error "'render must be \"link\" or \"image\""))))
  (when (slot-boundp link 'prompt)
    (unless (typep (slot-value link 'prompt) 'string)
      (error "'prompt must be string"))))

(defmethod tr-linkp ((this tr-link)) t)

(defmethod tr-linkp ((this t)) nil)

(defclass tr-timestamp ()
  ((m
    :documentation "number of milliseconds around epoch"
    :type integer
    :initarg :m
    :reader m)))

(defmethod tr-timestampp ((this tr-timestamp)) t)

(defmethod tr-timestampp ((this t)) nil)

(defclass tagged-value ()
  ((tag
    :initarg :tag
    :initform (error "Must suply a string value for 'tag")
    :reader tag)
   (rep
    :initarg :rep
    :initform (error "Must suply a string value for 'rep")
    :reader rep)))

(defmethod tagged-valuep ((this tagged-value)) t)

(defmethod tagged-valuep ((this t)) nil)

;; TODO better print-objects functions
;; for now:
(defmethod print-object ((object tr-timestamp) stream)
  (format stream "#<TR-TIMESTAMP ~a>"
          (slot-value object 'm)))

(defmethod print-object ((object tagged-value) stream)
  (format stream "#<TAGGED-VALUE tag:~a rep:~a>"
          (slot-value object 'tag)
          (slot-value object 'rep)))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                  using (hash-value value)
                collect (list key value))))

(defmethod print-object ((object tr-link) stream)
  (with-slots (href rel) object
    (format stream "<TR-LINK: href=~a rel=~a>~%"
            href rel)))
