(in-package :cl-transit-tests)

(defgeneric tr-equalp (o1 o2)
  (:documentation "test two transit values for equality"))

(defmethod tr-equalp ((l1 tr-link) (l2 tr-link))
  "we only consider the two mandatory slots"
  (with-slots ((href1 clt::href) (rel1 clt::rel)) l1
    (with-slots ((href2 clt::href) (rel2 clt::rel)) l2
      (and (equalp href1 href2) (equalp rel1 rel2)))))

(defmethod tr-equalp ((p1 tr-timestamp) (p2 tr-timestamp))
  (with-slots ((m1 cl-transit::m)) p1
    (with-slots ((m2 cl-transit::m)) p2
      (= m1 m2))))

(defmethod tr-equalp ((u1 uuid:uuid) (u2 uuid:uuid))
  (let ((str1 (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))
        (str2 (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s1 str1)
      (uuid:print-bytes s1 u1)
      (with-output-to-string (s2 str2)
        (uuid:print-bytes s2 u2)
        (string= str1 str2)))))

(defmethod tr-equalp ((s1 symbol) (s2 symbol))
  (string= s1 s2))

(defmethod tr-equalp ((ht1 hash-table) (ht2 hash-table))
  (tr-equalp
   (alexandria:hash-table-keys ht1)
   (alexandria:hash-table-keys ht2)))

(defmethod tr-equalp ((tv1 tagged-value) (tv2 tagged-value))
  (with-slots ((tag1 clt::tag) (rep1 clt::rep)) tv1
    (with-slots ((tag2 clt::tag) (rep2 clt::rep)) tv2
      (and (string= tag1 tag2)
           (equal rep1 rep2)))))


(defmethod fset:compare ((s1 symbol) (s2 symbol))
  (fset:compare (string s1) (string s2)))

(defmethod tr-equalp ((s1 fset:set) (s2 fset:set))
  (fset:equal? s1 s2))

(defmethod tr-equalp (x y)
  (equalp x y))

(defmethod tr-equalp ((c1 cons) (c2 cons))
  (when (= (length c1) (length c2))
    (every #'identity
           (loop
             for x in c1
             for y in c2
             collect (tr-equalp x y)))))

(defmethod tr-equalp ((c1 vector) (c2 vector))
  (when (= (length c1) (length c2))
    (every #'identity
           (loop
             for x across c1
             for y across c2
             collect (tr-equalp x y)))))
