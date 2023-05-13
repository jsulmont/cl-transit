(in-package :cl-transit-tests)

(def-suite test-examplar
  :description "test suite 1")

(in-suite test-examplar)

#| Uses test data from https://github.com/cognitect/transit-format
   which should be cloned at the same level as this repository. |#
(defparameter *examples-dir*
  (asdf:system-relative-pathname "cl-transit" "../transit-format/examples/0.8/simple"))

(defparameter *fixture-dir*
  (asdf:system-relative-pathname "cl-transit" "tests/fixtures"))

;; all defined tests
(defparameter *examples*
  '("cmap_null_key" "cmap_pathological" "dates_interesting" "doubles_interesting" "doubles_small" "false"
    "ints" "ints_interesting" "ints_interesting_neg" "keywords" "list_empty" "list_mixed" "list_nested"
    "list_simple" "map_10_items" "map_10_nested" "map_1935_nested" "map_1936_nested" "map_1937_nested" "map_mixed"
    "map_nested" "map_numeric_keys" "map_simple" "map_string_keys" "map_unrecognized_vals" "map_vector_keys"
    "maps_four_char_keyword_keys" "maps_four_char_string_keys" "maps_four_char_sym_keys" "maps_three_char_keyword_keys"
    "maps_three_char_string_keys" "maps_three_char_sym_keys" "maps_two_char_keyword_keys" "maps_two_char_string_keys"
    "maps_two_char_sym_keys" "maps_unrecognized_keys" "nil" "one" "one_date" "one_keyword" "one_string" "one_symbol"
    "one_uri" "one_uuid" "set_empty" "set_mixed" "set_nested" "set_simple" "small_ints" "small_strings" "strings_hash"
    "strings_hat" "strings_tilde" "symbols" "true" "uris" "uuids" "vector_1935_keywords_repeated_twice"
    "vector_1936_keywords_repeated_twice" "vector_1937_keywords_repeated_twice" "vector_empty" "vector_mixed"
    "vector_nested" "vector_simple" "vector_special_numbers" "vector_unrecognized_vals" "zero"))

(defparameter *marshalable-examples*
  (set-difference *examples* '("one_uri" "one_uuid" "uuids" "uris"
                               "set_nested" "set_simple" "set_mixed"
                               "one_date" "set_empty" "maps_unrecognized_keys"
                               "dates_interesting") :test #'equalp))

;; The following examples expect to fail with this impl
(defparameter *except-examples* '("one_date" "dates_interesting"))

(defun example-json (example)
  (let ((fn (format nil "~a/~a.json" *examples-dir* example)))
    (alex:read-file-into-string fn)))

(defun example-verbose (example)
  (let ((fn (format nil "~a/~a.verbose.json" *examples-dir* example)))
    (alex:read-file-into-string fn)))

(defun example-mp (example)
  (let ((fn (format nil "~a/~a.mp" *examples-dir* example)))
    (alex:read-file-into-byte-vector fn)))

(defun fixture (name)
  (format nil "~a/~a.cl" *fixture-dir* name))

(defun spit (name value)
  (with-open-file (f (fixture name)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (prin1 value f)))

(defun slurp (name)
  (with-open-file (f (fixture name)
                     :direction :input
                     :if-exists :supersede)
    (read f)))

(test json<->mp
  (let ((examples (set-difference *examples* *except-examples* :test #'equalp)))
    (dolist (example examples)
      (let ((f1 (decode-json (example-json example)))
            (f2 (decode-mp (example-mp example))))
        (is (tr-equalp f1 f2))))))

(test json<->verbose
  (let ((examples (set-difference *examples* *except-examples* :test #'equalp)))
    (dolist (example examples)
      (let ((f1 (decode-json (example-json example)))
            (f2 (decode-json (example-verbose example))))
        (is (tr-equalp f1 f2))))))

(test decode-marshalable
  (dolist (example *marshalable-examples*)
    (let* ((v (ms:unmarshal (slurp example)))
           (r1 (decode-json (example-json example)) )
           (r2 (decode-mp (example-mp example))))
      (is (tr-equalp v r1))
      (is (tr-equalp v r2)))))

(test round-trip-marshalabe
  (dolist (example *marshalable-examples*)
    (let ((value (ms:unmarshal (slurp example))))
      (is (tr-equalp value (decode-json (encode-json value))))
      (is (tr-equalp value (decode-mp (encode-mp value)))))))

(test one-uri
  (let ((r1 (decode-json (example-json "one_uri")))
        (r2 (decode-mp (example-mp "one_uri")))
        (r3 (decode-json (example-verbose "one_uri")))
        (v (quri:uri "http://example.com")))
    (is (equalp r1 v))
    (is (equalp r2 v))
    (is (equalp r3 v))
    (is (equalp v (decode-json (encode-json v))))
    (is (equalp v (decode-mp (encode-mp v))))))

(test uris
  (let ((r1 (decode-json (example-json "uris")))
        (r2 (decode-mp (example-mp "uris")))
        (r3 (decode-json (example-verbose "uris")))
        (v (list (quri:uri "http://example.com")
                 (quri:uri "ftp://example.com")
                 (quri:uri "file:///path/to/file.txt")
                 (quri:uri "http://www.詹姆斯.com/"))))
    (is (equalp r1 v))
    (is (equalp r2 v))
    (is (equalp r3 v))
    (is (equalp v (decode-json (encode-json v))))
    (is (equalp v (decode-mp (encode-mp v))))))

(test one-uuid
  (let ((r1 (decode-json (example-json "one_uuid")))
        (r2 (decode-mp (example-mp "one_uuid")))
        (r3 (decode-json (example-verbose "one_uuid")))
        (v (uuid:make-uuid-from-string "5A2CBEA3-E8C6-428B-B525-21239370DD55")))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-json (encode-json v))))
    (is (tr-equalp v (decode-mp (encode-mp v))))))

(test uuids
  (let ((r1 (decode-json (example-json "uuids")))
        (r2 (decode-mp (example-mp "uuids")))
        (r3 (decode-json (example-verbose "uuids")))
        (v (mapcar #'uuid:make-uuid-from-string
                   '("5A2CBEA3-E8C6-428B-B525-21239370DD55"
                     "D1DC64FA-DA79-444B-9FA4-D4412F427289"
                     "501A978E-3A3E-4060-B3BE-1CF2BD4B1A38"
                     "B3BA141A-A776-48E4-9FAE-A28EA8571F58"))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-json (encode-json v))))
    (is (tr-equalp v (decode-mp (encode-mp v))))))

(test one-date
  (let ((r1 (decode-json (example-json "one_date")))
        (r2 (decode-mp (example-mp "one_date")))
        (r3 (decode-json (example-verbose "one_date")))
        (ts (make-instance 'tr-timestamp :m 946728000000))
        (lt (local-time:parse-rfc3339-timestring "2000-01-01T12:00:00.000000Z")))
    (is (tr-equalp r1 ts))
    (is (tr-equalp r2 ts))
    (is (local-time:timestamp= r3 lt))
    (is (local-time:timestamp= (decode-mp (encode-mp lt))))
    (is (local-time:timestamp= (decode-json (encode-json lt))))))

(test set-empty
  (let ((r1 (decode-json (example-json "set_empty")))
        (r2 (decode-mp (example-mp "set_empty")))
        (r3 (decode-json (example-verbose "set_empty")))
        (v (make-instance 'tr-set :rep '())))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

(test set-simple
  (let ((r1 (decode-json (example-json "set_simple")))
        (r2 (decode-mp (example-mp "set_simple")))
        (r3 (decode-json (example-verbose "set_simple")))
        (v (make-instance 'tr-set :rep '(1 3 2))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

(test set-nested
  (let ((r1 (decode-json (example-json "set_nested")))
        (r2 (decode-mp (example-mp "set_nested")))
        (r3 (decode-json (example-verbose "set_nested")))
        (v (make-instance
            'tr-set
            :rep (list (make-instance 'tr-set :rep '(1 3 2))
                       (make-instance
                        'tr-set
                        :rep (list 'NULL 0 2.0d0 "~eight" 1 t "five" nil
                                   '|seven| :|six|))))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

(test set-mixed
  (let ((r1 (decode-json (example-json "set_mixed")))
        (r2 (decode-mp (example-mp "set_mixed")))
        (r3 (decode-json (example-verbose "set_mixed")))
        (v (make-instance
            'tr-set
            :rep (list 'NULL 0 2.0d0 "~eight" 1 t "five" nil
                       '|seven| :|six|))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

(test maps-unrocognized-keys
  (let ((r1 (decode-json (example-json "maps_unrecognized_keys")))
        (r2 (decode-mp (example-mp "maps_unrecognized_keys")))
        (r3 (decode-json (example-verbose "maps_unrecognized_keys")))
        (v (list (make-instance 'tagged-value :tag "abcde" :rep :|anything|)
                 (make-instance 'tagged-value :tag "fghij" :rep :|anything-else|))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

(test transit-link
  (let ((v (make-instance
            'tr-link :href (quri:uri "ftp://prep.ai.mit.edu")
            :rel "a string" :render "link")))
    (is (tr-equalp v (decode-mp (encode-mp v))))
    (is (tr-equalp v (decode-json (encode-json v))))))

#+sbcl
(test ratio
  (is (= (decode-mp (encode-mp #xFADED/FACADE)) #xFADED/FACADE))
  (is (= (decode-json (encode-json #xFADED/FACADE)) #xFADED/FACADE))
  (is (= (decode-mp (encode-mp 22/33)) 2/3))
  (is (= (decode-json (encode-json 22/33)) 2/3)))

(test dotted-pair
  (let ((v (cons 1 2))
        (v* '(1 2)))
    (is (equal v* (decode-json (encode-json v))))
    (is (equal v* (decode-json (encode-json v))))
    (is (equal (decode-json (encode-json v))
               (decode-mp (encode-mp v))))))
