(in-package :transit-tests)

(def-suite test-examplar
  :description "test suite 1")

(in-suite test-examplar)

#| Uses test data from https://github.com/cognitect/transit-format
which should be cloned at the same level as this repository. |#
(defparameter *examples-dir*
  (asdf:system-relative-pathname "transit" "../transit-format/examples/0.8/simple"))

(defparameter *fixture-dir*
  (asdf:system-relative-pathname "transit" "tests/fixtures"))

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
                               "set_nested" "set_simple" "set_mixed" "set_empty"
                               "one_date" "maps_unrecognized_keys"
                               "dates_interesting") :test #'equal))

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

(defun decode-json-example (example)
  (decode-json (example-json example)))

(defun decode-mp-example (example)
  (decode-mp (example-mp example)))

(defun decode-verbose-example (example)
  (decode-json (example-verbose example)))

(defun fixture (name)
  (format nil "~a/~a.cl" *fixture-dir* name))

(defun spit (name value)
  (with-open-file (f (fixture name)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (prin1 value f)))

(defun slurp (name)
  (with-open-file (f (fixture name) :direction :input)
    (read f)))

(test json-verbose-mp-decode-to-same
  (let ((examples (set-difference *examples* *except-examples* :test #'equal)))
    (dolist (example examples)
      (let ((f1 (decode-json-example example))
            (f2 (decode-mp-example example))
            (f3 (decode-verbose-example example)))
        (is (tr-equalp f1 f2))
        (is (tr-equalp f2 f3))))))

(test decode-marshalable
  (dolist (example *marshalable-examples*)
    (let* ((v (ms:unmarshal (slurp example)))
           (r1 (decode-json-example example) )
           (r2 (decode-mp-example example))
           (r3 (decode-verbose-example example)))
      (is (tr-equalp v r1))
      (is (tr-equalp v r2))
      (is (tr-equalp v r3)))))

(defun rnd-trip-json (val)
  (decode-json (encode-json val)))

(defun rnd-trip-mp (val)
  (decode-mp (encode-mp val)))

(test rnd-trip-marshalabe
  (dolist (example *marshalable-examples*)
    (let ((v (ms:unmarshal (slurp example))))
      (is (tr-equalp v (rnd-trip-json v)))
      (is (tr-equalp v (rnd-trip-mp v))))))

(test one-uri
  (let ((r1 (decode-json-example "one_uri"))
        (r2 (decode-mp-example "one_uri"))
        (r3 (decode-verbose-example "one_uri"))
        (v (quri:uri "http://example.com")))
    (is (equalp r1 v))
    (is (equalp r2 v))
    (is (equalp r3 v))
    (is (equalp v (rnd-trip-json v)))
    (is (equalp v (rnd-trip-mp v)))))

(test uris
  (let ((r1 (decode-json-example "uris"))
        (r2 (decode-mp-example "uris"))
        (r3 (decode-verbose-example "uris"))
        (v (coerce (list (quri:uri "http://example.com")
                         (quri:uri "ftp://example.com")
                         (quri:uri "file:///path/to/file.txt")
                         (quri:uri "http://www.詹姆斯.com/"))
                   'vector)))
    (is (equalp r1 v))
    (is (equalp r2 v))
    (is (equalp r3 v))
    (is (equalp v (rnd-trip-json v)))
    (is (equalp v (rnd-trip-mp v)))))

(test one-uuid
  (let ((r1 (decode-json-example "one_uuid"))
        (r2 (decode-mp-example "one_uuid"))
        (r3 (decode-verbose-example "one_uuid"))
        (v (fuuid:from-string "5A2CBEA3-E8C6-428B-B525-21239370DD55")))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-json v)))
    (is (tr-equalp v (rnd-trip-mp v)))))

(test uuids
  (let ((r1 (decode-json-example "uuids"))
        (r2 (decode-mp-example "uuids"))
        (r3 (decode-verbose-example "uuids"))
        (v  (coerce (mapcar #'fuuid:from-string
                            '("5A2CBEA3-E8C6-428B-B525-21239370DD55"
                              "D1DC64FA-DA79-444B-9FA4-D4412F427289"
                              "501A978E-3A3E-4060-B3BE-1CF2BD4B1A38"
                              "B3BA141A-A776-48E4-9FAE-A28EA8571F58"))
                    'vector)))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-json v)))
    (is (tr-equalp v (rnd-trip-mp v)))))

(test one-date
  (let ((r1 (decode-json-example "one_date"))
        (r2 (decode-mp-example "one_date"))
        (r3 (decode-verbose-example "one_date"))
        (ts (make-instance 'tr-timestamp :m 946728000000))
        (lt (local-time:parse-rfc3339-timestring "2000-01-01T12:00:00.000000Z")))
    (is (tr-equalp r1 ts))
    (is (tr-equalp r2 ts))
    (is (local-time:timestamp= r3 lt))
    (is (local-time:timestamp= (rnd-trip-mp lt)))
    (is (local-time:timestamp= (rnd-trip-json lt)))))

(test set-empty
  (let ((r1 (decode-json-example "set_empty"))
        (r2 (decode-mp-example "set_empty"))
        (r3 (decode-verbose-example "set_empty"))
        (v (fset:set)))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

(test set-simple
  (let ((r1 (decode-json-example "set_simple"))
        (r2 (decode-mp-example "set_simple"))
        (r3 (decode-verbose-example "set_simple"))
        (v (fset:set 1 3 2)))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

(test set-nested
  (let ((r1 (decode-json-example "set_nested"))
        (r2 (decode-mp-example "set_nested"))
        (r3 (decode-verbose-example "set_nested"))
        (v (fset:set (fset:set 1 2 3)
                     (fset:set nil 0 1 2.0d0 T ':|six| 'NULL
                               ':|seven| "five" "~eight"))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

(test set-mixed
  (let ((r1 (decode-json-example "set_mixed"))
        (r2 (decode-mp-example "set_mixed"))
        (r3 (decode-verbose-example "set_mixed"))
        (v (fset:set nil 0 1 2.0d0 T ':|six| 'NULL
                     ':|seven| "five" "~eight")))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

(test maps-unrocognized-keys
  (let ((r1 (decode-json-example "maps_unrecognized_keys"))
        (r2 (decode-mp-example "maps_unrecognized_keys"))
        (r3 (decode-verbose-example "maps_unrecognized_keys"))
        (v  (make-array 2 :initial-contents
                        (list
                         (make-instance 'tagged-value :tag "abcde" :rep :|anything|)
                         (make-instance 'tagged-value :tag "fghij" :rep :|anything-else|)))))
    (is (tr-equalp r1 v))
    (is (tr-equalp r2 v))
    (is (tr-equalp r3 v))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

(test transit-link
  (let ((v (make-instance
            'tr-link :href (quri:uri "ftp://prep.ai.mit.edu")
            :rel "a string" :render "link")))
    (is (tr-equalp v (rnd-trip-mp v)))
    (is (tr-equalp v (rnd-trip-json v)))))

#+sbcl
(test ratio
  (is (= (rnd-trip-mp #xFADED/FACADE) #xFADED/FACADE))
  (is (= (rnd-trip-json #xFADED/FACADE) #xFADED/FACADE))
  (is (= (rnd-trip-mp  22/33) 2/3))
  (is (= (rnd-trip-json 22/33) 2/3)))

(test dotted-pair
  (let ((v (cons 1 2))
        (v* '(1 2)))
    (is (equalp v* (rnd-trip-json v)))
    (is (equalp v* (rnd-trip-mp v)))))

#+nope
(dolist (example *marshalable-examples*)
  (spit example (ms:marshal (decode-mp-example example))))
