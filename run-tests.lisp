
(load "cl-transit.asd")
(load "cl-transit-tests.asd")

(ql:quickload "cl-transit-tests")

(in-package :cl-transit-tests)

(uiop:quit (if (run-all-tests) 0 1))
