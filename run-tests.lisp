(load "transit.asd")
(load "transit-tests.asd")

(ql:quickload "transit-tests")

(in-package :transit-tests)

(uiop:quit (if (run-all-tests) 0 1))
