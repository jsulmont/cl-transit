LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cl-transit.asd \
		--eval '(ql:quickload :cl-transit)' \
		--eval '(asdf:make :cl-transit)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
