LISP ?= sbcl --non-interactive

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	\
		--load transit.asd \
		--eval '(ql:quickload :transit)' \
		--eval '(asdf:make :transit)'

test:
	$(LISP) \
		--load run-tests.lisp
