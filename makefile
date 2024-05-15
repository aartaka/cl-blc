LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 4092 --no-userinit --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: blc

clean:
	rm blc

blc:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load cl-blc.asd --eval '(asdf:load-system :cl-blc)' --eval '(asdf:make :cl-blc)' --eval '(quit)'

install: sade
	cp blc $(DESTDIR)/
