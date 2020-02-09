######################################################################
#                            GNU MAKEFILE                            #
######################################################################

# clear out suffix list
.SUFFIXES:

EMACS = emacs
EFLAGS = -batch

# needed modules for testing
test_objects = ert minimal-setup.el composure-test.el\
               font-lock-test.el ../org-superstar.el

######################################################################
### Rules
######################################################################
.PHONY: font-lock font-lock-inlinetask all
# define main goal of make
all:
	-$(MAKE) font-lock
	$(MAKE) font-lock-inlinetask

font-lock:
	$(EMACS) $(EFLAGS) $(patsubst %,-l %, $(test_objects)) \
	-f ert-run-tests-batch-and-exit

font-lock-inlinetask: override test_objects += org-inlinetask
font-lock-inlinetask: font-lock
