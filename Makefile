CASK ?= cask

all: test

test:
	${CASK} exec ert-runner

.PHONY:	all test
