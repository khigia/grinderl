# Description: ad-hoc (Linux) Makefile for standard Erlang/OTP application
# Author: Ludovic Coquelle <lcoquelle@gmail.com>

# Configuration

CMD_ERL      = erl
CMD_DIALYZER = dialyzer
APPNAME      = $(shell pwd | xargs basename | sed -e 's/-.*$$//')

BUILDOPT_PROD={smp, enable}
BUILDOPT_DEV=debug_info

DOCOPT="{todo, true}, {private, true}"


# Main targets

all: prodbuild docs

prodbuild:
	@echo "==== Compiling application ===="
	${CMD_ERL} -noshell -pa ebin -eval 'make:all([${BUILDOPT_PROD}])' -s init stop
	@echo

docs:
	@echo "==== Compiling documentation ===="
	${CMD_ERL} -noshell -pa ebin -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' -s init stop
	@echo


# Development targets

dev: devbuild test checks docs

devbuild:
	@echo "==== Compiling application ===="
	${CMD_ERL} -noshell -pa ebin -eval 'make:all([${BUILDOPT_DEV}])' -s init stop
	@echo

test: devbuild
	@echo "==== Testing application (require eunit) ===="
	@cd test; make
	@echo

checks: xref dialyzer cover

xref:
	@echo "==== Checking code: xref ===="
	${CMD_ERL} -noshell -pa ebin -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop
	@echo

dialyzer:
	@echo "==== Checking code: dialyzer ===="
	${CMD_DIALYZER} -I include --src -c src
	@echo

cover:
	@echo "==== Checking code: cover ===="
	@echo "... nothing yet!"
	@echo

clean:
	@echo "==== Removing built files ===="
	rm -vf ebin/*.beam
	rm -vf test/*.beam
	rm -vf logs/*
	touch doc/remove.me # ensure next command does not failed
	ls -1 -I*.edoc -I*.txt doc | sed -e 's#\(.*\)#doc/\1#' | xargs rm -v
	@echo
