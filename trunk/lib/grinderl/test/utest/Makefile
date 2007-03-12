# really ad-hoc Makefile

SOURCES=${wildcard *.erl}
BEAMS=${SOURCES:.erl=.beam}

all: ${BEAMS}
	@make clean

${BEAMS}: %.beam: %.erl
	erlc $<
	erl -pa ../../ebin -eval 'eunit:test($*).' -s init stop

clean:
	rm -vf ${BEAMS}
