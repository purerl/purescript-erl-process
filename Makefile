.PHONY: ps erl all test clean distclean

.DEFAULT_GOAL := ps

all: test

ps:
	@spago build

clean:
	rm -rf output

distclean: clean
	rm -rf .spago
test: 
	@spago -x test.dhall test
