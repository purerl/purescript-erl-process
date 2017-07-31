.PHONY: ps erl all test

all: ps erl

ps:
	psc-package sources | xargs purs compile 'test/**/*.purs' 'src/**/*.purs' 

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

test:
	erl -pa ebin -noshell -eval '(test_main:main())()' -eval 'init:stop()'
