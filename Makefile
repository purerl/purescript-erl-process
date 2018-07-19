.PHONY: ps erl all test

all: ps erl

ps:
	psc-package sources | xargs purs compile 'test/**/*.purs' 'src/**/*.purs' 

erl: ps
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

test: erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'
