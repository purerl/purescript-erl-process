.PHONY: ps erl all run

all: ps erl

ps:
	psc-package sources | xargs pserlc 'test/**/*.purs' 'src/**/*.purs'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

run:
	erl -pa ebin -noshell -eval '(test_main:main())()' -eval 'init:stop()'
