SHELL := /bin/bash

compile:
	rebar compile

doc:	compile
	rebar skip_deps=true doc

eunit:
	rebar compile eunit

generate: 	compile
	rebar generate

console:	generate
	rel/ec_chess/bin/ec_chess console

clean:
	rebar clean

dialyze:	compile
	dialyzer -pa apps/*/ebin -Wno_return --apps ec_chess

xref:		compile
	rebar xref

check: xref dialyze

perftest:
	./apps/ec_perftest/scripts/perftest.escript
