SHELL := /bin/bash

DIALYZER_APPS = \
	erts kernel stdlib crypto public_key inets xmerl sasl mnesia

# some modules which use the native option will be native compiled
compile:
	./rebar -D NATIVE_COMPILE compile

# native compiling is disabled. Used for profiling.
erlang_compile:
	./rebar compile

doc:	compile
	./rebar skip_deps=true doc

eunit:	compile
	./rebar eunit

generate: 	compile
	./rebar generate

console:	generate
	rel/ec_chess/bin/ec_chess console

clean:
	./rebar clean

dialyze:	compile
	dialyzer --plt .dialyzer_plt  -pa apps/*/ebin -Wno_return \
		     --apps apps/*/ebin

.create_plt:
	dialyzer --no_check_plt --build_plt  --output_plt .dialyzer_plt \
		     --apps $(DIALYZER_APPS)

xref:		compile
	./rebar xref

check: xref dialyze

perftest:	clean compile
	escript -c ./apps/ec_perftest/scripts/perftest.escript

fprof:		clean erlang_compile
	escript -c ./apps/ec_prof/scripts/ec_profile.escript
