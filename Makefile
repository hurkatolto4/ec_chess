SHELL := /bin/bash

##
DIALYZER_APPS = \
	erts stdlib crypto public_key inets xmerl sasl tools

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
	rm -rf `find . -name *.beam`

dialyze:	compile
	dialyzer --plt .dialyzer_plt  -pa apps/*/ebin -Wno_return \
		     --apps apps/*/ebin | tee dialyzer_output.txt > /dev/null
	filter_output.sh dialyzer_output.txt dialyzer_filter_warnings.txt

.create_plt:
	dialyzer --no_check_plt --build_plt  --output_plt .dialyzer_plt \
		     --apps $(DIALYZER_APPS)

xref:		compile
	./rebar xref

check: xref dialyze

perftest:
	escript -c ./apps/ec_perftest/scripts/perftest.escript

fprof:		clean erlang_compile
	escript -c ./apps/ec_prof/scripts/ec_profile.escript
