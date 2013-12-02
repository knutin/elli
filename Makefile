compile:
	./rebar compile
	./rebar xref skip_deps=true

eunit:
	./rebar eunit skip_deps=true

init_dialyzer:
	dialyzer --apps stdlib kernel erts ssl public_key crypto --build_plt --output_plt .dialyzer.plt

dialyzer: compile
	dialyzer --no_native -Wno_return -Werror_handling -Wunderspecs -Wno_match -r ebin --plt .dialyzer.plt
