REBAR=rebar
ERL_SHELL=erl
SOURCES=$(wildcard src/*)

compile: $(SOURCES)
	@$(REBAR) compile

update:
	@$(REBAR) get-deps

start: compile
	@$(ERL_SHELL) -pa ebin/ -pa deps/erl_img/ebin/

test: compile
	rm -rf out/
	mkdir out
	@$(ERL_SHELL) -pa ebin/ -pa deps/erl_img/ebin/ -run img_proc test -run init stop -noshell

clean: 
	@$(REBAR) clean
	rm -rf *.beam *.dump
	rm -rf out/

.PHONY: clean