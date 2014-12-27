REBAR=rebar
ERL_SHELL=erl
SOURCES=$(wildcard src/*)

compile: $(SOURCES)
	@$(REBAR) compile

update:
	@$(REBAR) get-deps

start:
	@$(ERL_SHELL) -pa ebin/ -pa deps/erl_img/ebin/

clean: 
	@$(REBAR) clean

.PHONY: clean