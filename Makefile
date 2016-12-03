#!/usr/bin/make

#-----------------------------------------------------------------------------

#DIALYZER_PLT = ~/.dialyzer_plt
DIALYZER_OPTS = --no_check_plt $(foreach D,$(DIALYZER_PLT),--plt $D)

DIAGRAMS = $(basename $(notdir $(wildcard diagrams/*.diag)))
DIAGRAMS_SVG = $(foreach D,$(DIAGRAMS),doc/images/$D.svg)

#-----------------------------------------------------------------------------

.PHONY: all doc edoc diagrams compile build clean dialyzer

all: compile doc

build: compile
edoc: doc
doc: diagrams

compile clean doc:
	rebar $@

diagrams: $(DIAGRAMS_SVG)

doc/images/%.svg: diagrams/%.diag
	blockdiag -o $@ -T svg $<

YECC_ERL_FILES = $(subst .yrl,.erl,$(subst .xrl,.erl,$(wildcard src/*.[xy]rl)))
ERL_SOURCE_FILES = $(filter-out $(YECC_ERL_FILES),$(wildcard src/*.erl))
dialyzer:
	@echo "dialyzer $(strip $(DIALYZER_OPTS)) --src src/*.erl"
	@dialyzer $(strip $(DIALYZER_OPTS)) --src $(ERL_SOURCE_FILES)

#-----------------------------------------------------------------------------
# vim:ft=make
