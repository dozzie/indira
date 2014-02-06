#!/usr/bin/make

#-----------------------------------------------------------------------------

#DIALYZER_PLT = ~/.dialyzer_plt
DIALYZER_OPTS = --no_check_plt $(foreach D,$(DIALYZER_PLT),--plt $D)

#-----------------------------------------------------------------------------

.PHONY: all doc edoc compile build dialyzer

all: compile doc

doc edoc:
	rebar doc

compile build:
	rebar compile

YECC_ERL_FILES = $(subst .yrl,.erl,$(subst .xrl,.erl,$(wildcard src/*.[xy]rl)))
ERL_SOURCE_FILES = $(filter-out $(YECC_ERL_FILES),$(wildcard src/*.erl))
dialyzer:
	@echo "dialyzer $(strip $(DIALYZER_OPTS)) --src src/*.erl"
	@dialyzer $(strip $(DIALYZER_OPTS)) --src $(ERL_SOURCE_FILES)

#-----------------------------------------------------------------------------

#srpm: VERSION=$(shell awk '$$1 == "%define" && $$2 == "_version" {print $$3}' redhat/*.spec)
#srpm: PKGNAME=erlang-indira
#srpm:
#	rm -rf rpm-build
#	mkdir -p rpm-build/rpm/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
#	git archive --format=tar --prefix=$(PKGNAME)-$(VERSION)/ HEAD | gzip -9 > rpm-build/rpm/SOURCES/$(PKGNAME)-$(VERSION).tar.gz
#	rpmbuild --define="%_usrsrc $$PWD/rpm-build" --define="%_topdir %{_usrsrc}/rpm" -bs redhat/*.spec
#	mv rpm-build/rpm/SRPMS/$(PKGNAME)-*.src.rpm .
#	rm -r rpm-build

#-----------------------------------------------------------------------------
# vim:ft=make
