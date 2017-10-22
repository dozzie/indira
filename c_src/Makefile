#!/usr/bin/make -f

#-----------------------------------------------------------------------------
# erlang.mk setup

include env.mk

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose_2 = set -x;
gen_verbose = $(gen_verbose_$(V))

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

#-----------------------------------------------------------------------------
# entry points for erlang.mk

OUTDIR ?= ../priv
TARGETS = $(OUTDIR)/indira_af_unix_drv.so

.PHONY: all clean

all: $(TARGETS)

clean:
	$(gen_verbose) rm -f *.o $(TARGETS)

#-----------------------------------------------------------------------------
# main compilation

SOURCES = af_unix.c
HEADERS =

AF_UNIX_OBJS = af_unix.o

$(OUTDIR)/indira_af_unix_drv.so: $(AF_UNIX_OBJS)
	$(link_verbose) $(CC) -fPIC -shared $(LDFLAGS) -o $@ $^ $(LDLIBS)

%.o: %.c
	$(c_verbose) $(CC) -fPIC $(CFLAGS) $(CPPFLAGS) -c $<

-include deps.mk
deps.mk: $(SOURCES) $(HEADERS)
	$(verbose) $(CC) -MM $(CFLAGS) $(CPPFLAGS) $(SOURCES) > $@

#-----------------------------------------------------------------------------
# auxiliary rules

$(OUTDIR):
	$(verbose) mkdir $@

$(TARGETS): | $(OUTDIR)

#-----------------------------------------------------------------------------
# vim:ft=make
