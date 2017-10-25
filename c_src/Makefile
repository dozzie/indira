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
TARGETS = $(OUTDIR)/indira_af_unix_drv.so \
          $(OUTDIR)/indira_stdin_drv.so \
          $(OUTDIR)/indira_unix_unlinker

.PHONY: all clean

all: $(TARGETS)

clean:
	$(gen_verbose) rm -f *.o $(TARGETS)

#-----------------------------------------------------------------------------
# main compilation

SOURCES = af_unix.c stdin.c unlinker.c
HEADERS =

AF_UNIX_OBJS = af_unix.o
STDIN_OBJS = stdin.o
UNLINKER_OBJS = unlinker.o

$(OUTDIR)/indira_af_unix_drv.so: $(AF_UNIX_OBJS)
	$(link_verbose) $(CC) -fPIC -shared $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(OUTDIR)/indira_stdin_drv.so: $(STDIN_OBJS)
	$(link_verbose) $(CC) -fPIC -shared $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(OUTDIR)/indira_unix_unlinker: $(UNLINKER_OBJS)
	$(link_verbose) $(CC) -fPIC $(LDFLAGS) -o $@ $^

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
