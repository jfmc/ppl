# Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

INCLUDES = -I$(top_builddir)/src @extra_includes@

noinst_LIBRARIES = libpplsicstus.a

libpplsicstus_a_SOURCES = ppl_sicstus.cc

EXTRA_DIST = ppl_sicstus.pl check_script expected

if HAVE_SICSTUS
check-local: pplsp check_script expected
	./pplsp -l $(srcdir)/clpq.pl <$(srcdir)/check_script >obtained
	diff -q $(srcdir)/expected obtained

pplsp: ppl_sicstus.s.o $(top_builddir)/src/libppl.a
	spld --main=prolog -S -o $@ --resources=./ppl_sicstus.s.o \
	-LD -lstdc++ \
	-Wl,-whole-archive $(top_builddir)/src/libppl.a -Wl,-no-whole-archive \
	@gmp_library_option@

ppl_sicstus.s.o: ppl_sicstus.pl ppl_sicstus.o
	splfr -S $(srcdir)/ppl_sicstus.pl ppl_sicstus.o

MOSTLYCLEANFILES = pplsp
endif
