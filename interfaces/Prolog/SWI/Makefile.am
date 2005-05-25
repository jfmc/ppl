# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PPL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

EXTRA_DIST = \
ppl_swiprolog.cc \
ppl_swiprolog.pl \
pl_clpq.cc \
pl_clpq.pl \
swi_pl_check.pl

if HAVE_SWI_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_srcdir)/Watchdog \
-I$(top_builddir)/src \
@extra_includes@

pkglib_LTLIBRARIES = libppl_swiprolog.la
libppl_swiprolog_la_SOURCES = ppl_swiprolog.cc
libppl_swiprolog_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/libpwl.la \
@extra_libraries@
libppl_swiprolog_la_LDFLAGS = -module -avoid-version

bin_PROGRAMS = ppl_pl
ppl_pl_SOURCES = ppl_pl.cc

pkglib_DATA = ppl_swiprolog.pl

CLPQ_TESTS_NONSTRICT = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

CLPQ_TESTS_STRICT = \
schedule.clpq

CLPQ_TESTS = $(CLPQ_TESTS_NONSTRICT) $(CLPQ_TESTS_STRICT)

check_PROGRAMS = pl_clpq pl_clpq2
pl_clpq_SOURCES = pl_clpq.cc
pl_clpq2_SOURCES = pl_clpq.cc

# Notice: we are linking statically until we understand whether
# and if libtool and plld can interoperate.
ppl_pl$(EXEEXT): ppl_swiprolog.o ppl_pl.o
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -pl-options,-g,main -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../tests/clpq.pl
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -pl-options,-g,main -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq2$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../tests/clpq2.pl
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -pl-options,-g,main -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

EXPECTED = $(srcdir)/../tests/expected_$(COEFFICIENT_MNEMONIC)_a
EXPECTED2 = $(srcdir)/../tests/expected2_$(COEFFICIENT_MNEMONIC)_a
EXPECTED3 = $(srcdir)/../tests/expected3_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED = $(srcdir)/../tests/expected_$(COEFFICIENT_MNEMONIC)
EXPECTED2 = $(srcdir)/../tests/expected2_$(COEFFICIENT_MNEMONIC)
EXPECTED3 = $(srcdir)/../tests/expected3_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

check-local: pl_clpq ../tests/check_script $(EXPECTED) pl_clpq2 ../tests/check_script2 $(EXPECTED2) $(EXPECTED3)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq <$(srcdir)/../tests/check_script >obtained
	diff -u --ignore-all-space $(EXPECTED) obtained
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq2 <$(srcdir)/../tests/check_script2 >obtained2
	diff -u --ignore-all-space $(EXPECTED2) obtained2
	if [ $(srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/../tests/pl_check.pl . ; \
	fi ;\
        echo "ensure_loaded('$(srcdir)/swi_pl_check'). main." > check_script3
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
                ./ppl_pl < check_script3
	if [ $(srcdir) != `pwd` ]; then \
	  rm -f pl_check.pl; \
	fi
	diff -u --ignore-all-space $(EXPECTED3) obtained3

MOSTLYCLEANFILES = \
pl_clpq pl_clpq2 \
obtained obtained2 obtained3 \
$(CLPQ_TESTS) \
check_script3

endif HAVE_SWI_PROLOG
