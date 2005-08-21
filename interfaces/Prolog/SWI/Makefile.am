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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
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
-I$(SWI_PROLOG_INCLUDE_DIRECTORY) \
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
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../tests/clpq.pl
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq2$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../tests/clpq2.pl
	plld -cc $(CC) -c++ $(CXX) -ld $(CXX) -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

EXPECTED_CLPQ = $(srcdir)/../tests/expected_clpq_$(COEFFICIENT_MNEMONIC)_a
EXPECTED_CLPQ2 = $(srcdir)/../tests/expected_clpq2_$(COEFFICIENT_MNEMONIC)_a
EXPECTED_PCHK = $(srcdir)/../tests/expected_pchk_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED_CLPQ = $(srcdir)/../tests/expected_clpq_$(COEFFICIENT_MNEMONIC)
EXPECTED_CLPQ2 = $(srcdir)/../tests/expected_clpq2_$(COEFFICIENT_MNEMONIC)
EXPECTED_PCHK = $(srcdir)/../tests/expected_pchk_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

check-local: pl_check_test clpq_test clpq2_test

pl_check_test: swi_pl_check.pl ../tests/pl_check.pl $(EXPECTED_PCHK)
	if [ $(srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/../tests/pl_check.pl . ; \
	fi ;\
        echo "ensure_loaded('$(srcdir)/swi_pl_check'). main." > script_pchk
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
                ./ppl_pl < script_pchk
	if [ $(srcdir) != `pwd` ]; then \
	  rm -f pl_check.pl; \
	fi
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

clpq_test: pl_clpq ../tests/script_clpq $(EXPECTED_CLPQ) 
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq <$(srcdir)/../tests/script_clpq >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq

clpq2_test: pl_clpq2 ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq2 <$(srcdir)/../tests/script_clpq2 >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

MOSTLYCLEANFILES = \
pl_clpq pl_clpq2 \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS) \
script_pchk

endif HAVE_SWI_PROLOG
