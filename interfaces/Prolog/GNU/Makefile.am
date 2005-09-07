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

EXTRA_DIST = ppl_gprolog_sd.cc ppl_gprolog.pl gp_clpq.pl gnu_pl_check.pl

if BUILD_GNU_PROLOG_INTERFACE

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

bin_PROGRAMS = ppl_gprolog
ppl_gprolog_SOURCES =

pkglib_LTLIBRARIES = libppl_gprolog.la
libppl_gprolog_la_SOURCES = ppl_gprolog_sd.cc
libppl_gprolog_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/libpwl.la \
@extra_libraries@
libppl_gprolog_la_LDFLAGS = -module -avoid-version

pkglib_DATA = ppl_gprolog.pl

# Notice: we are linking with libppl_gprolog.a statically.
# This is a kludge until we understand whether and if libtool
# and gplc can interoperate.
ppl_gprolog$(EXEEXT): libppl_gprolog.la ppl_gprolog.pl
	gplc --c-compiler $(CXX) -o $@ \
		$(srcdir)/ppl_gprolog.pl .libs/libppl_gprolog.a \
		-L '-L$(top_builddir)/src/.libs -lppl -L$(top_builddir)/Watchdog/.libs -lpwl -lgmpxx -lgmp'

# Notice: we are linking with libppl_gprolog.a statically.
# This is a kludge until we understand whether and if libtool
# and gplc can interoperate.
gp_clpq: libppl_gprolog.la ppl_gprolog.pl gp_clpq.pl ../tests/clpq.pl
	gplc --c-compiler $(CXX) -o $@ \
		$(srcdir)/ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../tests/clpq.pl .libs/libppl_gprolog.a \
		-L '-L$(top_builddir)/src/.libs -lppl -L$(top_builddir)/Watchdog/.libs -lpwl -lgmpxx -lgmp'
gp_clpq2: libppl_gprolog.la ppl_gprolog.pl gp_clpq.pl ../tests/clpq2.pl
	gplc --c-compiler $(CXX) -o $@ \
		$(srcdir)/ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../tests/clpq2.pl .libs/libppl_gprolog.a \
		-L '-L$(top_builddir)/src/.libs -lppl -L$(top_builddir)/Watchdog/.libs -lpwl -lgmpxx -lgmp'

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

SCRIPT_CLPQ= $(srcdir)/../tests/script_clpq

if USE_INT8

SCRIPT_CLPQ2 = $(srcdir)/../tests/script_clpq2_int8

else !USE_INT8

SCRIPT_CLPQ2 = $(srcdir)/../tests/script_clpq2

endif !USE_INT8

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

pl_check_test: gnu_pl_check.pl ../tests/pl_check.pl $(EXPECTED_PCHK)
	echo "['$(srcdir)/../tests/pl_check.pl', '$(srcdir)/gnu_pl_check.pl']." \
		>script_pchk
	echo "main." >>script_pchk
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./ppl_gprolog <script_pchk
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

clpq_test: gp_clpq ../tests/script_clpq $(EXPECTED_CLPQ) 
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./gp_clpq <$(SCRIPT_CLPQ) >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq

clpq2_test: gp_clpq2 ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./gp_clpq2 <$(SCRIPT_CLPQ2) >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

MOSTLYCLEANFILES = \
gp_clpq gp_clpq2 \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS) \
script_pchk

endif BUILD_GNU_PROLOG_INTERFACE
