# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>
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

EXTRA_DIST = ppl_gprolog_sd.cc ppl_gprolog.pl gp_clpq.pl gnu_pl_check.pl

if HAVE_GNU_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

bin_PROGRAMS = ppl_gprolog
ppl_gprolog_SOURCES =

pkglib_LTLIBRARIES = libppl_gprolog.la
libppl_gprolog_la_SOURCES = ppl_gprolog_sd.cc
libppl_gprolog_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@
libppl_gprolog_la_LDFLAGS = -no-undefined -avoid-version

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
	gplc --c-compiler $(CC) -o $@ \
		$(srcdir)/ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../tests/clpq.pl .libs/libppl_gprolog.a \
		-L '-L$(top_builddir)/src/.libs -lppl -L$(top_builddir)/Watchdog/.libs -lpwl -lgmpxx -lgmp'
gp_clpq2: libppl_gprolog.la ppl_gprolog.pl gp_clpq.pl ../tests/clpq2.pl
	gplc --c-compiler $(CC) -o $@ \
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

CHECK_SCRIPT= $(srcdir)/../tests/check_script

if USE_INT8

CHECK_SCRIPT2 = $(srcdir)/../tests/check_script2_int8

else !USE_INT8

CHECK_SCRIPT2 = $(srcdir)/../tests/check_script2

endif !USE_INT8

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

check-local: gp_clpq ../tests/check_script $(EXPECTED) gp_clpq2 ../tests/check_script2 $(EXPECTED2) ../tests/pl_check.pl $(EXPECTED3)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./gp_clpq <$(CHECK_SCRIPT) >obtained
	diff -q --ignore-all-space $(EXPECTED) obtained
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./gp_clpq2 <$(CHECK_SCRIPT2) >obtained2
	diff -q --ignore-all-space $(EXPECTED2) obtained2
	echo "['$(srcdir)/../tests/pl_check.pl', '$(srcdir)/gnu_pl_check.pl']." \
		>check_script3
	echo "main." >>check_script3
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./ppl_gprolog <check_script3
	diff -q --ignore-all-space $(EXPECTED3) obtained3

MOSTLYCLEANFILES = \
gp_clpq gp_clpq2 \
obtained obtained2 obtained3 \
$(CLPQ_TESTS) \
check_script3

endif HAVE_GNU_PROLOG
