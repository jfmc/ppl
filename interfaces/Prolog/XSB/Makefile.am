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
abs_srcdir = @abs_srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

EXTRA_DIST = \
ppl_xsb.cc \
ppl_xsb.H \
xsb_clpq.P \
xsb_clpq2.P \
xsb_pl_check.P \
expected_mpz expected2_mpz expected3_mpz \
expected_int8 expected2_int8 expected3_int8 \
expected_int16 expected2_int16 expected3_int16 \
expected_int32 expected2_int32 expected3_int32 \
expected_int64 expected2_int64 expected3_int64 \
expected_mpz_a expected2_mpz_a expected3_mpz_a \
expected_int8_a expected2_int8_a expected3_int8_a \
expected_int16_a expected2_int16_a expected3_int16_a \
expected_int32_a expected2_int32_a expected3_int32_a \
expected_int64_a expected2_int64_a expected3_int64_a

if HAVE_XSB_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@ \
@xsb_includes@

pkglib_LTLIBRARIES = ppl_xsb.la
ppl_xsb_la_SOURCES = ppl_xsb.cc
ppl_xsb_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/libpwl.la \
@extra_libraries@
ppl_xsb_la_LDFLAGS = -module -avoid-version

pkglib_DATA = ppl_xsb.xwam

ppl_xsb.xwam: ppl_xsb.H ppl_xsb.cc ppl_xsb.la Makefile
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ppl_xsb.H . ; \
	fi
	cp -f $(srcdir)/ppl_xsb.cc ppl_xsb.c
	echo "consult(ppl_xsb," \
	  "[cc('$(CXX)'), cc_opts('$(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) " \
	    "$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)')])." \
		| $(LIBTOOL) --mode=execute \
			-dlopen ../../../src/libppl.la \
			-dlopen ../../../Watchdog/libpwl.la \
			xsb
	rm -f ppl_xsb.c
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ppl_xsb.H; \
	fi

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

# FIXME: we are checking against special versions of `expected'
#        and `expected2'.  This is due to the fact that XSB 2.6
#        and previous versions do not support control escape
#        sequences.

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

EXPECTED = $(srcdir)/expected_$(COEFFICIENT_MNEMONIC)_a
EXPECTED2 = $(srcdir)/expected2_$(COEFFICIENT_MNEMONIC)_a
EXPECTED3 = $(srcdir)/expected3_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED = $(srcdir)/expected_$(COEFFICIENT_MNEMONIC)
EXPECTED2 = $(srcdir)/expected2_$(COEFFICIENT_MNEMONIC)
EXPECTED3 = $(srcdir)/expected3_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

check-local: ppl_xsb.la xsb_clpq.P ../tests/clpq.pl ../tests/script_clpq xsb_clpq2.P ../tests/clpq2.pl ../tests/script_clpq2 pl_check_test 
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_clpq.P . ; \
	fi
	cp -f $(srcdir)/../tests/clpq.pl .
	rm -f xsb_clpq.xwam
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_clpq \
			<$(srcdir)/../tests/script_clpq >obtained
	rm -f clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED) obtained
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_clpq2.P . ; \
	fi
	cp -f $(srcdir)/../tests/clpq2.pl .
	rm -f xsb_clpq2.xwam
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_clpq2 \
			<$(srcdir)/../tests/script_clpq2 >obtained2
	rm -f clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq2.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED2) obtained2

pl_check_test: ppl_xsb.la ppl_xsb.xwam xsb_pl_check.P ../tests/pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_pl_check.P . ; \
	fi
	cp -f $(srcdir)/../tests/pl_check.pl .
	rm -f xsb_pl_check.xwam
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_pl_check \
			>obtained3
	rm -f pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_pl_check.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED3) obtained3

MOSTLYCLEANFILES = \
ppl_xsb.xwam ppl_xsb.so \
xsb_clpq.xwam xsb_clpq2.xwam xsb_pl_check.xwam \
$(CLPQ_TESTS) obtained obtained2 obtained3

endif HAVE_XSB_PROLOG
