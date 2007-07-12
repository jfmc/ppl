# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
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

abs_srcdir = @abs_srcdir@

XSB_CFLI_FILES = xsb_cfli.hh

EXTRA_DIST = \
ppl_interface_generator_xsb_cc.m4 \
ppl_interface_generator_xsb_H.m4 \
$(XSB_CFLI_FILES) \
xsb_clpq.P \
xsb_clpq2.P \
xsb_pl_check.P \
expected_clpq_mpz expected_clpq2_mpz expected_pchk_mpz \
expected_clpq_int8 expected_clpq2_int8 expected_pchk_int8 \
expected_clpq_int16 expected_clpq2_int16 expected_pchk_int16 \
expected_clpq_int32 expected_clpq2_int32 expected_pchk_int32 \
expected_clpq_int64 expected_clpq2_int64 expected_pchk_int64 \
expected_clpq_mpz_a expected_clpq2_mpz_a expected_pchk_mpz_a \
expected_clpq_int8_a expected_clpq2_int8_a expected_pchk_int8_a \
expected_clpq_int16_a expected_clpq2_int16_a expected_pchk_int16_a \
expected_clpq_int32_a expected_clpq2_int32_a expected_pchk_int32_a \
expected_clpq_int64_a expected_clpq2_int64_a expected_pchk_int64_a

if BUILD_XSB_PROLOG_INTERFACE

BUILT_SOURCES = \
ppl_xsb.H \
ppl_xsb.cc

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../ppl_interface_generator_prolog_dat.m4 \
../ppl_interface_generator_prolog_systems.m4

ppl_xsb.H: $(interface_generator_dependencies) ppl_interface_generator_xsb_H.m4 Makefile
	m4 --prefix-builtin -I../.. -I$(srcdir)/.. -I$(srcdir)/../.. \
           $(srcdir)/ppl_interface_generator_xsb_H.m4 > $@

ppl_xsb.cc: $(interface_generator_dependencies) ppl_interface_generator_xsb_cc.m4 Makefile
	m4 --prefix-builtin -I../.. -I$(srcdir)/.. -I$(srcdir)/../.. \
           $(srcdir)/ppl_interface_generator_xsb_cc.m4 > $@

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces/Prolog \
-I$(top_buildir)/interfaces/Prolog \
-I$(top_builddir)/src \
-I$(top_builddir)/Watchdog/src \
$(XSB_PROLOG_INCLUDE_OPTIONS) \
@extra_includes@

pkglib_LTLIBRARIES = ppl_xsb.la
nodist_ppl_xsb_la_SOURCES = ppl_xsb.cc
ppl_xsb_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/src/libpwl.la \
@extra_libraries@
ppl_xsb_la_LDFLAGS = -module -avoid-version

pkglib_DATA = ppl_xsb.xwam

ppl_xsb.xwam: ppl_xsb.H ppl_xsb.cc ppl_xsb.la Makefile
	cp -f ppl_xsb.cc ppl_xsb.c
	echo "consult(ppl_xsb," \
	  "[cc('$(CXX)'), cc_opts('$(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) " \
	    "$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) " \
	    "-L../../../src/.libs -L../../../Watchdog/src/.libs')])." \
		| $(LIBTOOL) --mode=execute \
			-dlopen ../../../src/libppl.la \
			-dlopen ../../../Watchdog/src/libpwl.la \
			xsb
	rm -f ppl_xsb.c

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

# FIXME: we are checking against special versions of `expected_clpq'
#        and `expected_clpq2'.  This is due to the fact that XSB 2.6
#        and previous versions do not support control escape
#        sequences.

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

EXPECTED_CLPQ = $(srcdir)/expected_clpq_$(COEFFICIENT_MNEMONIC)_a
EXPECTED_CLPQ2 = $(srcdir)/expected_clpq2_$(COEFFICIENT_MNEMONIC)_a
EXPECTED_PCHK = $(srcdir)/expected_pchk_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED_CLPQ = $(srcdir)/expected_clpq_$(COEFFICIENT_MNEMONIC)
EXPECTED_CLPQ2 = $(srcdir)/expected_clpq2_$(COEFFICIENT_MNEMONIC)
EXPECTED_PCHK = $(srcdir)/expected_pchk_$(COEFFICIENT_MNEMONIC)

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
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_clpq \
			<$(srcdir)/../tests/script_clpq >obtained_clpq
	rm -f clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_clpq2.P . ; \
	fi
	cp -f $(srcdir)/../tests/clpq2.pl .
	rm -f xsb_clpq2.xwam
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_clpq2 \
			<$(srcdir)/../tests/script_clpq2 >obtained_clpq2
	rm -f clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq2.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

pl_check_test: ppl_xsb.la ppl_xsb.xwam xsb_pl_check.P ../tests/pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_pl_check.P . ; \
	fi
	cp -f $(srcdir)/../tests/pl_check.pl .
	rm -f xsb_pl_check.xwam
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen ppl_xsb.la \
		xsb xsb_pl_check \
			>obtained_pchk
	rm -f pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_pl_check.P; \
	fi
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

DISTCLEANFILES = \
ppl_xsb.H \
ppl_xsb.cc

MOSTLYCLEANFILES = \
ppl_xsb.xwam ppl_xsb.so \
xsb_clpq.xwam xsb_clpq2.xwam xsb_pl_check.xwam \
$(CLPQ_TESTS) obtained_clpq obtained_clpq2 obtained_pchk

endif BUILD_XSB_PROLOG_INTERFACE
