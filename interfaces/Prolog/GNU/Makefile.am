# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
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

GPROLOG_CFLI_FILES = gprolog_cfli.hh

EXTRA_DIST = \
ppl_interface_generator_gprolog_pl.m4 \
$(GPROLOG_CFLI_FILES) \
gp_clpq.pl \
gnu_pl_check.pl \
README.gprolog

if BUILD_GNU_PROLOG_INTERFACE

if BUILD_WATCHDOG_LIBRARY

WATCHDOG_LIBRARY = $(top_builddir)/Watchdog/src/libpwl.la
WATCHDOG_INCLUDE_OPTIONS = -I$(top_builddir)/Watchdog/src
WATCHDOG_DLOPEN = -dlopen $(WATCHDOG_LIBRARY)
WATCHDOG_LINK_OPTIONS = -L$(top_builddir)/Watchdog/src/.libs -lpwl

endif BUILD_WATCHDOG_LIBRARY

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces \
-I$(top_srcdir)/interfaces/Prolog \
-I$(top_buildir)/interfaces/Prolog \
-I$(top_builddir)/src \
$(WATCHDOG_INCLUDE_OPTIONS) \
@extra_includes@

BUILT_SOURCES = \
@required_instantiations_prolog_cxx_headers@ \
@required_instantiations_prolog_cxx_sources@

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../../ppl_interface_generator_common_procedure_generators.m4 \
../ppl_interface_generator_prolog_procedure_generators.m4 \
../ppl_interface_generator_prolog_systems.m4 \
ppl_interface_generator_gprolog_pl.m4

pkglib_LTLIBRARIES = libppl_gprolog.la

nodist_libppl_gprolog_la_SOURCES = \
ppl_prolog_sysdep.hh \
gprolog_efli.hh \
gprolog_efli.cc \
../ppl_prolog_common.cc

EXTRA_libppl_gprolog_la_SOURCES = \
../ppl_prolog_BD_Shape_double.cc \
../ppl_prolog_BD_Shape_mpq_class.cc \
../ppl_prolog_BD_Shape_mpz_class.cc \
../ppl_prolog_Constraints_Product_C_Polyhedron_Grid.cc \
../ppl_prolog_Double_Box.cc \
../ppl_prolog_Grid.cc \
../ppl_prolog_Octagonal_Shape_double.cc \
../ppl_prolog_Octagonal_Shape_mpq_class.cc \
../ppl_prolog_Octagonal_Shape_mpz_class.cc \
../ppl_prolog_Pointset_Powerset_C_Polyhedron.cc \
../ppl_prolog_Pointset_Powerset_NNC_Polyhedron.cc \
../ppl_prolog_Polyhedron.cc \
../ppl_prolog_Rational_Box.cc

libppl_gprolog_la_LIBADD = \
@required_instantiations_prolog_cxx_objects@ \
$(top_builddir)/src/libppl.la \
$(WATCHDOG_LIBRARY) \
@extra_libraries@

libppl_gprolog_la_DEPENDENCIES = \
@required_instantiations_prolog_cxx_objects@

libppl_gprolog_la_LDFLAGS = \
-module \
-avoid-version

ppl_gprolog.pl: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. -I$(srcdir)/.. -I$(srcdir)/../.. \
           $(srcdir)/ppl_interface_generator_gprolog_pl.m4 > $@

pkglib_DATA = ppl_gprolog.pl

bin_PROGRAMS = ppl_gprolog
ppl_gprolog_SOURCES =


if ENABLE_SHARED

LIBPPL_GPROLOG = .libs/libppl_gprolog.so

else !ENABLE_SHARED

LIBPPL_GPROLOG = .libs/libppl_gprolog.a

endif !ENABLE_SHARED

ppl_gprolog$(EXEEXT): libppl_gprolog.la ppl_gprolog.pl
	gplc --c-compiler $(CXX) -o $@ \
		ppl_gprolog.pl $(LIBPPL_GPROLOG) \
		-L '$(AM_CXXFLAGS) $(CXXFLAGS) -L$(top_builddir)/src/.libs -lppl $(WATCHDOG_LINK_OPTIONS) -lgmpxx -lgmp'

gp_clpq: libppl_gprolog.la ppl_gprolog.pl gp_clpq.pl ../tests/clpq.pl
	gplc --c-compiler $(CXX) -o $@ \
		ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../tests/clpq.pl $(LIBPPL_GPROLOG) \
		-L '$(AM_CXXFLAGS) $(CXXFLAGS) -L$(top_builddir)/src/.libs -lppl $(WATCHDOG_LINK_OPTIONS) -lgmpxx -lgmp'

gp_clpq2: libppl_gprolog.la ppl_gprolog.pl gp_clpq.pl ../tests/clpq2.pl
	gplc --c-compiler $(CXX) -o $@ \
		ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../tests/clpq2.pl $(LIBPPL_GPROLOG) \
		-L '$(AM_CXXFLAGS) $(CXXFLAGS) -L$(top_builddir)/src/.libs -lppl $(WATCHDOG_LINK_OPTIONS) -lgmpxx -lgmp'

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
		$(WATCHDOG_DLOPEN) \
		-dlopen libppl_gprolog.la \
		./ppl_gprolog <script_pchk
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

clpq_test: gp_clpq ../tests/script_clpq $(EXPECTED_CLPQ) 
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		$(WATCHDOG_DLOPEN) \
		-dlopen libppl_gprolog.la \
		./gp_clpq <$(SCRIPT_CLPQ) >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq

clpq2_test: gp_clpq2 ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		$(WATCHDOG_DLOPEN) \
		-dlopen libppl_gprolog.la \
		./gp_clpq2 <$(SCRIPT_CLPQ2) >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

endif BUILD_GNU_PROLOG_INTERFACE

DISTCLEANFILES = \
ppl_gprolog.pl

MOSTLYCLEANFILES = \
gp_clpq gp_clpq2 \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS) \
script_pchk
