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

YAP_CFLI_FILES = yap_cfli.hh

EXTRA_DIST = \
ppl_interface_generator_yap_cc.m4 \
$(YAP_CFLI_FILES) \
ppl_yap.pl \
yap_clpq.pl \
yap_clpq2.pl \
yap_pl_check.pl \
README.yap

if BUILD_YAP_PROLOG_INTERFACE

if BUILD_WATCHDOG_LIBRARY

WATCHDOG_LIBRARY = $(top_builddir)/Watchdog/src/libpwl.la
WATCHDOG_INCLUDE_OPTIONS = -I$(top_builddir)/Watchdog/src
WATCHDOG_DLOPEN = -dlopen $(WATCHDOG_LIBRARY)
WATCHDOG_LINK_OPTIONS = -L$(top_builddir)/Watchdog/src/.libs -lpwl

endif BUILD_WATCHDOG_LIBRARY

BUILT_SOURCES = \
ppl_yap.cc

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../ppl_interface_generator_prolog_dat.m4 \
../ppl_interface_generator_prolog_systems.m4 \
ppl_interface_generator_yap_cc.m4

ppl_yap.cc: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. -I$(srcdir)/.. -I$(srcdir)/../.. \
           $(srcdir)/ppl_interface_generator_yap_cc.m4 > $@

if ENABLE_SHARED

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces/Prolog \
-I$(top_buildir)/interfaces/Prolog \
-I$(top_builddir)/src \
$(WATCHDOG_INCLUDE_OPTIONS) \
@extra_includes@

if NO_UNDEFINED

NO_UNDEFINED_FLAG = -no-undefined

endif NO_UNDEFINED

pkglib_LTLIBRARIES = ppl_yap.la
nodist_ppl_yap_la_SOURCES = ppl_yap.cc
ppl_yap_la_LIBADD = \
-lYap \
$(top_builddir)/src/libppl.la \
$(WATCHDOG_LIBRARY) \
@extra_libraries@

ppl_yap_la_LDFLAGS = $(NO_UNDEFINED_FLAG) -module -avoid-version

pkglib_DATA = ppl_yap.pl

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

pl_check_test: yap_pl_check.pl ppl_yap.pl ../tests/pl_check.pl $(EXPECTED_PCHK)
	echo "main." >script_pchk
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		$(WATCHDOG_DLOPEN) \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_pl_check.pl \
			<script_pchk >obtained_pchk
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

clpq_test: yap_clpq.pl ppl_yap.pl ../tests/clpq.pl ../tests/script_clpq $(EXPECTED_CLPQ)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	echo "main." >script_clpq
	cat $(srcdir)/../tests/script_clpq >>script_clpq
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		$(WATCHDOG_DLOPEN) \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_clpq.pl \
			<script_clpq >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq

clpq2_test: yap_clpq2.pl ppl_yap.pl ../tests/clpq2.pl ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	echo "main." >script_clpq2
	cat $(srcdir)/../tests/script_clpq2 >>script_clpq2
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		$(WATCHDOG_DLOPEN) \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_clpq2.pl \
			<script_clpq2 >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

endif ENABLE_SHARED
endif BUILD_YAP_PROLOG_INTERFACE

DISTCLEANFILES = \
ppl_yap.cc

MOSTLYCLEANFILES = \
script_clpq script_clpq2 script_pchk \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS)
