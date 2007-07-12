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

CIAO_CFLI_FILES = ciao_cfli.hh

EXTRA_DIST = \
ppl_interface_generator_ciao_pl.m4 \
$(CIAO_CFLI_FILES) \
ciao_clpq.pl \
ciao_clpq2.pl \
ciao_pl_check.pl

if BUILD_CIAO_PROLOG_INTERFACE
if ENABLE_SHARED

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces/Prolog \
-I$(top_builddir)/src \
-I$(top_builddir)/Watchdog/src \
@extra_includes@

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../ppl_interface_generator_prolog_dat.m4 \
../ppl_interface_generator_prolog_systems.m4 \
ppl_interface_generator_ciao_pl.m4

pkglib_LTLIBRARIES = libppl_ciao.la
libppl_ciao_la_SOURCES = ppl_ciao.cc
libppl_ciao_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/src/libpwl.la \
@extra_libraries@
libppl_ciao_la_LDFLAGS = -module -avoid-version

pkglib_DATA = ppl_ciao.po

ppl_ciao.pl: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. -I$(srcdir)/.. -I$(srcdir)/../.. \
           $(srcdir)/ppl_interface_generator_ciao_pl.m4 > $@

ppl_ciao.po: ppl_ciao.pl
	ciaoc -c ppl_ciao.pl

# All the invocations of `ciaoc' to build `ciao_pl_check', `ciao_clpq'
# and `ciao_clpq2' will try to build identical glue files (e.g.,
# `ppl_ciao_LINUXi86.so').  On parallel builds, this may cause errors.
# This is why we make `ciao_clpq' and `ciao_clpq2' depend on `ciao_pl_check':
# building the latter will build the glue files once and for all;  after
# that, `ciao_clpq' and `ciao_clpq2' can be built in parallel.
ciao_pl_check: .libs/libppl_ciao.so ppl_ciao.po ciao_pl_check.pl ../tests/pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_pl_check.pl .; \
	fi
	cp -f $(srcdir)/../tests/pl_check.pl .
	ciaoc -o $@ ciao_pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_pl_check.pl; \
	fi
	rm -f pl_check.pl

ciao_clpq: .libs/libppl_ciao.so ppl_ciao.po ciao_clpq.pl ../tests/clpq.pl | ciao_pl_check
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq.pl .; \
	fi
	cp -f $(srcdir)/../tests/clpq.pl .
	ciaoc -o $@ ciao_clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq.pl; \
	fi
	rm -f clpq.pl

ciao_clpq2: .libs/libppl_ciao.so ppl_ciao.po ciao_clpq.pl ../tests/clpq2.pl | ciao_pl_check
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq2.pl .; \
	fi
	cp -f $(srcdir)/../tests/clpq2.pl .
	ciaoc -o $@ ciao_clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq2.pl; \
	fi
	rm -f clpq2.pl


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

pl_check_test: ciao_pl_check$(EXEEXT) .libs/libppl_ciao.so $(EXPECTED_PCHK)
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_pl_check \
			 >obtained_pchk
	diff -u --ignore-all-space $(EXPECTED_PCHK) obtained_pchk

clpq_test: ciao_clpq$(EXEEXT) .libs/libppl_ciao.so ../tests/script_clpq $(EXPECTED_CLPQ)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	GLOBALSTKSIZE=1000000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_clpq \
			<$(srcdir)/../tests/script_clpq >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED_CLPQ) obtained_clpq

clpq2_test: ciao_clpq2$(EXEEXT) .libs/libppl_ciao.so ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	GLOBALSTKSIZE=100000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_clpq2 \
			<$(srcdir)/../tests/script_clpq2 >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED_CLPQ2) obtained_clpq2

DISTCLEANFILES = \
ppl_ciao.pl

MOSTLYCLEANFILES = \
*.itf *.po *.c *.so \
ciao_clpq ciao_clpq2 ciao_pl_check \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS)

endif ENABLE_SHARED
endif BUILD_CIAO_PROLOG_INTERFACE
