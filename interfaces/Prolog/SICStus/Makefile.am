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

SICSTUS_CFLI_FILES = sicstus_cfli.h sicstus_cfli.ic sicstus_cfli.cc

EXTRA_DIST = \
$(SICSTUS_CFLI_FILES) \
ppl_sicstus_sd.cc \
ppl_sicstus.pl \
sp_clpq.pl \
sp_pl_check.pl

if BUILD_SICSTUS_PROLOG_INTERFACE

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

bin_PROGRAMS = ppl_sicstus
ppl_sicstus_SOURCES =

pkglib_DATA = ppl_sicstus.s.o ppl_sicstus.pl

noinst_LTLIBRARIES = ppl_sicstus_sd.la
ppl_sicstus_sd_la_SOURCES = ppl_sicstus_sd.cc
ppl_sicstus_sd_la_LDFLAGS = -module -avoid-version

ppl_sicstus$(EXEEXT): ppl_sicstus.s.o $(top_builddir)/src/libppl.la $(top_builddir)/Watchdog/libpwl.la
	$(LIBTOOL) --mode=link --tag=CXX \
	spld --main=prolog \
		--resources=ppl_sicstus.s.o -o $@ \
		$(top_builddir)/src/libppl.la \
		$(top_builddir)/Watchdog/libpwl.la \
		@extra_libraries@

ppl_sicstus.s.o: ppl_sicstus.pl ppl_sicstus_sd.o
	splfr --static $(srcdir)/ppl_sicstus.pl ppl_sicstus_sd.o

ppl_sicstus.so: ppl_sicstus.pl ppl_sicstus_sd.o $(top_builddir)/src/libppl.la $(top_builddir)/Watchdog/libpwl.la
	$(LIBTOOL) --mode=link --tag=CXX \
	splfr $(srcdir)/ppl_sicstus.pl ppl_sicstus_sd.o -o $@ \
		$(top_builddir)/src/libppl.la \
		$(top_builddir)/Watchdog/libpwl.la \
		@extra_libraries@

install-data-local: ppl_sicstus.so
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) \
		$< $(DESTDIR)$(pkglibdir)/$<

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

EXPECTED = $(srcdir)/../tests/expected_clpq_$(COEFFICIENT_MNEMONIC)_a
EXPECTED2 = $(srcdir)/../tests/expected_clpq2_$(COEFFICIENT_MNEMONIC)_a
EXPECTED3 = $(srcdir)/../tests/expected_pchk_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED = $(srcdir)/../tests/expected_clpq_$(COEFFICIENT_MNEMONIC)
EXPECTED2 = $(srcdir)/../tests/expected_clpq2_$(COEFFICIENT_MNEMONIC)
EXPECTED3 = $(srcdir)/../tests/expected_pchk_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

check-local: pl_check_test clpq_test clpq2_test

pl_check_test: ppl_sicstus ppl_sicstus.pl sp_pl_check.pl ../tests/pl_check.pl $(EXPECTED_PCHK)
	if [ $(srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/../tests/pl_check.pl . ;\
	fi
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		 ./ppl_sicstus -l $(srcdir)/sp_pl_check.pl \
        --goal "main."  \
			>obtained_pchk
	if [ $(srcdir) != `pwd` ]; then \
	  rm -f pl_check.pl ;\
	fi
	diff -u --ignore-all-space $(EXPECTED3) obtained_pchk

clpq_test: ppl_sicstus ppl_sicstus.pl sp_clpq.pl ../tests/clpq.pl ../tests/script_clpq $(EXPECTED_CLPQ)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
	  ./ppl_sicstus -l $(srcdir)/sp_clpq.pl \
	    --goal "main('$(top_srcdir)/interfaces/Prolog/tests/clpq')." \
	      <$(srcdir)/../tests/script_clpq >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED) obtained_clpq

clpq2_test: ppl_sicstus ppl_sicstus.pl sp_clpq.pl ../tests/clpq2.pl ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
	  ./ppl_sicstus -l $(srcdir)/sp_clpq.pl \
	    --goal "main('$(top_srcdir)/interfaces/Prolog/tests/clpq2')." \
	      <$(srcdir)/../tests/script_clpq2 >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED2) obtained_clpq2

MOSTLYCLEANFILES = \
ppl_sicstus.s.o ppl_sicstus.so \
$(CLPQ_TESTS) \
obtained_clpq obtained_clpq2 obtained_pchk

endif BUILD_SICSTUS_PROLOG_INTERFACE
