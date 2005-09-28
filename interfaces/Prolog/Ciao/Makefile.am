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

EXTRA_DIST = ppl_ciao.pl ciao_clpq.pl ciao_clpq2.pl ciao_pl_check.pl

if BUILD_CIAO_PROLOG_INTERFACE

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog/src \
@extra_includes@

pkglib_LTLIBRARIES = libppl_ciao.la
libppl_ciao_la_SOURCES = ppl_ciao.cc
libppl_ciao_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/src/libpwl.la \
@extra_libraries@
libppl_ciao_la_LDFLAGS = -module -avoid-version

pkglib_DATA = ppl_ciao.po

ppl_ciao.po: ppl_ciao.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ppl_ciao.pl .; \
	fi
	ciaoc -c ppl_ciao.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ppl_ciao.pl; \
	fi

ciao_clpq: libppl_ciao.la ppl_ciao.pl ciao_clpq.pl ../tests/clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq.pl $(srcdir)/ppl_ciao.pl .; \
	fi
	cp -f $(srcdir)/../tests/clpq.pl .
	ciaoc -o $@ ciao_clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq.pl ppl_ciao.pl; \
	fi
	rm -f clpq.pl

ciao_clpq2: libppl_ciao.la ppl_ciao.pl ciao_clpq.pl ../tests/clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq2.pl $(srcdir)/ppl_ciao.pl .; \
	fi
	cp -f $(srcdir)/../tests/clpq2.pl .
	ciaoc -o $@ ciao_clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq2.pl ppl_ciao.pl; \
	fi
	rm -f clpq2.pl

ciao_pl_check: libppl_ciao.la ppl_ciao.pl ciao_pl_check.pl ../tests/pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_pl_check.pl $(srcdir)/ppl_ciao.pl .; \
	fi
	cp -f $(srcdir)/../tests/pl_check.pl .
	ciaoc -o $@ ciao_pl_check.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_pl_check.pl ppl_ciao.pl; \
	fi
	rm -f pl_check.pl

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

pl_check_test: ciao_pl_check$(EXEEXT) $(EXPECTED_PCHK)
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_pl_check \
			 >obtained_pchk
	diff -u --ignore-all-space $(EXPECTED3) obtained_pchk

clpq_test: ciao_clpq$(EXEEXT) ../tests/script_clpq $(EXPECTED_CLPQ) 
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	GLOBALSTKSIZE=1000000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_clpq \
			<$(srcdir)/../tests/script_clpq >obtained_clpq
	diff -u --ignore-all-space $(EXPECTED) obtained_clpq

clpq2_test: ciao_clpq2$(EXEEXT) ../tests/script_clpq2 $(EXPECTED_CLPQ2)
	LANG=C; \
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../tests/$$k .; done
	GLOBALSTKSIZE=100000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/src/libpwl.la \
		-dlopen libppl_ciao.la \
		./ciao_clpq2 \
			<$(srcdir)/../tests/script_clpq2 >obtained_clpq2
	diff -u --ignore-all-space $(EXPECTED2) obtained_clpq2

MOSTLYCLEANFILES = \
*.itf *.po *.c *.so \
ciao_clpq ciao_clpq2 ciao_pl_check \
obtained_clpq obtained_clpq2 obtained_pchk \
$(CLPQ_TESTS)

endif BUILD_CIAO_PROLOG_INTERFACE
