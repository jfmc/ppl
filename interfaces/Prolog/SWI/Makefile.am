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

EXTRA_DIST = ppl_swiprolog.cc ppl_swiprolog.pl pl_clpq.cc pl_clpq.pl swi_pl_check.pl

if HAVE_SWI_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

pkglib_LTLIBRARIES = libppl_swiprolog.la
libppl_swiprolog_la_SOURCES = ppl_swiprolog.cc
libppl_swiprolog_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@
libppl_swiprolog_la_LDFLAGS = -no-undefined -avoid-version

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
	plld -cc $(CC) -c++ $(CXX) -pl-options,-g,main., -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../clpq.pl
	plld -cc $(CC) -c++ $(CXX) -pl-options,-g,main., -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

pl_clpq2$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../clpq2.pl
	plld -cc $(CC) -c++ $(CXX) -pl-options,-g,main., -o $@ $+ \
		-L$(top_builddir)/src/.libs -lppl \
		-L$(top_builddir)/Watchdog/.libs -lpwl \
		@extra_libraries@

check-local: pl_clpq ../check_script ../expected pl_clpq2 ../check_script2 ../expected2 ../expected3
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		./pl_clpq2 <$(srcdir)/../check_script2 >obtained2
	diff -q --ignore-all-space $(srcdir)/../expected2 obtained2
	if [ $(srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/../pl_check.pl . ; \
	fi ;\
        echo "ensure_loaded('$(srcdir)/swi_pl_check'). main." > check_script3
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
                ./ppl_pl < check_script3
	if [ $(srcdir) != `pwd` ]; then \
	  rm -f pl_check.pl; \
	fi
	diff -q --ignore-all-space $(srcdir)/../expected3 obtained3

MOSTLYCLEANFILES = pl_clpq pl_clpq2 $(CLPQ_TESTS) obtained obtained2

endif
