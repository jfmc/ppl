# automake source file for the PPL.
# Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>
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

EXTRA_DIST = ppl_swiprolog.cc pl_clpq.cc pl_clpq.pl

if HAVE_SWI_PROLOG

# Libtool -version-info for libpplswiprolog.la.
#
#  1. Start with version information of `0:0:0' for each libtool library.
#
#  2. Update the version information only immediately before a public
#     release of your software.  More frequent updates are unnecessary,
#     and only guarantee that the current interface number gets larger
#     faster.
#
#  3. If the library source code has changed at all since the last
#     update, then increment REVISION (`C:R:A' becomes `C:r+1:A').
#
#  4. If any interfaces have been added, removed, or changed since the
#     last update, increment CURRENT, and set REVISION to 0.
#
#  5. If any interfaces have been added since the last public release,
#     then increment AGE.
#
#  6. If any interfaces have been removed since the last public release,
#     then set AGE to 0.
#
#	PPL release		-version-info
#	    0.1			    -----
#	    0.2			    -----
#	    0.3			    -----
#	    0.4			    0:0:0

LIBPPLSWIPROLOG_LT_CURRENT =  0
LIBPPLSWIPROLOG_LT_REVISION = 0
LIBPPLSWIPROLOG_LT_AGE =      0

INCLUDES = -I$(top_builddir)/src @extra_includes@

lib_LTLIBRARIES = libpplswiprolog.la

libpplswiprolog_la_SOURCES = ppl_swiprolog.cc

libpplswiprolog_la_LDFLAGS = \
-version-info $(LIBPPLSWIPROLOG_LT_CURRENT):$(LIBPPLSWIPROLOG_LT_REVISION):$(LIBPPLSWIPROLOG_LT_AGE)

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

#check_PROGRAMS = pplpl
#pplpl_SOURCES = pplpl.cc
#pplpl_LDADD = libpplswiprolog.la $(top_builddir)/src/libppl.la /usr/local/lib/pl-5.0.2/runtime/i686-linux/libpl.a -lreadline -ldl -ltermcap @extra_libraries@

noinst_PROGRAMS = pplpl pl_clpq

pplpl_SOURCES = pplpl.cc
pl_clpq_SOURCES = pl_clpq.cc

pplpl$(EXEEXT): ppl_swiprolog.o pplpl.o
	plld -pl-options,-g,main., -o $@ $+ -L$(top_builddir)/src/.libs -lppl @extra_libraries@

pl_clpq$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../clpq.pl
	plld -pl-options,-g,main., -o $@ $+ -L$(top_builddir)/src/.libs -lppl @extra_libraries@

check-local: pl_clpq ../check_script ../expected
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	./pl_clpq -l $(srcdir)/clpq.pl <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained

MOSTLYCLEANFILES = pplpl pl_clpq $(CLPQ_TESTS) obtained
endif
