# Automake source file for the Parma Polyhedra Library.
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

AM_CPPFLAGS = \
-I$(top_srcdir)/src @extra_includes@ \
-DPPL_INTERNAL=public -DPPL_HIDDEN=public @extra_cpp_flags@

pkglib_LTLIBRARIES = libppl_swiprolog.la
libppl_swiprolog_la_SOURCES = ppl_swiprolog.cc
libppl_swiprolog_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@
libppl_swiprolog_la_LDFLAGS = -no-undefined -avoid-version

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

bin_PROGRAMS = ppl_pl
ppl_pl_SOURCES = ppl_pl.cc

check_PROGRAMS = pl_clpq
pl_clpq_SOURCES = pl_clpq.cc

# Notice: we are linking statically until we understand whether
# and if libtool and plld can interoperate.
ppl_pl$(EXEEXT): ppl_swiprolog.o ppl_pl.o
	plld -pl-options,-g,main., -o $@ $+ -L$(top_builddir)/src/.libs -lppl @extra_libraries@

pl_clpq$(EXEEXT): ppl_swiprolog.o pl_clpq.o pl_clpq.pl ../clpq.pl
	plld -pl-options,-g,main., -o $@ $+ -L$(top_builddir)/src/.libs -lppl @extra_libraries@

check-local: pl_clpq ../check_script ../expected
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	libtool --mode=execute -dlopen ../../../src/libppl.la \
		./pl_clpq -l $(srcdir)/clpq.pl <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained

MOSTLYCLEANFILES = pl_clpq $(CLPQ_TESTS) obtained
endif
