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

EXTRA_DIST = ppl_gprolog_sd.cc ppl_gprolog.pl gp_clpq.pl

if HAVE_GNU_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src @extra_includes@ \
-DPPL_INTERNAL=public -DPPL_HIDDEN=public @extra_cpp_flags@

bin_PROGRAMS = ppl_gprolog
ppl_gprolog_SOURCES =

pkglib_LTLIBRARIES = libppl_gprolog.la
libppl_gprolog_la_SOURCES = ppl_gprolog_sd.cc

# Notice: we are linking with libppl_gprolog.a statically.
# This is a kludge until we understand whether and if libtool
# and gplc can interoperate.
ppl_gprolog$(EXEEXT): libppl_gprolog.la ppl_gprolog.pl $(top_builddir)/src/.libs/libppl.a
	gplc -o $@ $(srcdir)/ppl_gprolog.pl .libs/libppl_gprolog.a \
		-L '-L$(top_builddir)/src/.libs -lppl -lgmp -lgmpxx -lstdc++'

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

gp_clpq: libppl_gprolog.a ppl_gprolog.pl gp_clpq.pl ../clpq.pl
	gplc -o $@ $(srcdir)/ppl_gprolog.pl $(srcdir)/gp_clpq.pl \
		$(srcdir)/../clpq.pl \
-L '-L. -lppl_gprolog -L$(top_builddir)/src/.libs -lppl -lgmp -lgmpxx -lstdc++'

check-local: gp_clpq ../check_script ../expected
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	libtool --mode=execute -dlopen ../../../src/libppl.la \
		./gp_clpq <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained

MOSTLYCLEANFILES = \
gp_clpq $(CLPQ_TESTS) obtained
endif
