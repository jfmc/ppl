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

# Libtool -version-info for libpplgprolog.la.
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

LIBPPLGPROLOG_LT_CURRENT =  0
LIBPPLGPROLOG_LT_REVISION = 0
LIBPPLGPROLOG_LT_AGE =      0

INCLUDES = -I$(top_builddir)/src @extra_includes@

lib_LTLIBRARIES = libpplgprolog.la

libpplgprolog_la_SOURCES = ppl_gprolog.cc

libpplgprolog_la_LDFLAGS = \
-version-info $(LIBPPLGPROLOG_LT_CURRENT):$(LIBPPLGPROLOG_LT_REVISION):$(LIBPPLGPROLOG_LT_AGE)

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

EXTRA_DIST = ppl_gprolog.pl clpq.pl clpq2.pl check_script expected \
$(CLPQ_TESTS)

if HAVE_GNU_PROLOG
#check_PROGRAMS = pplgprolog
#pplgprolog_SOURCES = ppl_gprolog.cc
#pplgprolog_LDADD = libpplgprolog.la $(top_builddir)/src/libppl.la /usr/local/lib/pl-5.0.1/runtime/i686-linux/libpl.a -lreadline -ldl -ltermcap @extra_libraries@

#pplgprolog$(EXEEXT): ppl_gprolog.o ppl_gprolog.pl
#	gplc -o $@ $+ -L '-L../../../src/.libs -lppl -lgmp -lgmpxx -lstdc++'

#gp_clpq$(EXEEXT): gp_clpq.pl ppl_gprolog.o ppl_gprolog.pl ../clpq.pl
#	gplc -o $@ $+ -L '-L../../../src/.libs -lppl -lgmp -lgmpxx -lstdc++'

#check-local: pplgprolog$(EXEEXT) gp_clpq$(EXEEXT) ../check_script ../expected
#	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
#	gp_clpq <$(srcdir)/../check_script >obtained
#	diff -q --ignore-all-space $(srcdir)/../expected obtained

MOSTLYCLEANFILES = pplpl $(CLPQ_TESTS) obtained
endif
