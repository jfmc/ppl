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

EXTRA_DIST = ppl_xsb.cc ppl_xsb.H xsb_clpq.P xsb_clpq2.P expected expected2

if HAVE_XSB_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src @extra_includes@ @extra_cpp_flags@ @xsb_includes@

pkglib_LTLIBRARIES = ppl_xsb.la
ppl_xsb_la_SOURCES = ppl_xsb.cc
ppl_xsb_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@
ppl_xsb_la_LDFLAGS = -no-undefined -module -avoid-version

pkglib_DATA = ppl_xsb.O

ppl_xsb.O: ppl_xsb.H ppl_xsb.cc
	@cp -f $(srcdir)/ppl_xsb.H . || true
	@cp -f $(srcdir)/ppl_xsb.cc ppl_xsb.c || true
	echo "consult(ppl_xsb, [cc('g++'), cc_opts('$(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)')])." | xsb 

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

.P.O:
	xsb $<

# FIXME: we are checking against special versions of `expected'
#        and `expected2'.  This is due to the fact that XSB 2.5
#        and previous versions do not support control escape
#        sequences.

check-local: xsb_clpq.P ../check_script expected xsb_clpq2.P ../check_script2 expected2
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	@cp -f $(srcdir)/xsb_clpq.P . || true
	@cp -f $(srcdir)/../clpq.pl . || true
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
				-dlopen ppl_xsb.la \
		xsb xsb_clpq \
			<$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/expected obtained
	@cp -f $(srcdir)/xsb_clpq2.P . || true
	@cp -f $(srcdir)/../clpq2.pl . || true
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
				-dlopen ppl_xsb.la \
		xsb xsb_clpq2 \
			<$(srcdir)/../check_script2 >obtained2
	diff -q --ignore-all-space $(srcdir)/expected2 obtained2

MOSTLYCLEANFILES = \
ppl_xsb.H ppl_xsb.c ppl_xsb.O ppl_xsb.so \
xsb_clpq.P xsb_clpq.O clpq.pl xsb_clpq2.P xsb_clpq2.O clpq2.pl \
$(CLPQ_TESTS) obtained obtained2

endif
