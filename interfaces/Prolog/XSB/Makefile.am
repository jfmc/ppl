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
abs_srcdir = @abs_srcdir@
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

ppl_xsb.O: ppl_xsb.H ppl_xsb.cc Makefile
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ppl_xsb.H . ; \
	fi
	cp -f $(srcdir)/ppl_xsb.cc ppl_xsb.c
	echo "consult(ppl_xsb," \
	  "[cc('$(CXX)'), cc_opts('$(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) " \
	    "$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)')])." \
		| xsb
	rm -f ppl_xsb.c
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ppl_xsb.H; \
	fi

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

# FIXME: we are checking against special versions of `expected'
#        and `expected2'.  This is due to the fact that XSB 2.5
#        and previous versions do not support control escape
#        sequences.

check-local: ppl_xsb.la xsb_clpq.P ../clpq.pl ../check_script expected xsb_clpq2.P ../clpq2.pl ../check_script2 expected2
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_clpq.P . ; \
	fi
	cp -f $(srcdir)/../clpq.pl .
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
				-dlopen ppl_xsb.la \
		xsb xsb_clpq \
			<$(srcdir)/../check_script >obtained
	rm -f clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq.P; \
	fi
	diff -q --ignore-all-space $(srcdir)/expected obtained
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/xsb_clpq2.P . ; \
	fi
	cp -f $(srcdir)/../clpq2.pl .
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
				-dlopen ppl_xsb.la \
		xsb xsb_clpq2 \
			<$(srcdir)/../check_script2 >obtained2
	rm -f clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f xsb_clpq2.P; \
	fi
	diff -q --ignore-all-space $(srcdir)/expected2 obtained2

MOSTLYCLEANFILES = \
ppl_xsb.O ppl_xsb.so \
xsb_clpq.O xsb_clpq2.O \
$(CLPQ_TESTS) obtained obtained2

endif
