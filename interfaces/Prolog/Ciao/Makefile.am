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

EXTRA_DIST = ppl_ciao.pl fake_main.pl ciao_clpq.pl

if HAVE_CIAO_PROLOG

AM_CPPFLAGS = -I$(top_srcdir)/src @extra_includes@ @extra_cpp_flags@

pkglib_LTLIBRARIES = libppl_ciao.la
libppl_ciao_la_SOURCES = ppl_ciao.cc
libppl_ciao_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@
libppl_ciao_la_LDFLAGS = -no-undefined -module -avoid-version

pkglib_DATA = ppl_ciao.po

ppl_ciao.po: ppl_ciao.pl fake_main.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ppl_ciao.pl $(srcdir)/fake_main.pl .; \
	fi
	ciaoc fake_main.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ppl_ciao.pl fake_main.pl; \
	fi

ciao_clpq: libppl_ciao.la ppl_ciao.po ciao_clpq.pl ../clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq.pl $(srcdir)/ppl_ciao.pl $(srcdir)/../clpq.pl .; \
	fi
	ciaoc -o $@ ciao_clpq.pl clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq.pl clpq.pl; \
	fi

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

#check-local: ciao_clpq$(EXEEXT) ../check_script ../expected
#	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
#	ciao_clpq <$(srcdir)/../check_script >obtained
#	diff -q --ignore-all-space $(srcdir)/../expected obtained
#	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
#				-dlopen ppl_ciao.la \
#		xsb xsb_clpq \
#			<$(srcdir)/../check_script >obtained

#pplpl $(CLPQ_TESTS) obtained

MOSTLYCLEANFILES = *.itf *.po
endif
