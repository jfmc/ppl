# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>
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

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@ @extra_cpp_flags@

pkglib_LTLIBRARIES = libppl_ciao.la
libppl_ciao_la_SOURCES = ppl_ciao.cc
libppl_ciao_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/libpwl.la \
@extra_libraries@
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

ciao_clpq: libppl_ciao.la ppl_ciao.pl ciao_clpq.pl ../clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq.pl $(srcdir)/ppl_ciao.pl $(srcdir)/../clpq.pl .; \
	fi
	ciaoc -o $@ ciao_clpq.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq.pl ppl_ciao.pl clpq.pl; \
	fi

ciao_clpq2: libppl_ciao.la ppl_ciao.pl ciao_clpq.pl ../clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/ciao_clpq2.pl $(srcdir)/ppl_ciao.pl $(srcdir)/../clpq2.pl .; \
	fi
	ciaoc -o $@ ciao_clpq2.pl
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f ciao_clpq2.pl ppl_ciao.pl clpq2.pl; \
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

check-local: ciao_clpq$(EXEEXT) ../check_script ../expected ciao_clpq2$(EXEEXT) ../check_script2 ../expected2
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	GLOBALSTKSIZE=1000000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen libppl_ciao.la \
		ciao_clpq \
			<$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained
	GLOBALSTKSIZE=1000000 \
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen libppl_ciao.la \
		ciao_clpq2 \
			<$(srcdir)/../check_script2 >obtained2
	diff -q --ignore-all-space $(srcdir)/../expected2 obtained2

MOSTLYCLEANFILES = \
fake_main \
*.itf *.po *.c *.so \
$(CLPQ_TESTS) \
obtained obtained2
endif
