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

EXTRA_DIST = ppl_yap.pl yap_clpq.pl yap_clpq2.pl yap_pl_check.pl

if HAVE_YAP_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

pkglib_LTLIBRARIES = ppl_yap.la
ppl_yap_la_SOURCES = ppl_yap.cc
ppl_yap_la_LIBADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/Watchdog/libpwl.la \
@extra_libraries@
ppl_yap_la_LDFLAGS = -no-undefined -module -avoid-version

pkglib_DATA = ppl_yap.pl

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

if USE_INT64

EXPECTED = expected_int64
EXPECTED2 = expected2_int64
EXPECTED3 = expected3_int64

else !USE_INT64
if USE_INT32

EXPECTED = expected_int32
EXPECTED2 = expected2_int32
EXPECTED3 = expected3_int32

else !USE_INT32
if USE_INT16

EXPECTED = expected_int16
EXPECTED2 = expected2_int16
EXPECTED3 = expected3_int16

else !USE_INT16
if USE_INT8

EXPECTED = expected_int8
EXPECTED2 = expected2_int8
EXPECTED3 = expected3_int8

else !USE_INT8

EXPECTED = expected
EXPECTED2 = expected2
EXPECTED3 = expected3

endif !USE_INT8
endif !USE_INT16
endif !USE_INT32
endif !USE_INT64

check-local: yap_clpq.pl ../check_script ../$(EXPECTED) yap_clpq2.pl ../check_script2 ../$(EXPECTED2) ppl_yap.pl ../pl_check.pl ../$(EXPECTED3)
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	echo "main." >check_script
	cat $(srcdir)/../check_script >>check_script
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_clpq.pl \
			<check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED) obtained
	echo "main." >check_script2
	cat $(srcdir)/../check_script2 >>check_script2
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_clpq2.pl \
			<check_script2 >obtained2
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED2) obtained2
	echo "main." >check_script3
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		-dlopen ../../../Watchdog/libpwl.la \
		-dlopen ppl_yap.la \
		yap -l $(srcdir)/yap_pl_check.pl \
			<check_script3 >obtained3
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED3) obtained3

MOSTLYCLEANFILES = \
check_script check_script2 check_script3 \
obtained obtained2 obtained3 \
$(CLPQ_TESTS)

endif # HAVE_YAP_PROLOG
