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

SICSTUS_CFLI_FILES = sicstus_cfli.h sicstus_cfli.ic sicstus_cfli.cc

EXTRA_DIST = $(SICSTUS_CFLI_FILES) ppl_sicstus_sd.cc ppl_sicstus.pl sp_clpq.pl sp_pl_check.pl

if HAVE_SICSTUS_PROLOG

AM_CPPFLAGS = \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
-I$(top_srcdir)/Watchdog \
@extra_includes@

bin_PROGRAMS = ppl_sicstus
ppl_sicstus_SOURCES =

pkglib_DATA = ppl_sicstus.s.o ppl_sicstus.pl

noinst_LTLIBRARIES = ppl_sicstus_sd.la
ppl_sicstus_sd_la_SOURCES = ppl_sicstus_sd.cc
ppl_sicstus_sd_la_LDFLAGS = -no-undefined -module -avoid-version

ppl_sicstus$(EXEEXT): ppl_sicstus.s.o $(top_builddir)/src/libppl.la $(top_builddir)/Watchdog/libpwl.la
	$(LIBTOOL) --mode=link --tag=CXX \
	spld --main=prolog \
		--resources=ppl_sicstus.s.o -o $@ \
		$(top_builddir)/src/libppl.la \
		$(top_builddir)/Watchdog/libpwl.la \
		@extra_libraries@

ppl_sicstus.s.o: ppl_sicstus.pl ppl_sicstus_sd.o
	splfr --static $(srcdir)/ppl_sicstus.pl ppl_sicstus_sd.o

ppl_sicstus.so: ppl_sicstus.pl ppl_sicstus_sd.o $(top_builddir)/src/libppl.la $(top_builddir)/Watchdog/libpwl.la
	$(LIBTOOL) --mode=link --tag=CXX \
	splfr $(srcdir)/ppl_sicstus.pl ppl_sicstus_sd.o -o $@ \
		$(top_builddir)/src/libppl.la \
		$(top_builddir)/Watchdog/libpwl.la \
		@extra_libraries@

install-data-local: ppl_sicstus.so
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) \
		$< $(DESTDIR)$(pkglibdir)/$<

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

check-local: ppl_sicstus ../check_script ../$(EXPECTED) ../check_script2 ../$(EXPECTED2) ../$(EXPECTED3)
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
	  ./ppl_sicstus -l $(srcdir)/sp_clpq.pl \
	    --goal "main('$(top_srcdir)/interfaces/Prolog/clpq')." \
	      <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED) obtained
	$(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
	  ./ppl_sicstus -l $(srcdir)/sp_clpq.pl \
	    --goal "main('$(top_srcdir)/interfaces/Prolog/clpq2')." \
	      <$(srcdir)/../check_script2 >obtained2
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED2) obtained2
	if [ $(srcdir) != `pwd` ]; then \
	  cp -f $(srcdir)/../pl_check.pl . ;\
	fi
	$(LIBTOOL) --mode=execute \
		-dlopen ../../../src/libppl.la \
		 ./ppl_sicstus -l $(srcdir)/sp_pl_check.pl \
        --goal "main."  \
			>obtained3
	if [ $(srcdir) != `pwd` ]; then \
	  rm -f pl_check.pl ;\
	fi
	diff -q --ignore-all-space $(srcdir)/../$(EXPECTED3) obtained3

MOSTLYCLEANFILES = \
ppl_sicstus.s.o ppl_sicstus.so \
$(CLPQ_TESTS) \
obtained obtained2 obtained3

endif # HAVE_SICSTUS_PROLOG
