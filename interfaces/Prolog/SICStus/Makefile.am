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

INCLUDES = -I$(top_builddir)/src @extra_includes@

noinst_LIBRARIES = libpplsicstus.a

libpplsicstus_a_SOURCES = ppl_sicstus.cc

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

EXTRA_DIST = ppl_sicstus.pl clpq.pl clpq2.pl check_script expected \
$(CLPQ_TESTS)

if HAVE_SICSTUS
check-local: pplsp check_script expected
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/$$k .; done
	./pplsp -l $(srcdir)/clpq.pl <$(srcdir)/check_script >obtained
	diff -q --ignore-all-space $(srcdir)/expected obtained

pplsp: ppl_sicstus.s.o $(top_builddir)/src/libppl.la
	spld --main=prolog -S -o $@ --resources=./ppl_sicstus.s.o \
	-LD -lstdc++ \
	-Wl,-whole-archive $(top_builddir)/src/.libs/libppl.a -Wl,-no-whole-archive \
	@extra_libraries@

ppl_sicstus.s.o: ppl_sicstus.pl ppl_sicstus.o
	splfr -S $(srcdir)/ppl_sicstus.pl ppl_sicstus.o

MOSTLYCLEANFILES = pplsp $(CLPQ_TESTS) obtained
endif
