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

EXTRA_DIST = ppl_sicstus.cc ppl_sicstus.pl sp_clpq.pl

if HAVE_SICSTUS_PROLOG

INCLUDES = -I$(top_builddir)/src @extra_includes@

bin_PROGRAMS = ppl_sicstus

ppl_sicstus_SOURCES = ppl_sicstus.cc

ppl_sicstus$(EXEEXT): static_library $(top_builddir)/src/libppl.la
	spld --static --main=prolog \
		--resources=ppl_sicstus.s.o -o $@ \
		-LD -lstdc++ \
		-Wl,-whole-archive $(top_builddir)/src/.libs/libppl.a \
		-Wl,-no-whole-archive \
		@extra_libraries@

static_library: ppl_sicstus.pl ppl_sicstus.o
	splfr --verbose --static $(srcdir)/ppl_sicstus.pl ppl_sicstus.o
	touch static_library

dynamic_library: ppl_sicstus.pl ppl_sicstus.o
	splfr --verbose $(srcdir)/ppl_sicstus.pl ppl_sicstus.o
	touch dynamic_library

CLPQ_TESTS = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

check-local: ppl_sicstus ../check_script ../expected
	for k in $(CLPQ_TESTS); do cp -f $(srcdir)/../$$k .; done
	./ppl_sicstus -l $(srcdir)/sp_clpq.pl --goal "main." <$(srcdir)/../check_script >obtained
	diff -q --ignore-all-space $(srcdir)/../expected obtained

MOSTLYCLEANFILES = $(CLPQ_TESTS) obtained static_library dynamic_library
endif
