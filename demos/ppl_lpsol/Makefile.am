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

SUBDIRS = examples

EXTRA_DIST = ppl_lpsol.c expected

if HAVE_GLPK

AM_CPPFLAGS = -I$(top_builddir)/interfaces/C @extra_includes@

noinst_PROGRAMS = ppl_lpsol
ppl_lpsol_SOURCES = ppl_lpsol.c
ppl_lpsol_LDADD = \
-lglpk \
$(top_builddir)/interfaces/C/libppl_c.la \
$(top_builddir)/src/libppl.la \
@extra_libraries@

check-local: ppl_lpsol expected
	rm -f obtained
	for k in $(srcdir)/examples/*.mps; \
	do \
	  for m in --min --max; \
	  do \
	    echo "*** ppl_lpsol -v $$m -oobtained `basename $$k`" >>obtained; \
	    $(LIBTOOL) --mode=execute -dlopen ../../src/libppl.la \
	      -dlopen ../../interfaces/C/libppl_c.la \
	        ./ppl_lpsol -v $$m -oobtained $$k >/dev/null; \
	  done \
	done
	diff -q --ignore-all-space $(srcdir)/expected obtained

MOSTLYCLEANFILES = obtained

endif
