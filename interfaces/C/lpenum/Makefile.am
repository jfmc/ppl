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

MPS_FILES = afiro.mps bgprtr.mps ex1.mps sample.mps unboundedmin.mps

EXTRA_DIST = lpenum.c $(MPS_FILES) expected

if HAVE_GLPK

AM_CPPFLAGS = -I$(top_srcdir)/interfaces/C @extra_includes@

noinst_PROGRAMS = lpenum
lpenum_SOURCES = lpenum.c
lpenum_LDADD = \
-lglpk \
$(top_builddir)/interfaces/C/libppl_c.la \
$(top_builddir)/src/libppl.la \
@extra_libraries@

check-local: $(MPS_FILES) lpenum #expected
	rm -f obtained
	for k in $(MPS_FILES); \
	do \
	  for m in --min --max; \
	  do \
	    echo "*** lpenum -v $$m -oobtained $$k" >>obtained; \
	    $(LIBTOOL) --mode=execute -dlopen ../../../src/libppl.la \
	      -dlopen ../libppl_c.la \
	        ./lpenum -v $$m -oobtained $(srcdir)/$$k >/dev/null; \
	  done \
	done
	diff -q --ignore-all-space $(srcdir)/expected obtained

MOSTLYCLEANFILES = obtained

endif
