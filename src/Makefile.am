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


# Libtool -version-info for libppl.la.
#
#  1. Start with version information of `0:0:0' for each libtool library.
#
#  2. Update the version information only immediately before a public
#     release of your software.  More frequent updates are unnecessary,
#     and only guarantee that the current interface number gets larger
#     faster.
#
#  3. If the library source code has changed at all since the last
#     update, then increment REVISION (`C:R:A' becomes `C:r+1:A').
#
#  4. If any interfaces have been added, removed, or changed since the
#     last update, increment CURRENT, and set REVISION to 0.
#
#  5. If any interfaces have been added since the last public release,
#     then increment AGE.
#
#  6. If any interfaces have been removed since the last public release,
#     then set AGE to 0.
#
#	PPL release		-version-info
#	    0.1			    -----
#	    0.2			    -----
#	    0.3			    0:0:0
#	    0.4			    1:0:1
#	    0.5			    2:0:0
#	    0.6			    3:0:0
#	    0.7			    4:0:0

LIBPPL_LT_CURRENT =  4
LIBPPL_LT_REVISION = 0
LIBPPL_LT_AGE =      0

AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir)/src @extra_includes@

lib_LTLIBRARIES = libppl.la

# The ordering of the following list *matters*!
INCLUDE_FILES = \
Integer.types.hh \
Interval.types.hh \
BoundingBox.types.hh \
Constraint.types.hh \
Generator.types.hh \
Init.types.hh \
SatRow.types.hh \
SatMatrix.types.hh \
Row.types.hh \
Matrix.types.hh \
Variable.types.hh \
LinExpression.types.hh \
ConSys.types.hh \
GenSys.types.hh \
Poly_Con_Relation.types.hh \
Poly_Gen_Relation.types.hh \
BHRZ03_Certificate.types.hh \
H79_Certificate.types.hh \
Polyhedron.types.hh \
C_Polyhedron.types.hh \
NNC_Polyhedron.types.hh \
Integer.defs.hh \
Integer.inlines.hh \
globals.defs.hh \
globals.inlines.hh \
Interval.defs.hh \
Interval.inlines.hh \
BoundingBox.defs.hh \
BoundingBox.inlines.hh \
Variable.defs.hh \
Variable.inlines.hh \
Init.defs.hh \
initializer.hh \
Topology.hh \
SatRow.defs.hh \
SatRow.inlines.hh \
SatMatrix.defs.hh \
SatMatrix.inlines.hh \
Row.defs.hh \
Row.inlines.hh \
Matrix.defs.hh \
Matrix.inlines.hh \
ConSys.defs.hh \
GenSys.defs.hh \
LinExpression.defs.hh \
LinExpression.inlines.hh \
Constraint.defs.hh \
Constraint.inlines.hh \
ConSys.inlines.hh \
Generator.defs.hh \
Generator.inlines.hh \
GenSys.inlines.hh \
Poly_Con_Relation.defs.hh \
Poly_Con_Relation.inlines.hh \
Poly_Gen_Relation.defs.hh \
Poly_Gen_Relation.inlines.hh \
BHRZ03_Certificate.defs.hh \
BHRZ03_Certificate.inlines.hh \
H79_Certificate.defs.hh \
H79_Certificate.inlines.hh \
Polyhedron.defs.hh \
Ph_Status.inlines.hh \
Polyhedron.inlines.hh \
C_Polyhedron.defs.hh \
C_Polyhedron.inlines.hh \
NNC_Polyhedron.defs.hh \
NNC_Polyhedron.inlines.hh \
Widening_Function.types.hh \
Widening_Function.defs.hh \
Widening_Function.inlines.hh \
Determinate.types.hh \
Determinate.defs.hh \
Determinate.inlines.hh \
Powerset.types.hh \
Powerset.defs.hh \
Powerset.inlines.hh \
Polyhedra_Powerset.types.hh \
Polyhedra_Powerset.defs.hh \
Polyhedra_Powerset.inlines.hh \
max_space_dimension.hh \
algorithms.hh

libppl_la_SOURCES = \
BoundingBox.cc \
Constraint.cc \
ConSys.cc \
GenSys.cc \
Generator.cc \
Init.cc \
Integer.cc \
Interval.cc \
LinExpression.cc \
Matrix.cc \
Poly_Con_Relation.cc \
Poly_Gen_Relation.cc \
BHRZ03_Certificate.cc \
H79_Certificate.cc \
Polyhedron_nonpublic.cc \
Polyhedron_public.cc \
Polyhedron_chdims.cc \
Polyhedron_widenings.cc \
C_Polyhedron.cc \
NNC_Polyhedron.cc \
Row.cc \
SatMatrix.cc \
SatRow.cc \
Ph_Status.cc \
Variable.cc \
conversion.cc \
minimize.cc \
simplify.cc \
globals.cc \
version.cc \
$(INCLUDE_FILES) \
Ph_Status.idefs.hh

# The ordering of the following list *matters*!
DOC_FILES = \
Integer.types.hh \
Variable.types.hh \
LinExpression.types.hh \
Constraint.types.hh \
ConSys.types.hh \
Generator.types.hh \
GenSys.types.hh \
Poly_Con_Relation.types.hh \
Poly_Gen_Relation.types.hh \
BHRZ03_Certificate.types.hh \
H79_Certificate.types.hh \
Polyhedron.types.hh \
C_Polyhedron.types.hh \
NNC_Polyhedron.types.hh \
Variable.defs.hh \
LinExpression.defs.hh \
Constraint.defs.hh \
ConSys.defs.hh \
Generator.defs.hh \
GenSys.defs.hh \
Poly_Con_Relation.defs.hh \
Poly_Gen_Relation.defs.hh \
BHRZ03_Certificate.defs.hh \
H79_Certificate.defs.hh \
Polyhedron.defs.hh \
C_Polyhedron.defs.hh \
NNC_Polyhedron.defs.hh \
globals.defs.hh \
Widening_Function.types.hh \
Widening_Function.defs.hh \
Widening_Function.inlines.hh \
Determinate.types.hh \
Determinate.defs.hh \
Determinate.inlines.hh \
Powerset.types.hh \
Powerset.defs.hh \
Powerset.inlines.hh \
Polyhedra_Powerset.types.hh \
Polyhedra_Powerset.defs.hh \
Polyhedra_Powerset.inlines.hh \
algorithms.hh

libppl_la_LDFLAGS = \
-version-info $(LIBPPL_LT_CURRENT):$(LIBPPL_LT_REVISION):$(LIBPPL_LT_AGE)

noinst_HEADERS = ppl.hh ppl_install.hh

BUILT_SOURCES = ppl.hh ppl_install.hh

ppl.hh:	$(DOC_FILES) $(top_builddir)/src/version.hh Makefile
	cat $(top_builddir)/src/version.hh >$@
	(cd $(srcdir) ; cat $(DOC_FILES)) >>$@

ppl_install.hh:	ppl_header.top $(top_builddir)/config.h $(top_builddir)/src/version.hh ppl_header.middle $(INCLUDE_FILES) ppl_header.bottom Makefile
	mkdir -p tmp
	cat $(top_builddir)/config.h >tmp/concat
	(cd $(srcdir) ; cat $(INCLUDE_FILES)) >>tmp/concat
	grep "^#include <" <tmp/concat | sort -u >tmp/includes
	sed -e "/^#include/d" <tmp/concat >tmp/noincludes.cc
	cat $(srcdir)/ppl_header.top \
		$(top_builddir)/src/version.hh $(top_builddir)/config.h \
		$(srcdir)/ppl_header.middle tmp/includes >$@
	$(CXXCPP) -I$(srcdir) -P @debug_flag@ tmp/noincludes.cc \
		>>$@
	cat $(srcdir)/ppl_header.bottom >>$@
	rm -rf tmp

# ppl.hh and ppl_install.hh are not distributed.
dist-hook:
	rm -f $(distdir)/ppl.hh $(distdir)/ppl_install.hh

install-data-local: ppl_install.hh
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) ppl_install.hh $(DESTDIR)$(includedir)/ppl.hh

uninstall-local:
	rm -f $(DESTDIR)$(includedir)/ppl.hh

EXTRA_DIST = ppl_header.top ppl_header.middle ppl_header.bottom version.hh.in

MOSTLYCLEANFILES = $(BUILT_SOURCES)
