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


# Libtool -version-info for libppl.la and libmp.la.
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

LIBPPL_LT_CURRENT =  0
LIBPPL_LT_REVISION = 0
LIBPPL_LT_AGE =      0

INCLUDES = -I$(top_builddir) -I$(top_srcdir)/src @extra_includes@

CPPFLAGS = -DPPL_INTERNAL=public -DPPL_HIDDEN=public @extra_cpp_flags@

lib_LTLIBRARIES = libppl.la

# The ordering of the following list *matters*!
INCLUDE_FILES = \
Integer.types.hh \
Integer.defs.hh \
Integer.inlines.hh \
globals.hh \
Init.types.hh \
Init.defs.hh \
Init.inlines.hh \
initializer.hh \
maxmin.hh \
SatRow.types.hh \
SatMatrix.types.hh \
Row.types.hh \
Matrix.types.hh \
Variable.types.hh \
LinExpression.types.hh \
Constraint.types.hh \
ConSys.types.hh \
Generator.types.hh \
GenSys.types.hh \
Status.types.hh \
Polyhedron.types.hh \
SatRow.defs.hh \
SatRow.inlines.hh \
SatMatrix.defs.hh \
SatMatrix.inlines.hh \
Row.defs.hh \
Row.inlines.hh \
Matrix.defs.hh \
Matrix.inlines.hh \
Variable.defs.hh \
Variable.inlines.hh \
LinExpression.defs.hh \
LinExpression.inlines.hh \
Constraint.defs.hh \
Constraint.inlines.hh \
ConSys.defs.hh \
ConSys.inlines.hh \
Generator.defs.hh \
Generator.inlines.hh \
GenSys.defs.hh \
GenSys.inlines.hh \
Status.defs.hh \
Status.inlines.hh \
Polyhedron.defs.hh \
Polyhedron.inlines.hh

libppl_la_SOURCES = \
Constraint.cc \
ConSys.cc \
GenSys.cc \
Generator.cc \
Init.cc \
LinExpression.cc \
Matrix.cc \
Polyhedron.cc \
Row.cc \
SatMatrix.cc \
SatRow.cc \
Status.cc \
Variable.cc \
conversion.cc \
minimize.cc \
simplify.cc \
globals.cc \
$(INCLUDE_FILES)

# The ordering of the following list *matters*!
DOC_FILES = \
Variable.types.hh \
LinExpression.types.hh \
Constraint.types.hh \
ConSys.types.hh \
Generator.types.hh \
GenSys.types.hh \
Polyhedron.types.hh \
Variable.defs.hh \
LinExpression.defs.hh \
Constraint.defs.hh \
ConSys.defs.hh \
Generator.defs.hh \
GenSys.defs.hh \
Polyhedron.defs.hh

libppl_la_LDFLAGS = \
-version-info $(LIBPPL_LT_CURRENT):$(LIBPPL_LT_REVISION):$(LIBPPL_LT_AGE)

noinst_HEADERS = ppl.hh ppl_install.hh

BUILT_SOURCES = ppl.hh ppl_install.hh

ppl.hh:	$(DOC_FILES)
	(cd $(srcdir) ; cat $(DOC_FILES)) \
		| sed -f $(srcdir)/ppl_filter_doxygen.sed >ppl.hh

ppl_install.hh:	ppl_header.top $(top_builddir)/config.h ppl_header.middle $(INCLUDE_FILES) ppl_header.bottom
	mkdir -p tmp
	(cd $(srcdir) ; cat $(INCLUDE_FILES)) >tmp/concat
	grep -e "^#include <" <tmp/concat | sort -u >tmp/includes
	sed -e "/^#include/d" -f $(srcdir)/ppl_filter_install.sed <tmp/concat >tmp/noincludes.cc
	cat $(srcdir)/ppl_header.top $(top_builddir)/config.h \
		$(srcdir)/ppl_header.middle tmp/includes >ppl_install.hh
	$(CXXCPP) -P @debug_flag@ tmp/noincludes.cc >>ppl_install.hh
	cat $(srcdir)/ppl_header.bottom >>ppl_install.hh
	rm -rf tmp*

# ppl.hh and ppl_install.hh are not distributed.
dist-hook:
	rm -f $(distdir)/ppl.hh $(distdir)/ppl_install.hh

install-data-local: ppl_install.hh
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) ppl_install.hh $(DESTDIR)$(includedir)/ppl.hh

uninstall-local:
	rm -f $(DESTDIR)$(includedir)/ppl.hh

EXTRA_DIST = \
ppl_header.top ppl_header.middle ppl_header.bottom \
ppl_filter_doxygen.sed ppl_filter_install.sed

MOSTLYCLEANFILES = $(BUILT_SOURCES)
