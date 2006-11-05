# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

if BUILD_JAVA_INTERFACE

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_srcdir)/src @extra_includes@

lib_LTLIBRARIES = libppl_java.la

libppl_java_la_SOURCES = \
ppl_java_C_Polyhedron.cc \
ppl_java_common.hh \
ppl_java_Polyhedron.cc \
ppl_java_common.cc \
ppl_java_common.hh

BUILT_SOURCES = \
ppl_java_C_Polyhedron.h \
ppl_java_Polyhedron.h

libppl_java_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@

ppl_java_C_Polyhedron.h:
	javah -classpath .. ppl_java.C_Polyhedron

ppl_java_Polyhedron.h:
	javah -classpath .. ppl_java.Polyhedron

MOSTLYCLEANFILES =

endif BUILD_JAVA_INTERFACE
