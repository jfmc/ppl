# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
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

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

AM_CPPFLAGS = @extra_includes@

noinst_LIBRARIES = libppl_utils.a
libppl_utils_a_SOURCES = \
timings.hh \
timings.cc

dist_noinst_SCRIPTS = \
build_header.in \
text2cxxarray.in

bin_SCRIPTS = \
build_header \
text2cxxarray

CLEANFILES = $(bin_SCRIPTS)

.PHONY: zchmods

bin_SCRIPTS += zchmods

zchmods:
	chmod +x build_header
	chmod +x text2cxxarray
