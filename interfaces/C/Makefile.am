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

SUBDIRS = . lpenum

# Libtool -version-info for libppl_c.la.
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
#	    0.3			    -----
#	    0.4			    0:0:0

LIBPPL_C_LT_CURRENT =  0
LIBPPL_C_LT_REVISION = 0
LIBPPL_C_LT_AGE =      0

AM_CPPFLAGS = -I$(top_srcdir)/src @extra_includes@

CPPFLAGS = -DPPL_INTERNAL=public -DPPL_HIDDEN=public @extra_cpp_flags@

lib_LTLIBRARIES = libppl_c.la
libppl_c_la_SOURCES = ppl_c.cc
libppl_c_la_LDFLAGS = \
-version-info $(LIBPPL_C_LT_CURRENT):$(LIBPPL_C_LT_REVISION):$(LIBPPL_C_LT_AGE)

include_HEADERS = ppl_c.h
