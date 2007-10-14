# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
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

AM_CPPFLAGS = \
-I$(top_builddir)/src \
-I$(top_srcdir)/src -I$(top_srcdir)/tests \
-I$(top_srcdir)/utils \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

TESTS = \
addpolynomialconstraint1 \
addconstraint1 \
addpolynomialconstraints1 \
addconstraints1 \
ascii_dump_load1 \
concatenateassign1 \
entails1 \
joinassign1 \
polynomialspacedifferenceassign1 \
addspacedimensionsandproject1 \
removespacedims1 \
removehigherspacedims1 \
meetassign1 \
affineimage1 \
polynomialimage1 \
affinepreimage1 \
polynomialpreimage1 \
polynomialconstraints1 \
polynomialspace1 \
isbottomortop1 \
addspacedimensionsandembed1 \
mapspacedims1 \
operators1

#
# Sources for the tests
#

addpolynomialconstraint1_SOURCES         = addpolynomialconstraint1.cc
addconstraint1_SOURCES                   = addconstraint1.cc
addpolynomialconstraints1_SOURCES        = addpolynomialconstraints1.cc
addconstraints1_SOURCES                  = addconstraints1.cc
ascii_dump_load1_SOURCES                 = ascii_dump_load1.cc
concatenateassign1_SOURCES               = concatenateassign1.cc
entails1_SOURCES                         = entails1.cc
joinassign1_SOURCES                      = joinassign1.cc
polynomialspacedifferenceassign1_SOURCES = polynomialspacedifferenceassign1.cc
addspacedimensionsandproject1_SOURCES    = addspacedimensionsandproject1.cc
removespacedims1_SOURCES                 = removespacedims1.cc
removehigherspacedims1_SOURCES           = removehigherspacedims1.cc
meetassign1_SOURCES                      = meetassign1.cc
affineimage1_SOURCES                     = affineimage1.cc
polynomialimage1_SOURCES                 = polynomialimage1.cc
affinepreimage1_SOURCES                  = affinepreimage1.cc
polynomialpreimage1_SOURCES              = polynomialpreimage1.cc
polynomialconstraints1_SOURCES           = polynomialconstraints1.cc
polynomialspace1_SOURCES                 = polynomialspace1.cc
isbottomortop1_SOURCES                   = isbottomortop1.cc
addspacedimensionsandembed1_SOURCES      = addspacedimensionsandembed1.cc
mapspacedims1_SOURCES                    = mapspacedims1.cc
operators1_SOURCES = operators1.cc





XFAIL_ALWAYS =

XFAIL_WITH_INT64 = \
$(XFAIL_ALWAYS)

XFAIL_WITH_INT32 = \
$(XFAIL_WITH_INT64)

XFAIL_WITH_INT16 = \
$(XFAIL_WITH_INT32)

XFAIL_WITH_INT8 = \
$(XFAIL_WITH_INT16)

if USE_INT64

XFAIL_SET = $(XFAIL_WITH_INT64)

else !USE_INT64
if USE_INT32

XFAIL_SET = $(XFAIL_WITH_INT32)

else !USE_INT32
if USE_INT16

XFAIL_SET = $(XFAIL_WITH_INT16)

else !USE_INT16
if USE_INT8

XFAIL_SET = $(XFAIL_WITH_INT8)

else !USE_INT8

XFAIL_SET = $(XFAIL_ALWAYS)

endif !USE_INT8
endif !USE_INT16
endif !USE_INT32
endif !USE_INT64

XFAIL_TESTS = $(XFAIL_SET)

check_PROGRAMS = $(TESTS)

MOSTLYCLEANFILES =
