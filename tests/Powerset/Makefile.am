# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
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

if VALGRIND_TESTS_ENABLED

CHECKER = \
$(VALGRIND) --tool=memcheck -q --trace-children=yes --leak-check=yes \
--num-callers=100 --leak-resolution=high \
--suppressions=$(top_srcdir)/tests/valgrind_suppressions

else !VALGRIND_TESTS_ENABLED

CHECKER =

endif !VALGRIND_TESTS_ENABLED

TESTS_ENVIRONMENT = $(CHECKER)

AM_CPPFLAGS = \
-I$(top_builddir)/src \
-I$(top_srcdir)/src -I$(top_srcdir)/tests \
-I$(top_srcdir)/utils \
@extra_includes@ \
@debug_flag@

LDADD = \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
$(top_builddir)/src/libppl.la \
@extra_libraries@

TESTS = \
frombdshape1 \
frombox1 \
fromcongruences1 \
fromconstraints1 \
fromgrid1 \
fromoctagonalshape1 \
frompolyhedron1 \
fromspacedimension1 \
simplifyusingcontext1 \
powerset1 \
pointsetpowerset1 pointsetpowerset2 pointsetpowerset3 \
pointsetpowerset4 pointsetpowerset5 \
product1

XFAIL_TESTS =

#
# Sources for the tests
#

frombdshape1_SOURCES = frombdshape1.cc

frombox1_SOURCES = frombox1.cc

fromcongruences1_SOURCES = fromcongruences1.cc

fromconstraints1_SOURCES = fromconstraints1.cc

fromgrid1_SOURCES = fromgrid1.cc

fromoctagonalshape1_SOURCES = fromoctagonalshape1.cc

frompolyhedron1_SOURCES = frompolyhedron1.cc

fromspacedimension1_SOURCES = fromspacedimension1.cc

powerset1_SOURCES = powerset1.cc

simplifyusingcontext1_SOURCES = simplifyusingcontext1.cc

pointsetpowerset1_SOURCES = pointsetpowerset1.cc
pointsetpowerset2_SOURCES = pointsetpowerset2.cc
pointsetpowerset3_SOURCES = pointsetpowerset3.cc
pointsetpowerset4_SOURCES = pointsetpowerset4.cc
pointsetpowerset5_SOURCES = pointsetpowerset5.cc

product1_SOURCES = product1.cc

check_PROGRAMS = $(TESTS)
