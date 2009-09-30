# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
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
$(TEST_CPPFLAGS) \
-I$(top_builddir)/src \
-I$(top_srcdir)/src \
-I$(top_srcdir)/tests \
-I$(top_srcdir)/utils \
@extra_includes@ \
@debug_flag@

LDADD = \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
$(top_builddir)/src/libppl.la \
@extra_libraries@

ORIGINAL_TESTS = \
digitalfilters1 \
bdshape2

#bdshape1 \
#floatingpointexpr1 \
#linearform1 \
#octagonalshape1 \
#octagonalshape2 \
#polyhedron1 \
#polyhedron2




DERIVED_TESTS =

ALL_TESTS = $(ORIGINAL_TESTS) $(DERIVED_TESTS)

# This will be overridden by the `run_tests' script.
TEST_CPPFLAGS=-DANALYZER_FP_FORMAT=float -DANALYZED_FP_FORMAT=float_ieee754_half

dist_check_SCRIPTS = run_tests

TESTS = run_tests.stamp

.PHONY: run_tests.stamp
run_tests.stamp: run_tests
	+MAKE=$(MAKE) $(srcdir)/run_tests
	echo "true" >run_tests.stamp
	chmod +x run_tests.stamp

XFAIL_TESTS =

# Do not change the ordering in what follows.
if SUPPORTED_FLOAT
FLOAT_INSTANCES= \
float/float_ieee754_half \
float/float_ieee754_single \
float/float_ieee754_double \
float/float_ieee754_quad \
float/float_ibm_single \
float/float_ibm_double \
float/float_intel_double_extended
endif
if SUPPORTED_DOUBLE
DOUBLE_INSTANCES= \
double/float_ieee754_half \
double/float_ieee754_single \
double/float_ieee754_double \
double/float_ieee754_quad \
double/float_ibm_single \
double/float_ibm_double \
double/float_intel_double_extended
endif
if SUPPORTED_LONG_DOUBLE
LONG_DOUBLE_INSTANCES= \
long_double/float_ieee754_half \
long_double/float_ieee754_single \
long_double/float_ieee754_double \
long_double/float_ieee754_quad \
long_double/float_ibm_single \
long_double/float_ibm_double \
long_double/float_intel_double_extended
endif

# Do not change the ordering in what follows.
INSTANCES = \
$(FLOAT_INSTANCES) \
$(DOUBLE_INSTANCES) \
$(LONG_DOUBLE_INSTANCES)

print_check_PROGRAMS:
	echo $(check_PROGRAMS)

print_INSTANCES:
	echo $(INSTANCES)


#
# Sources for the tests
#

digitalfilters1_SOURCES = digitalfilters1.cc

bdshape2_SOURCE = bdshape2.cc

polyhedron2_SOURCES = polyhedron2.cc

polyhedron1_SOURCES = polyhedron1.cc

bdshape1_SOURCES = bdshape1.cc

floatingpointexpr1_SOURCES = floatingpointexpr1.cc

linearform1_SOURCES = linearform1.cc

octagonalshape1_SOURCES = octagonalshape1.cc

octagonalshape2_SOURCES = octagonalshape2.cc


#
# Sources and compilation flags for the derived check programs
#

DERIVED_CXXFLAGS = -DDERIVED_TEST $(AM_CXXFLAGS)

#nnc_frompolyhedron1_SOURCES = frompolyhedron1.cc
#nnc_frompolyhedron1_CXXFLAGS = $(DERIVED_CXXFLAGS)

BUGS =

check_PROGRAMS = $(ALL_TESTS) $(BUGS)

#EXTRA_DIST = $(refinewithconstraints2_SRCS)

#MOSTLYCLEANFILES = \
#ascii_dump_load1.dat

CLEANFILES = \
run_tests.stamp

DISTCLEANFILES = \
dirty_marker

$(top_builddir)/utils/libppl_utils.a:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/utils libppl_utils.a

$(top_builddir)/tests/libppl_tests.a:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/tests libppl_tests.a

$(top_builddir)/src/libppl.la:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/src libppl.la

$(top_builddir)/Watchdog/src/libpwl.la:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/Watchdog/src libpwl.la
