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

AM_CPPFLAGS = \
$(TEST_CPPFLAGS) \
-I$(top_builddir)/src \
-I$(top_srcdir)/src \
-I$(top_srcdir)/tests \
-I$(top_srcdir)/utils \
@extra_includes@

LDADD = \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
$(top_builddir)/src/libppl.la \
@extra_libraries@

check_PROGRAMS = \
	addconstraints1 \
	addspacedims1 \
	affinedimension1 \
	affineimage1 affineimage2 \
	affinepreimage1 affinepreimage2 \
	ascii_dump_load1 \
	bdsdifference1 \
	bdshull1 \
	bgp99extrapolation1 \
	bhmz05widening1 \
	bhz03widening1 \
	cc76narrowing1 \
	cc76extrapolation1 \
	closure1 \
	concatenate1 \
	constraints1 \
	contains1 \
	empty1 \
	equality1 \
	exceptions1 exceptions2 exceptions3 \
	fromgensys1 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 \
	generalizedaffinepreimage1 generalizedaffinepreimage2 \
	geomcovers1 \
	h79widening1 \
	intersection1 \
	limitedbhmz05extrapolation1 \
	limitedcc76extrapolation1 \
	limitedh79extrapolation1 \
	mapspacedims1 \
	maxspacedim \
	minconstraints1 \
	relations1 relations2 \
	removespacedims1 \
	timeelapse1 \
	universe1 \
	writebdshape1

# This will be overriden by the `run_tests' script.
TEST_CPPFLAGS=-DBD_SHAPE_INSTANCE=mpq_class

dist_check_SCRIPTS = run_tests

TESTS = $(dist_check_SCRIPTS)

XFAIL_TESTS =

print_check_PROGRAMS:
	echo $(check_PROGRAMS)

#
# Sources for the tests
#

addconstraints1_SOURCES = addconstraints1.cc

addspacedims1_SOURCES = addspacedims1.cc

affinedimension1_SOURCES = affinedimension1.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc

affinepreimage1_SOURCES = affinepreimage1.cc
affinepreimage2_SOURCES = affinepreimage2.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc

bdsdifference1_SOURCES = bdsdifference1.cc

bdshull1_SOURCES = bdshull1.cc

bgp99extrapolation1_SOURCES = bgp99extrapolation1.cc

bhmz05widening1_SOURCES = bhmz05widening1.cc

bhz03widening1_SOURCES = bhz03widening1.cc

cc76narrowing1_SOURCES = cc76narrowing1.cc

cc76extrapolation1_SOURCES = cc76extrapolation1.cc

closure1_SOURCES = closure1.cc

concatenate1_SOURCES = concatenate1.cc

constraints1_SOURCES = constraints1.cc

contains1_SOURCES = contains1.cc

empty1_SOURCES = empty1.cc

equality1_SOURCES = equality1.cc

exceptions1_SOURCES = exceptions1.cc
exceptions2_SOURCES = exceptions2.cc
exceptions3_SOURCES = exceptions3.cc

fromgensys1_SOURCES = fromgensys1.cc

generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc
generalizedaffineimage3_SOURCES = generalizedaffineimage3.cc

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

geomcovers1_SOURCES = geomcovers1.cc

h79widening1_SOURCES = h79widening1.cc

intersection1_SOURCES = intersection1.cc

limitedbhmz05extrapolation1_SOURCES = limitedbhmz05extrapolation1.cc

limitedcc76extrapolation1_SOURCES = limitedcc76extrapolation1.cc

limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

maxspacedim_SOURCES = maxspacedim.cc

minconstraints1_SOURCES = minconstraints1.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc

removespacedims1_SOURCES = removespacedims1.cc

timeelapse1_SOURCES = timeelapse1.cc

universe1_SOURCES = universe1.cc

writebdshape1_SOURCES = writebdshape1.cc


MOSTLYCLEANFILES = \
ascii_dump_load1.dat
