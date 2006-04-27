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
-I$(top_srcdir)/src -I$(top_srcdir)/tests \
-I$(top_builddir)/src \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

ORIGINAL_TESTS = \
	addspacedims1 \
	affinedimension1 \
	affineimage1 affineimage2 \
	affinepreimage1 affinepreimage2 \
	ascii_dump_load1 \
	bhz03widening1 \
	bhmz05widening1 \
	cc76extrapolation1 \
	cc76narrowing1 \
	chinainit \
	concatenate1 \
	constraints1 \
	contains1 \
	empty1 \
	fromgensys1 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffinepreimage1 generalizedaffinepreimage2 \
	intersection1 \
	limitedbhmz05extrapolation1 \
	limitedcc76extrapolation1 \
	mapspacedims1 \
	maxspacedim \
	minconstraints1 \
	octdifference1 \
	octhull1 \
	relatwithcons1 relatwithcons2 \
	relatwithgen1 \
	removespacedims1 \
	timeelapse1 \
	universe1 \
	writeoctagon1 

#
# Sources for the tests
#

addspacedims1_SOURCES = addspacedims1.cc

affinedimension1_SOURCES = affinedimension1.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc

affinepreimage1_SOURCES = affinepreimage1.cc
affinepreimage2_SOURCES = affinepreimage2.cc
affinepreimage3_SOURCES = affinepreimage3.cc
affinepreimage4_SOURCES = affinepreimage4.cc
affinepreimage5_SOURCES = affinepreimage5.cc
affinepreimage6_SOURCES = affinepreimage6.cc
affinepreimage7_SOURCES = affinepreimage7.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc
ascii_dump_load2_SOURCES = ascii_dump_load2.cc
ascii_dump_load3_SOURCES = ascii_dump_load3.cc
ascii_dump_load4_SOURCES = ascii_dump_load4.cc

bhz03widening1_SOURCES = bhz03widening1.cc

bhmz05widening1_SOURCES = bhmz05widening1.cc

cc76extrapolation1_SOURCES = cc76extrapolation1.cc

cc76narrowing1_SOURCES = cc76narrowing1.cc

chinainit_SOURCES = chinainit.cc

concatenate1_SOURCES = concatenate1.cc

constraints1_SOURCES = constraints1.cc

contains1_SOURCES = contains1.cc

empty1_SOURCES = empty1.cc

exceptions1_SOURCES = exceptions1.cc
exceptions2_SOURCES = exceptions2.cc
exceptions3_SOURCES = exceptions3.cc

fromgensys1_SOURCES = fromgensys1.cc

generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc
generalizedaffineimage3_SOURCES = generalizedaffineimage3.cc
generalizedaffineimage4_SOURCES = generalizedaffineimage4.cc
generalizedaffineimage5_SOURCES = generalizedaffineimage5.cc
generalizedaffineimage6_SOURCES = generalizedaffineimage6.cc
generalizedaffineimage7_SOURCES = generalizedaffineimage7.cc
generalizedaffineimage8_SOURCES = generalizedaffineimage8.cc
generalizedaffineimage9_SOURCES = generalizedaffineimage9.cc

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

intersection1_SOURCES = intersection1.cc

limitedbhmz05extrapolation1_SOURCES = limitedbhmz05extrapolation1.cc

limitedcc76extrapolation1_SOURCES = limitedcc76extrapolation1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

maxspacedim_SOURCES = maxspacedim.cc

minconstraints1_SOURCES = minconstraints1.cc

octdifference1_SOURCES = octdifference1.cc

octhull1_SOURCES = octhull1.cc

relatwithcons1_SOURCES = relatwithcons1.cc
relatwithcons2_SOURCES = relatwithcons2.cc

relatwithgen1_SOURCES = relatwithgen1.cc

removespacedims1_SOURCES = removespacedims1.cc

timeelapse1_SOURCES = timeelapse1.cc

universe1_SOURCES = universe1.cc

writeoctagon1_SOURCES = writeoctagon1.cc

TESTS = $(ORIGINAL_TESTS)

XFAIL_TESTS =

check_PROGRAMS = $(TESTS) $(BUGS)

BUGS =

MOSTLYCLEANFILES =
