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
	addspacedims1 addspacedims2 addspacedims3 addspacedims4 \
	addspacedims5 addspacedims6 addspacedims7 addspacedims8 \
	affinedimension1 \
	affineimage1 affineimage2 affineimage3 affineimage4 \
	affineimage5 affineimage6 affineimage7 affineimage8 \
	affineimage9 affineimage10 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinepreimage5 affinepreimage6 affinepreimage7 \
	ascii_dump_load1 ascii_dump_load2 ascii_dump_load3 ascii_dump_load4 \
	bhz03widening1 \
	bhmz05widening1 \
	cc76extrapolation1 \
	cc76narrowing1 \
	chinainit \
	concatenate1 \
	constraints1 constraints2 constraints3 constraints4 constraints5 \
	contains1 contains2 contains3 contains4 contains5 contains6 \
	contains7 contains8 \
	empty1 empty2 empty3 \
	exceptions1 exceptions2 exceptions3 \
	fromgensys1 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffineimage5 generalizedaffineimage6 \
	generalizedaffineimage7 generalizedaffineimage8 \
	generalizedaffineimage9 \
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
addspacedims2_SOURCES = addspacedims2.cc
addspacedims3_SOURCES = addspacedims3.cc
addspacedims4_SOURCES = addspacedims4.cc
addspacedims5_SOURCES = addspacedims5.cc
addspacedims6_SOURCES = addspacedims6.cc
addspacedims7_SOURCES = addspacedims7.cc
addspacedims8_SOURCES = addspacedims8.cc

affinedimension1_SOURCES = affinedimension1.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc
affineimage3_SOURCES = affineimage3.cc
affineimage4_SOURCES = affineimage4.cc
affineimage5_SOURCES = affineimage5.cc
affineimage6_SOURCES = affineimage6.cc
affineimage7_SOURCES = affineimage7.cc
affineimage8_SOURCES = affineimage8.cc
affineimage9_SOURCES = affineimage9.cc
affineimage10_SOURCES = affineimage10.cc

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
constraints2_SOURCES = constraints2.cc
constraints3_SOURCES = constraints3.cc
constraints4_SOURCES = constraints4.cc
constraints5_SOURCES = constraints5.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc
contains3_SOURCES = contains3.cc
contains4_SOURCES = contains4.cc
contains5_SOURCES = contains5.cc
contains6_SOURCES = contains6.cc
contains7_SOURCES = contains7.cc
contains8_SOURCES = contains8.cc

empty1_SOURCES = empty1.cc
empty2_SOURCES = empty2.cc
empty3_SOURCES = empty3.cc

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
