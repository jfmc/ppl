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
-I$(top_srcdir)/utils \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

ORIGINAL_TESTS = \
	addconstraints1 \
	addspacedims1  addspacedims2 addspacedims3 addspacedims4 \
	addspacedims5 addspacedims6 addspacedims7 \
	affineimage1 affineimage2 affineimage3 affineimage4 affineimage5 \
	affineimage6 affineimage7 affineimage8 affineimage9 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinepreimage5 \
	ascii_dump_load1 ascii_dump_load2 \
	cc76narrowing1 cc76narrowing2 cc76narrowing3 cc76narrowing4 \
	cc76narrowing5 \
	cc76extrapolation1 cc76extrapolation2 cc76extrapolation3 \
	cc76extrapolation4 cc76extrapolation5 cc76extrapolation6 \
	ch78widening1 ch78widening2 ch78widening3 ch78widening4 \
	ch78widening5 ch78widening6 ch78widening7 ch78widening8 \
	concatenate1 concatenate2 concatenate3 concatenate4 concatenate5 \
	constraints1 constraints2 \
	contains1 contains2 contains3 \
	empty1 empty2 empty3 empty4 empty5 \
	equality1 \
	exceptions1 exceptions2 exceptions3 \
	fromgensys1 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffineimage5 generalizedaffineimage6 \
	generalizedaffineimage7 generalizedaffineimage8 \
	generalizedaffineimage9 generalizedaffineimage10 \
	generalizedaffineimage11 generalizedaffineimage12 \
	generalizedaffineimage13 \
	h79widening1 \
	intersection1 intersection2 intersection3 intersection4 \
	intersection5 intersection6 intersection7 intersection8 \
	limitedCC76extrapolation1 limitedCC76extrapolation2 \
	limitedCC76extrapolation3 \
	limitedCH78extrapolation1 limitedCH78extrapolation2 \
	limitedCH78extrapolation3 limitedCH78extrapolation4 \
	limitedCH78extrapolation5 limitedCH78extrapolation6 \
	limitedCH78extrapolation7 limitedCH78extrapolation8 \
	limitedCH78extrapolation9 \
	limitedh79extrapolation1 \
	mapspacedims1 mapspacedims2 mapspacedims3 mapspacedims4 \
	maxspacedim \
	polydifference1 polydifference2 polydifference3 polydifference4 \
	polydifference5 polydifference6 \
	polyhull1 polyhull2 polyhull3 polyhull4 \
	relations1 relations2 relations3 relations4 relations5 \
	removespacedims1 removespacedims2 removespacedims3 \
	timeelapse1 timeelapse2 timeelapse3 timeelapse4 timeelapse5 \
	universe1 \
	writebdiffs1 writebdiffs2

#
# Sources for the tests
#

addconstraints1_SOURCES = addconstraints1.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc 
addspacedims3_SOURCES = addspacedims3.cc 
addspacedims4_SOURCES = addspacedims4.cc 
addspacedims5_SOURCES = addspacedims5.cc 
addspacedims6_SOURCES = addspacedims6.cc
addspacedims7_SOURCES = addspacedims7.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc
affineimage3_SOURCES = affineimage3.cc
affineimage4_SOURCES = affineimage4.cc
affineimage5_SOURCES = affineimage5.cc
affineimage6_SOURCES = affineimage6.cc
affineimage7_SOURCES = affineimage7.cc
affineimage8_SOURCES = affineimage8.cc
affineimage9_SOURCES = affineimage9.cc

affinepreimage1_SOURCES = affinepreimage1.cc
affinepreimage2_SOURCES = affinepreimage2.cc
affinepreimage3_SOURCES = affinepreimage3.cc
affinepreimage4_SOURCES = affinepreimage4.cc
affinepreimage5_SOURCES = affinepreimage5.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc
ascii_dump_load2_SOURCES = ascii_dump_load2.cc

cc76narrowing1_SOURCES = cc76narrowing1.cc
cc76narrowing2_SOURCES = cc76narrowing2.cc
cc76narrowing3_SOURCES = cc76narrowing3.cc
cc76narrowing4_SOURCES = cc76narrowing4.cc
cc76narrowing5_SOURCES = cc76narrowing5.cc

cc76extrapolation1_SOURCES = cc76extrapolation1.cc
cc76extrapolation2_SOURCES = cc76extrapolation2.cc
cc76extrapolation3_SOURCES = cc76extrapolation3.cc
cc76extrapolation4_SOURCES = cc76extrapolation4.cc
cc76extrapolation5_SOURCES = cc76extrapolation5.cc
cc76extrapolation6_SOURCES = cc76extrapolation6.cc

ch78widening1_SOURCES = ch78widening1.cc
ch78widening2_SOURCES = ch78widening2.cc
ch78widening3_SOURCES = ch78widening3.cc
ch78widening4_SOURCES = ch78widening4.cc
ch78widening5_SOURCES = ch78widening5.cc
ch78widening6_SOURCES = ch78widening6.cc
ch78widening7_SOURCES = ch78widening7.cc
ch78widening8_SOURCES = ch78widening8.cc

concatenate1_SOURCES = concatenate1.cc
concatenate2_SOURCES = concatenate2.cc
concatenate3_SOURCES = concatenate3.cc
concatenate4_SOURCES = concatenate4.cc
concatenate5_SOURCES = concatenate5.cc

constraints1_SOURCES = constraints1.cc
constraints2_SOURCES = constraints2.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc
contains3_SOURCES = contains3.cc

empty1_SOURCES = empty1.cc
empty2_SOURCES = empty2.cc
empty3_SOURCES = empty3.cc
empty4_SOURCES = empty4.cc
empty5_SOURCES = empty5.cc

equality1_SOURCES = equality1.cc

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
generalizedaffineimage10_SOURCES = generalizedaffineimage10.cc
generalizedaffineimage11_SOURCES = generalizedaffineimage11.cc
generalizedaffineimage12_SOURCES = generalizedaffineimage12.cc
generalizedaffineimage13_SOURCES = generalizedaffineimage13.cc

h79widening1_SOURCES = h79widening1.cc

intersection1_SOURCES = intersection1.cc
intersection2_SOURCES = intersection2.cc
intersection3_SOURCES = intersection3.cc
intersection4_SOURCES = intersection4.cc
intersection5_SOURCES = intersection5.cc
intersection6_SOURCES = intersection6.cc
intersection7_SOURCES = intersection7.cc
intersection8_SOURCES = intersection8.cc

limitedCC76extrapolation1_SOURCES = limitedCC76extrapolation1.cc
limitedCC76extrapolation2_SOURCES = limitedCC76extrapolation2.cc
limitedCC76extrapolation3_SOURCES = limitedCC76extrapolation3.cc

limitedCH78extrapolation1_SOURCES = limitedCH78extrapolation1.cc
limitedCH78extrapolation2_SOURCES = limitedCH78extrapolation2.cc
limitedCH78extrapolation3_SOURCES = limitedCH78extrapolation3.cc
limitedCH78extrapolation4_SOURCES = limitedCH78extrapolation4.cc
limitedCH78extrapolation5_SOURCES = limitedCH78extrapolation5.cc
limitedCH78extrapolation6_SOURCES = limitedCH78extrapolation6.cc
limitedCH78extrapolation7_SOURCES = limitedCH78extrapolation7.cc
limitedCH78extrapolation8_SOURCES = limitedCH78extrapolation8.cc
limitedCH78extrapolation9_SOURCES = limitedCH78extrapolation9.cc

limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc

mapspacedims1_SOURCES = mapspacedims1.cc
mapspacedims2_SOURCES = mapspacedims2.cc
mapspacedims3_SOURCES = mapspacedims3.cc
mapspacedims4_SOURCES = mapspacedims4.cc

maxspacedim_SOURCES = maxspacedim.cc

polydifference1_SOURCES = polydifference1.cc
polydifference2_SOURCES = polydifference2.cc
polydifference3_SOURCES = polydifference3.cc
polydifference4_SOURCES = polydifference4.cc
polydifference5_SOURCES = polydifference5.cc
polydifference6_SOURCES = polydifference6.cc

polyhull1_SOURCES = polyhull1.cc
polyhull2_SOURCES = polyhull2.cc
polyhull3_SOURCES = polyhull3.cc
polyhull4_SOURCES = polyhull4.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc
relations3_SOURCES = relations3.cc
relations4_SOURCES = relations4.cc
relations5_SOURCES = relations5.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc
removespacedims3_SOURCES = removespacedims3.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc
timeelapse3_SOURCES = timeelapse3.cc
timeelapse4_SOURCES = timeelapse4.cc
timeelapse5_SOURCES = timeelapse5.cc

universe1_SOURCES = universe1.cc

writebdiffs1_SOURCES = writebdiffs1.cc
writebdiffs2_SOURCES = writebdiffs2.cc

TESTS = $(ORIGINAL_TESTS)

XFAIL_TESTS =

check_PROGRAMS = $(TESTS) $(BUGS)

BUGS =

MOSTLYCLEANFILES = \
ascii_dump_load1.dat \
ascii_dump_load2.dat
