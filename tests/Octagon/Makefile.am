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
	affineimage1 affineimage2 affineimage3 affineimage4 \
	affineimage5 affineimage6 affineimage7 affineimage8 \
	affineimage9 affineimage10 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinepreimage5 affinepreimage6 affinepreimage7 \
	ascii_dump_load1 ascii_dump_load2 ascii_dump_load3 ascii_dump_load4 \
	cc76extrapolation1 cc76extrapolation2 cc76extrapolation3 \
	cc76extrapolation4 cc76extrapolation5 \
	cc76narrowing1 cc76narrowing2 \
	ch78widening1 ch78widening2 ch78widening3 ch78widening4 \
	ch78widening5 ch78widening6 ch78widening7 ch78widening8 \
	ch78widening9 ch78widening10 ch78widening11 ch78widening12 \
	ch78widening13 \
	chinainit \
	concatenate1 \
	constraints1 constraints2 constraints3 constraints4 constraints5 \
	contains1 contains2 contains3 contains4 contains5 contains6 \
	contains7 contains8 \
	empty1 empty2 empty3 \
	exceptions1 exceptions2 exceptions3 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffineimage5 generalizedaffineimage6 \
	generalizedaffineimage7 generalizedaffineimage8 \
	generalizedaffineimage9 \
	intersection1 intersection2 intersection3 intersection4 intersection5 \
	intersection6 intersection7 \
	limitedcc76extrapolation1 limitedcc76extrapolation2 \
	limitedcc76extrapolation3 \
	limitedch78extrapolation1 limitedch78extrapolation2 \
	limitedch78extrapolation3 limitedch78extrapolation4 \
	limitedch78extrapolation5 limitedch78extrapolation6 \
	mapspacedims1 \
	maxspacedim \
	octdifference1 octdifference2 octdifference3 \
	polyhull1 polyhull2 polyhull3 polyhull4 polyhull5 polyhull6 \
	polyhull7 polyhull8 polyhull9 polyhull10 \
	relations1 relations2 relations3 relations4 relations5 \
	relations6 \
	removespacedims1 removespacedims2 removespacedims3 \
	removespacedims4 removespacedims5 \
	timeelapse1 timeelapse2 timeelapse3 timeelapse4 timeelapse5 \
	timeelapse6 timeelapse7 timeelapse8 timeelapse9 \
	universe1 \
	writeoctagon1 writeoctagon2 writeoctagon3

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

cc76extrapolation1_SOURCES = cc76extrapolation1.cc
cc76extrapolation2_SOURCES = cc76extrapolation2.cc
cc76extrapolation3_SOURCES = cc76extrapolation3.cc
cc76extrapolation4_SOURCES = cc76extrapolation4.cc
cc76extrapolation5_SOURCES = cc76extrapolation5.cc

cc76narrowing1_SOURCES = cc76narrowing1.cc
cc76narrowing2_SOURCES = cc76narrowing2.cc

ch78widening1_SOURCES = ch78widening1.cc
ch78widening2_SOURCES = ch78widening2.cc
ch78widening3_SOURCES = ch78widening3.cc
ch78widening4_SOURCES = ch78widening4.cc
ch78widening5_SOURCES = ch78widening5.cc
ch78widening6_SOURCES = ch78widening6.cc
ch78widening7_SOURCES = ch78widening7.cc
ch78widening8_SOURCES = ch78widening8.cc
ch78widening9_SOURCES = ch78widening9.cc
ch78widening10_SOURCES = ch78widening10.cc
ch78widening11_SOURCES = ch78widening11.cc
ch78widening12_SOURCES = ch78widening12.cc
ch78widening13_SOURCES = ch78widening13.cc

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
intersection2_SOURCES = intersection2.cc
intersection3_SOURCES = intersection3.cc
intersection4_SOURCES = intersection4.cc
intersection5_SOURCES = intersection5.cc
intersection6_SOURCES = intersection6.cc
intersection7_SOURCES = intersection7.cc

limitedcc76extrapolation1_SOURCES = limitedcc76extrapolation1.cc
limitedcc76extrapolation2_SOURCES = limitedcc76extrapolation2.cc
limitedcc76extrapolation3_SOURCES = limitedcc76extrapolation3.cc

limitedch78extrapolation1_SOURCES = limitedch78extrapolation1.cc
limitedch78extrapolation2_SOURCES = limitedch78extrapolation2.cc
limitedch78extrapolation3_SOURCES = limitedch78extrapolation3.cc
limitedch78extrapolation4_SOURCES = limitedch78extrapolation4.cc
limitedch78extrapolation5_SOURCES = limitedch78extrapolation5.cc
limitedch78extrapolation6_SOURCES = limitedch78extrapolation6.cc

mapspacedims1_SOURCES = mapspacedims1.cc

maxspacedim_SOURCES = maxspacedim.cc

octdifference1_SOURCES = octdifference1.cc
octdifference2_SOURCES = octdifference2.cc
octdifference3_SOURCES = octdifference3.cc

polyhull1_SOURCES = polyhull1.cc
polyhull2_SOURCES = polyhull2.cc
polyhull3_SOURCES = polyhull3.cc
polyhull4_SOURCES = polyhull4.cc
polyhull5_SOURCES = polyhull5.cc
polyhull6_SOURCES = polyhull6.cc
polyhull7_SOURCES = polyhull7.cc
polyhull8_SOURCES = polyhull8.cc
polyhull9_SOURCES = polyhull9.cc
polyhull10_SOURCES = polyhull10.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc
relations3_SOURCES = relations3.cc
relations4_SOURCES = relations4.cc
relations5_SOURCES = relations5.cc
relations6_SOURCES = relations6.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc
removespacedims3_SOURCES = removespacedims3.cc
removespacedims4_SOURCES = removespacedims4.cc
removespacedims5_SOURCES = removespacedims5.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc
timeelapse3_SOURCES = timeelapse3.cc
timeelapse4_SOURCES = timeelapse4.cc
timeelapse5_SOURCES = timeelapse5.cc
timeelapse6_SOURCES = timeelapse6.cc
timeelapse7_SOURCES = timeelapse7.cc
timeelapse8_SOURCES = timeelapse8.cc
timeelapse9_SOURCES = timeelapse9.cc

universe1_SOURCES = universe1.cc

writeoctagon1_SOURCES = writeoctagon1.cc
writeoctagon2_SOURCES = writeoctagon2.cc
writeoctagon3_SOURCES = writeoctagon3.cc

TESTS = $(ORIGINAL_TESTS)

XFAIL_TESTS =

check_PROGRAMS = $(TESTS) $(BUGS)

BUGS =

MOSTLYCLEANFILES =
