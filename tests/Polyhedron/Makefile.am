# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>
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

CHECKER =
#CHECKER = valgrind --tool=memcheck -q --trace-children=yes --leak-check=yes --num-callers=100 --leak-resolution=high --suppressions=$(srcdir)/valgrind_suppressions

TESTS_ENVIRONMENT = $(CHECKER)

AM_CPPFLAGS = \
-I$(top_builddir)/src \
-I$(top_srcdir)/utils \
-I$(top_srcdir)/tests \
-I$(top_builddir)/Watchdog \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

ORIGINAL_TESTS = \
	CbecomesNNC1 \
	NNCbecomesC1 \
	NNCminimize1 NNCminimize2 NNCminimize3 NNCminimize4 NNCminimize5 \
        NNCminimize6 \
	addconstraint1 addconstraint2 addconstraint3 \
        addconstraints1 addconstraints2 addconstraints3 addconstraints4 \
	addconstraints5 addconstraints6 addconstraints7 addconstraints8 \
	addconstraints9 addconstraints10 addconstraints11 \
	addconstraints12 addconstraints13 \
	addgenerator1 addgenerator2 addgenerator3 addgenerator4 \
	addgenerator5 \
	addgenerators1 addgenerators2 addgenerators3 addgenerators4 \
	addgenerators5 addgenerators6 addgenerators7 addgenerators8 \
	addgenerators9 addgenerators10 addgenerators11 addgenerators12 \
	addgenerators13 \
	addspacedims1 addspacedims2 addspacedims3 addspacedims4 \
	addspacedims5 addspacedims6 addspacedims7 addspacedims8 \
	addspacedims9 addspacedims10 addspacedims11 addspacedims12 \
	addspacedims13 \
	affineimage1 affineimage2 affineimage3 affineimage4 affineimage5 \
	affineimage6 affineimage7 affineimage8 affineimage9 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinepreimage5 affinepreimage6 affinepreimage7 affinepreimage8 \
	affinepreimage9 affinepreimage10 \
	affinetrans \
        append1 append2 \
	ascii_dump_load1 ascii_dump_load2 ascii_dump_load3 ascii_dump_load4 \
	ascii_dump_load5 ascii_dump_load6 ascii_dump_load7 \
	bgp99extrapolation1 bgp99extrapolation2 \
	bhrz03widening1 bhrz03widening2 bhrz03widening3 bhrz03widening4 \
	bhrz03widening5 bhrz03widening6 bhrz03widening7 bhrz03widening8 \
	bhrz03widening9 bhrz03widening10 bhrz03widening11 bhrz03widening12 \
	bhrz03widening13 bhrz03widening14 bhrz03widening15 bhrz03widening16 \
	bhrz03widening17 bhrz03widening18 bhrz03widening19 \
	bhz03widening1 bhz03widening2 bhz03widening3 bhz03widening4 \
	bhz03widening5 bhz03widening6 \
	bounded1 \
	boundedh79extrapolation1 \
	boundingbox1 boundingbox2 boundingbox3 boundingbox4 boundingbox5 \
	bounds1 bounds2 \
	concatenate1 concatenate2 concatenate3 concatenate4 concatenate5 \
	concatenate6 \
	constraints1 constraints2 constraints3 constraints4 \
	contains1 contains2 contains3 contains4 contains5 \
	disjoint1 disjoint2 disjoint3 \
	dualhypercubes \
	empty1 \
	equals1 \
	exceptions1 exceptions2  exceptions3 \
	expandspacedim1 expandspacedim2 \
	foldspacedims1 foldspacedims2 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffineimage5 generalizedaffineimage6 \
	generalizedaffineimage7 generalizedaffineimage8 \
	generalizedaffineimage9 generalizedaffineimage10 \
	generalizedaffineimage11 generalizedaffineimage12 \
	generators1 generators2 generators3 generators4 generators5 \
	generators6 \
	geomcovers1 \
	h79widening1 h79widening2 h79widening3 h79widening4 h79widening5 \
	h79widening6 h79widening7 h79widening8 \
        intersection1 intersection2 intersection3 intersection4 \
	intersection5 intersection6 intersection7 intersection8 \
	intersection9 intersection10 intersection11 \
	limitedbhrz03extrapolation1 \
	limitedh79extrapolation1 limitedh79extrapolation2 \
	limitedh79extrapolation3 limitedh79extrapolation4 \
	linearpartition1 linearpartition2 \
	linearpartition3 linearpartition4 \
	linexpression1 \
	mapspacedims1 mapspacedims2 mapspacedims3  mapspacedims4 \
	max_min1 max_min2 \
	maxspacedim1 \
	mc91 \
	membytes1 membytes2 \
	memory1 \
	minconstraints1 minconstraints2 minconstraints3 minconstraints4 \
	mingenerators1 mingenerators2 mingenerators3 \
	onepoint \
        permute \
	polydifference1 polydifference2 polydifference3 polydifference4 \
	polydifference5 polydifference6 polydifference7 polydifference8 \
	polydifference9 \
	polyhull1 polyhull2 polyhull3 polyhull4 polyhull5 polyhull6 \
	polyhull7 polyhull8 polyhull9 polyhull10 polyhull11 \
	randphull1 randphull2 \
	relations1 relations2 relations3 relations4 relations5 relations6 \
	relations7 relations8 relations9 relations10 relations11 \
	relations12 relations13 relations14 relations15 relations16 \
	relations17 relations18 relations19 \
	removespacedims1 removespacedims2 removespacedims3 removespacedims4 \
	removespacedims5 removespacedims6 removespacedims7 removespacedims8 \
	removespacedims9 removespacedims10 \
	smm1 \
	timeelapse1 timeelapse2 timeelapse3 timeelapse4 timeelapse5 \
	timeelapse6 timeelapse7 timeelapse8 \
	topclosed1 topclosed2 topclosed3 \
	topclosure1 topclosure2 topclosure3 topclosure4 \
	universe1 universe2 universe3 universe4 universe5 universe6 universe7 \
	writeconsys1 \
	writegensys1 writegensys2 writegensys3 \
	writepolyhedron1 writepolyhedron2 writepolyhedron3 \
	writerelation1 \
	writevariable1

DERIVED_TESTS = \
	nnc_addconstraint2 nnc_addconstraint3 \
	nnc_addconstraints1 nnc_addconstraints2 nnc_addconstraints3 \
	nnc_addconstraints4 nnc_addconstraints5 nnc_addconstraints6 \
	nnc_addconstraints7 nnc_addconstraints8 nnc_addconstraints9 \
	nnc_addconstraints10 nnc_addconstraints13 \
	nnc_addgenerator3 nnc_addgenerator5 \
	nnc_addgenerators1 nnc_addgenerators2 nnc_addgenerators3 \
	nnc_addgenerators4 nnc_addgenerators5 nnc_addgenerators6 \
	nnc_addgenerators9 nnc_addgenerators10 nnc_addgenerators12 \
	nnc_addgenerators13 \
	nnc_addspacedims1 nnc_addspacedims2 nnc_addspacedims3 \
	nnc_addspacedims4 nnc_addspacedims9 nnc_addspacedims12 \
	nnc_affineimage1 nnc_affineimage2 nnc_affineimage3 nnc_affineimage4 \
	nnc_affineimage5 nnc_affineimage6 nnc_affineimage7 nnc_affineimage8 \
	nnc_affinepreimage1 nnc_affinepreimage2 nnc_affinepreimage3 \
	nnc_affinepreimage4 nnc_affinepreimage5 nnc_affinepreimage6 \
	nnc_affinepreimage7 nnc_affinepreimage8 nnc_affinepreimage9 \
	nnc_affinepreimage10 \
	nnc_affinetrans \
	nnc_append1 nnc_append2 \
	nnc_ascii_dump_load1 nnc_ascii_dump_load2 nnc_ascii_dump_load3 \
	nnc_ascii_dump_load6 nnc_ascii_dump_load7 \
	nnc_bgp99extrapolation1 nnc_bgp99extrapolation2 \
	nnc_bhrz03widening1 nnc_bhrz03widening2 nnc_bhrz03widening3 \
	nnc_bhrz03widening5 nnc_bhrz03widening6 \
	nnc_bhrz03widening8 nnc_bhrz03widening9 \
	nnc_bhrz03widening10 nnc_bhrz03widening11 nnc_bhrz03widening12 \
	nnc_bounded1 \
	nnc_boundedh79extrapolation1 \
	nnc_boundingbox1 nnc_boundingbox3 nnc_boundingbox5 \
	nnc_bounds1 nnc_bounds2 \
	nnc_concatenate1 nnc_concatenate2 nnc_concatenate3 nnc_concatenate4 \
	nnc_concatenate5 nnc_concatenate6 \
	nnc_constraints1 nnc_constraints2 nnc_constraints3 nnc_constraints4 \
	nnc_contains1 nnc_contains2 \
	nnc_disjoint1 nnc_disjoint2 \
	nnc_empty1 \
	nnc_equals1 \
	nnc_exceptions1 nnc_exceptions3 \
	nnc_expandspacedim1 \
	nnc_foldspacedims1 \
	nnc_generalizedaffineimage1 nnc_generalizedaffineimage2 \
	nnc_generalizedaffineimage3 \
	nnc_generalizedaffineimage5 nnc_generalizedaffineimage6 \
	nnc_generalizedaffineimage11 \
	nnc_generators1 nnc_generators2 nnc_generators3 nnc_generators4 \
	nnc_generators5 nnc_generators6 \
	nnc_geomcovers1 \
	nnc_h79widening1 nnc_h79widening2 nnc_h79widening4 nnc_h79widening5 \
        nnc_h79widening6 nnc_h79widening7 nnc_h79widening8 \
	nnc_intersection1 nnc_intersection2 nnc_intersection3 \
	nnc_intersection4 nnc_intersection5 nnc_intersection6 \
	nnc_intersection7 nnc_intersection8 nnc_intersection9 \
	nnc_intersection10 nnc_intersection11 \
	nnc_limitedbhrz03extrapolation1 \
	nnc_limitedh79extrapolation1 nnc_limitedh79extrapolation2 \
	nnc_limitedh79extrapolation3 nnc_limitedh79extrapolation4 \
	nnc_linearpartition1 nnc_linearpartition2 \
	nnc_linearpartition3 nnc_linearpartition4 \
	nnc_mapspacedims1 nnc_mapspacedims2 nnc_mapspacedims3 \
	nnc_mapspacedims4 \
	nnc_max_min1 nnc_max_min2 \
	nnc_mc91 \
	nnc_membytes1 nnc_membytes2 \
	nnc_minconstraints1 nnc_minconstraints2 \
	nnc_mingenerators1 nnc_mingenerators2 \
	nnc_onepoint \
	nnc_permute \
	nnc_polydifference1 nnc_polydifference3 nnc_polydifference4 \
	nnc_polydifference6 nnc_polydifference7 nnc_polydifference9 \
	nnc_polyhull1 nnc_polyhull2 nnc_polyhull3 nnc_polyhull4 \
	nnc_polyhull5 nnc_polyhull6 nnc_polyhull7 nnc_polyhull8 \
	nnc_polyhull9 nnc_polyhull10 \
	nnc_randphull1 nnc_randphull2 \
	nnc_relations1 nnc_relations2 nnc_relations3 nnc_relations4 \
	nnc_relations5 nnc_relations6 nnc_relations7 nnc_relations8 \
	nnc_relations9 nnc_relations10 nnc_relations12 nnc_relations13 \
	nnc_relations14 nnc_relations15 nnc_relations16 nnc_relations19 \
	nnc_removespacedims1 nnc_removespacedims2 nnc_removespacedims3 \
	nnc_removespacedims4 nnc_removespacedims5 nnc_removespacedims6 \
	nnc_removespacedims8 nnc_removespacedims9 nnc_removespacedims10 \
	nnc_smm1 \
	nnc_timeelapse2 nnc_timeelapse5 nnc_timeelapse6 nnc_timeelapse7 \
	nnc_timeelapse8 \
	nnc_universe1 nnc_universe2 nnc_universe3 nnc_universe4 \
	nnc_universe5 nnc_universe7 \
	nnc_writepolyhedron2 nnc_writepolyhedron3

#
# Sources for the tests
#

CbecomesNNC1_SOURCES = CbecomesNNC1.cc

NNCbecomesC1_SOURCES = NNCbecomesC1.cc

NNCminimize1_SOURCES = NNCminimize1.cc
NNCminimize2_SOURCES = NNCminimize2.cc
NNCminimize3_SOURCES = NNCminimize3.cc
NNCminimize4_SOURCES = NNCminimize4.cc
NNCminimize5_SOURCES = NNCminimize5.cc
NNCminimize6_SOURCES = NNCminimize6.cc

addconstraint1_SOURCES = addconstraint1.cc
addconstraint2_SOURCES = addconstraint2.cc
addconstraint3_SOURCES = addconstraint3.cc

addconstraints1_SOURCES = addconstraints1.cc
addconstraints2_SOURCES = addconstraints2.cc
addconstraints3_SOURCES = addconstraints3.cc
addconstraints4_SOURCES = addconstraints4.cc
addconstraints5_SOURCES = addconstraints5.cc
addconstraints6_SOURCES = addconstraints6.cc
addconstraints7_SOURCES = addconstraints7.cc
addconstraints8_SOURCES = addconstraints8.cc
addconstraints9_SOURCES = addconstraints9.cc
addconstraints10_SOURCES = addconstraints10.cc
addconstraints11_SOURCES = addconstraints11.cc
addconstraints12_SOURCES = addconstraints12.cc
addconstraints13_SOURCES = addconstraints13.cc

addgenerator1_SOURCES = addgenerator1.cc
addgenerator2_SOURCES = addgenerator2.cc
addgenerator3_SOURCES = addgenerator3.cc
addgenerator4_SOURCES = addgenerator4.cc
addgenerator5_SOURCES = addgenerator5.cc

addgenerators1_SOURCES = addgenerators1.cc
addgenerators2_SOURCES = addgenerators2.cc
addgenerators3_SOURCES = addgenerators3.cc
addgenerators4_SOURCES = addgenerators4.cc
addgenerators5_SOURCES = addgenerators5.cc
addgenerators6_SOURCES = addgenerators6.cc
addgenerators7_SOURCES = addgenerators7.cc
addgenerators8_SOURCES = addgenerators8.cc
addgenerators9_SOURCES = addgenerators9.cc
addgenerators10_SOURCES = addgenerators10.cc
addgenerators11_SOURCES = addgenerators11.cc
addgenerators12_SOURCES = addgenerators12.cc
addgenerators13_SOURCES = addgenerators13.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc
addspacedims3_SOURCES = addspacedims3.cc
addspacedims4_SOURCES = addspacedims4.cc
addspacedims5_SOURCES = addspacedims5.cc
addspacedims6_SOURCES = addspacedims6.cc
addspacedims7_SOURCES = addspacedims7.cc
addspacedims8_SOURCES = addspacedims8.cc
addspacedims9_SOURCES = addspacedims9.cc
addspacedims10_SOURCES = addspacedims10.cc
addspacedims11_SOURCES = addspacedims11.cc
addspacedims12_SOURCES = addspacedims12.cc
addspacedims13_SOURCES = addspacedims13.cc

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
affinepreimage6_SOURCES = affinepreimage6.cc
affinepreimage7_SOURCES = affinepreimage7.cc
affinepreimage8_SOURCES = affinepreimage8.cc
affinepreimage9_SOURCES = affinepreimage9.cc
affinepreimage10_SOURCES = affinepreimage10.cc

affinetrans_SOURCES = affinetrans.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc
ascii_dump_load2_SOURCES = ascii_dump_load2.cc
ascii_dump_load3_SOURCES = ascii_dump_load3.cc
ascii_dump_load4_SOURCES = ascii_dump_load4.cc
ascii_dump_load5_SOURCES = ascii_dump_load5.cc
ascii_dump_load6_SOURCES = ascii_dump_load6.cc
ascii_dump_load7_SOURCES = ascii_dump_load7.cc

bgp99extrapolation1_SOURCES = bgp99extrapolation1.cc
bgp99extrapolation2_SOURCES = bgp99extrapolation2.cc

bhrz03widening1_SOURCES = bhrz03widening1.cc
bhrz03widening2_SOURCES = bhrz03widening2.cc
bhrz03widening3_SOURCES = bhrz03widening3.cc
bhrz03widening4_SOURCES = bhrz03widening4.cc
bhrz03widening5_SOURCES = bhrz03widening5.cc
bhrz03widening6_SOURCES = bhrz03widening6.cc
bhrz03widening7_SOURCES = bhrz03widening7.cc
bhrz03widening8_SOURCES = bhrz03widening8.cc
bhrz03widening9_SOURCES = bhrz03widening9.cc
bhrz03widening10_SOURCES = bhrz03widening10.cc
bhrz03widening11_SOURCES = bhrz03widening11.cc
bhrz03widening12_SOURCES = bhrz03widening12.cc
bhrz03widening13_SOURCES = bhrz03widening13.cc
bhrz03widening14_SOURCES = bhrz03widening14.cc
bhrz03widening15_SOURCES = bhrz03widening15.cc
bhrz03widening16_SOURCES = bhrz03widening16.cc
bhrz03widening17_SOURCES = bhrz03widening17.cc
bhrz03widening18_SOURCES = bhrz03widening18.cc
bhrz03widening19_SOURCES = bhrz03widening19.cc

bhz03widening1_SOURCES = bhz03widening1.cc
bhz03widening2_SOURCES = bhz03widening2.cc
bhz03widening3_SOURCES = bhz03widening3.cc
bhz03widening4_SOURCES = bhz03widening4.cc
bhz03widening5_SOURCES = bhz03widening5.cc
bhz03widening6_SOURCES = bhz03widening6.cc

bounded1_SOURCES = bounded1.cc

boundedh79extrapolation1_SOURCES = boundedh79extrapolation1.cc

boundingbox1_SOURCES = boundingbox1.cc BBox.cc
boundingbox2_SOURCES = boundingbox2.cc BBox.cc
boundingbox3_SOURCES = boundingbox3.cc
boundingbox4_SOURCES = boundingbox4.cc
boundingbox5_SOURCES = boundingbox5.cc BBox.cc

bounds1_SOURCES = bounds1.cc
bounds2_SOURCES = bounds2.cc

concatenate1_SOURCES = concatenate1.cc
concatenate2_SOURCES = concatenate2.cc
concatenate3_SOURCES = concatenate3.cc
concatenate4_SOURCES = concatenate4.cc
concatenate5_SOURCES = concatenate5.cc
concatenate6_SOURCES = concatenate6.cc

constraints1_SOURCES = constraints1.cc
constraints2_SOURCES = constraints2.cc
constraints3_SOURCES = constraints3.cc
constraints4_SOURCES = constraints4.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc
contains3_SOURCES = contains3.cc
contains4_SOURCES = contains4.cc
contains5_SOURCES = contains5.cc

disjoint1_SOURCES = disjoint1.cc
disjoint2_SOURCES = disjoint2.cc
disjoint3_SOURCES = disjoint3.cc

dualhypercubes_SOURCES = dualhypercubes.cc

empty1_SOURCES = empty1.cc

equals1_SOURCES = equals1.cc

exceptions1_SOURCES = exceptions1.cc
exceptions2_SOURCES = exceptions2.cc
exceptions3_SOURCES = exceptions3.cc

expandspacedim1_SOURCES = expandspacedim1.cc
expandspacedim2_SOURCES = expandspacedim2.cc

foldspacedims1_SOURCES = foldspacedims1.cc
foldspacedims2_SOURCES = foldspacedims2.cc

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

generators1_SOURCES = generators1.cc
generators2_SOURCES = generators2.cc
generators3_SOURCES = generators3.cc
generators4_SOURCES = generators4.cc
generators5_SOURCES = generators5.cc
generators6_SOURCES = generators6.cc

geomcovers1_SOURCES = geomcovers1.cc

h79widening1_SOURCES = h79widening1.cc
h79widening2_SOURCES = h79widening2.cc
h79widening3_SOURCES = h79widening3.cc
h79widening4_SOURCES = h79widening4.cc
h79widening5_SOURCES = h79widening5.cc
h79widening6_SOURCES = h79widening6.cc
h79widening7_SOURCES = h79widening7.cc
h79widening8_SOURCES = h79widening8.cc

intersection1_SOURCES = intersection1.cc
intersection2_SOURCES = intersection2.cc
intersection3_SOURCES = intersection3.cc
intersection4_SOURCES = intersection4.cc
intersection5_SOURCES = intersection5.cc
intersection6_SOURCES = intersection6.cc
intersection7_SOURCES = intersection7.cc
intersection8_SOURCES = intersection8.cc
intersection9_SOURCES = intersection9.cc
intersection10_SOURCES = intersection10.cc
intersection11_SOURCES = intersection11.cc

limitedbhrz03extrapolation1_SOURCES = limitedbhrz03extrapolation1.cc

limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc
limitedh79extrapolation2_SOURCES = limitedh79extrapolation2.cc
limitedh79extrapolation3_SOURCES = limitedh79extrapolation3.cc
limitedh79extrapolation4_SOURCES = limitedh79extrapolation4.cc

linearpartition1_SOURCES = linearpartition1.cc
linearpartition2_SOURCES = linearpartition2.cc
linearpartition3_SOURCES = linearpartition3.cc
linearpartition4_SOURCES = linearpartition4.cc

linexpression1_SOURCES = linexpression1.cc

mapspacedims1_SOURCES = mapspacedims1.cc
mapspacedims2_SOURCES = mapspacedims2.cc
mapspacedims3_SOURCES = mapspacedims3.cc
mapspacedims4_SOURCES = mapspacedims4.cc

max_min1_SOURCES = max_min1.cc
max_min2_SOURCES = max_min2.cc

maxspacedim1_SOURCES = maxspacedim1.cc

mc91_SOURCES = mc91.cc

membytes1_SOURCES = membytes1.cc
membytes2_SOURCES = membytes2.cc

memory1_SOURCES = memory1.cc

minconstraints1_SOURCES = minconstraints1.cc
minconstraints2_SOURCES = minconstraints2.cc
minconstraints3_SOURCES = minconstraints3.cc
minconstraints4_SOURCES = minconstraints4.cc

mingenerators1_SOURCES = mingenerators1.cc
mingenerators2_SOURCES = mingenerators2.cc
mingenerators3_SOURCES = mingenerators3.cc

onepoint_SOURCES = onepoint.cc

permute_SOURCES = permute.cc

polydifference1_SOURCES = polydifference1.cc
polydifference2_SOURCES = polydifference2.cc
polydifference3_SOURCES = polydifference3.cc
polydifference4_SOURCES = polydifference4.cc
polydifference5_SOURCES = polydifference5.cc
polydifference6_SOURCES = polydifference6.cc
polydifference7_SOURCES = polydifference7.cc
polydifference8_SOURCES = polydifference8.cc
polydifference9_SOURCES = polydifference9.cc

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
polyhull11_SOURCES = polyhull11.cc

randphull1_SOURCES = randphull1.cc
randphull2_SOURCES = randphull2.cc
randphull2_LDADD = $(LDADD) -lm

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc
relations3_SOURCES = relations3.cc
relations4_SOURCES = relations4.cc
relations5_SOURCES = relations5.cc
relations6_SOURCES = relations6.cc
relations7_SOURCES = relations7.cc
relations8_SOURCES = relations8.cc
relations9_SOURCES = relations9.cc
relations10_SOURCES = relations10.cc
relations11_SOURCES = relations11.cc
relations12_SOURCES = relations12.cc
relations13_SOURCES = relations13.cc
relations14_SOURCES = relations14.cc
relations15_SOURCES = relations15.cc
relations16_SOURCES = relations16.cc
relations17_SOURCES = relations17.cc
relations18_SOURCES = relations18.cc
relations19_SOURCES = relations19.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc
removespacedims3_SOURCES = removespacedims3.cc
removespacedims4_SOURCES = removespacedims4.cc
removespacedims5_SOURCES = removespacedims5.cc
removespacedims6_SOURCES = removespacedims6.cc
removespacedims7_SOURCES = removespacedims7.cc
removespacedims8_SOURCES = removespacedims8.cc
removespacedims9_SOURCES = removespacedims9.cc
removespacedims10_SOURCES = removespacedims10.cc

smm1_SOURCES = smm1.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc
timeelapse3_SOURCES = timeelapse3.cc
timeelapse4_SOURCES = timeelapse4.cc
timeelapse5_SOURCES = timeelapse5.cc
timeelapse6_SOURCES = timeelapse6.cc
timeelapse7_SOURCES = timeelapse7.cc
timeelapse8_SOURCES = timeelapse8.cc

topclosed1_SOURCES = topclosed1.cc
topclosed2_SOURCES = topclosed2.cc
topclosed3_SOURCES = topclosed3.cc

topclosure1_SOURCES = topclosure1.cc
topclosure2_SOURCES = topclosure2.cc
topclosure3_SOURCES = topclosure3.cc
topclosure4_SOURCES = topclosure4.cc

universe1_SOURCES = universe1.cc
universe2_SOURCES = universe2.cc
universe3_SOURCES = universe3.cc
universe4_SOURCES = universe4.cc
universe5_SOURCES = universe5.cc
universe6_SOURCES = universe6.cc
universe7_SOURCES = universe7.cc

writeconsys1_SOURCES = writeconsys1.cc

writegensys1_SOURCES = writegensys1.cc
writegensys2_SOURCES = writegensys2.cc
writegensys3_SOURCES = writegensys3.cc

writepolyhedron1_SOURCES = writepolyhedron1.cc
writepolyhedron2_SOURCES = writepolyhedron2.cc
writepolyhedron3_SOURCES = writepolyhedron3.cc

writerelation1_SOURCES = writerelation1.cc

writevariable1_SOURCES = writevariable1.cc

#
# Sources and compilation flags for the derived check programs
#

DERIVED_CXXFLAGS = -DDERIVED_TEST $(AM_CXXFLAGS)

nnc_addconstraint2_SOURCES = addconstraint2.cc
nnc_addconstraint2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraint3_SOURCES = addconstraint3.cc
nnc_addconstraint3_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addconstraints1_SOURCES = addconstraints1.cc
nnc_addconstraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints2_SOURCES = addconstraints2.cc
nnc_addconstraints2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints3_SOURCES = addconstraints3.cc
nnc_addconstraints3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints4_SOURCES = addconstraints4.cc
nnc_addconstraints4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints5_SOURCES = addconstraints5.cc
nnc_addconstraints5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints6_SOURCES = addconstraints6.cc
nnc_addconstraints6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints7_SOURCES = addconstraints7.cc
nnc_addconstraints7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints8_SOURCES = addconstraints8.cc
nnc_addconstraints8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints9_SOURCES = addconstraints9.cc
nnc_addconstraints9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints10_SOURCES = addconstraints10.cc
nnc_addconstraints10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addconstraints13_SOURCES = addconstraints13.cc
nnc_addconstraints13_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addgenerator3_SOURCES = addgenerator3.cc
nnc_addgenerator3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerator5_SOURCES = addgenerator5.cc
nnc_addgenerator5_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addgenerators1_SOURCES = addgenerators1.cc
nnc_addgenerators1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators2_SOURCES = addgenerators2.cc
nnc_addgenerators2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators3_SOURCES = addgenerators3.cc
nnc_addgenerators3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators4_SOURCES = addgenerators4.cc
nnc_addgenerators4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators5_SOURCES = addgenerators5.cc
nnc_addgenerators5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators6_SOURCES = addgenerators6.cc
nnc_addgenerators6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators9_SOURCES = addgenerators9.cc
nnc_addgenerators9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators10_SOURCES = addgenerators10.cc
nnc_addgenerators10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators12_SOURCES = addgenerators12.cc
nnc_addgenerators12_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addgenerators13_SOURCES = addgenerators13.cc
nnc_addgenerators13_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addspacedims1_SOURCES = addspacedims1.cc
nnc_addspacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addspacedims2_SOURCES = addspacedims2.cc
nnc_addspacedims2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addspacedims3_SOURCES = addspacedims3.cc
nnc_addspacedims3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addspacedims4_SOURCES = addspacedims4.cc
nnc_addspacedims4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addspacedims9_SOURCES = addspacedims9.cc
nnc_addspacedims9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_addspacedims12_SOURCES = addspacedims12.cc
nnc_addspacedims12_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_affineimage1_SOURCES = affineimage1.cc
nnc_affineimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage2_SOURCES = affineimage2.cc
nnc_affineimage2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage3_SOURCES = affineimage3.cc
nnc_affineimage3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage4_SOURCES = affineimage4.cc
nnc_affineimage4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage5_SOURCES = affineimage5.cc
nnc_affineimage5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage6_SOURCES = affineimage6.cc
nnc_affineimage6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage7_SOURCES = affineimage7.cc
nnc_affineimage7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affineimage8_SOURCES = affineimage8.cc
nnc_affineimage8_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_affinepreimage1_SOURCES = affinepreimage1.cc
nnc_affinepreimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage2_SOURCES = affinepreimage2.cc
nnc_affinepreimage2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage3_SOURCES = affinepreimage3.cc
nnc_affinepreimage3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage4_SOURCES = affinepreimage4.cc
nnc_affinepreimage4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage5_SOURCES = affinepreimage5.cc
nnc_affinepreimage5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage6_SOURCES = affinepreimage6.cc
nnc_affinepreimage6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage7_SOURCES = affinepreimage7.cc
nnc_affinepreimage7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage8_SOURCES = affinepreimage8.cc
nnc_affinepreimage8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage9_SOURCES = affinepreimage9.cc
nnc_affinepreimage9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_affinepreimage10_SOURCES = affinepreimage10.cc
nnc_affinepreimage10_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_affinetrans_SOURCES = affinetrans.cc
nnc_affinetrans_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_append1_SOURCES = append1.cc
nnc_append1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_append2_SOURCES = append2.cc
nnc_append2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_ascii_dump_load1_SOURCES = ascii_dump_load1.cc
nnc_ascii_dump_load1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load2_SOURCES = ascii_dump_load2.cc
nnc_ascii_dump_load2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load3_SOURCES = ascii_dump_load3.cc
nnc_ascii_dump_load3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load6_SOURCES = ascii_dump_load6.cc
nnc_ascii_dump_load6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load7_SOURCES = ascii_dump_load7.cc
nnc_ascii_dump_load7_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bgp99extrapolation1_SOURCES = bgp99extrapolation1.cc
nnc_bgp99extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bgp99extrapolation2_SOURCES = bgp99extrapolation2.cc
nnc_bgp99extrapolation2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bhrz03widening1_SOURCES = bhrz03widening1.cc
nnc_bhrz03widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening2_SOURCES = bhrz03widening2.cc
nnc_bhrz03widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening3_SOURCES = bhrz03widening3.cc
nnc_bhrz03widening3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening5_SOURCES = bhrz03widening5.cc
nnc_bhrz03widening5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening6_SOURCES = bhrz03widening6.cc
nnc_bhrz03widening6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening8_SOURCES = bhrz03widening8.cc
nnc_bhrz03widening8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening9_SOURCES = bhrz03widening9.cc
nnc_bhrz03widening9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening10_SOURCES = bhrz03widening10.cc
nnc_bhrz03widening10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening11_SOURCES = bhrz03widening11.cc
nnc_bhrz03widening11_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening12_SOURCES = bhrz03widening12.cc
nnc_bhrz03widening12_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bounded1_SOURCES = bounded1.cc
nnc_bounded1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundedh79extrapolation1_SOURCES = boundedh79extrapolation1.cc
nnc_boundedh79extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundingbox1_SOURCES = boundingbox1.cc BBox.cc
nnc_boundingbox1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_boundingbox3_SOURCES = boundingbox3.cc BBox.cc
nnc_boundingbox3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_boundingbox5_SOURCES = boundingbox3.cc BBox.cc
nnc_boundingbox5_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bounds1_SOURCES = bounds1.cc
nnc_bounds1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bounds2_SOURCES = bounds2.cc
nnc_bounds2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_concatenate1_SOURCES = concatenate1.cc
nnc_concatenate1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_concatenate2_SOURCES = concatenate2.cc
nnc_concatenate2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_concatenate3_SOURCES = concatenate3.cc
nnc_concatenate3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_concatenate4_SOURCES = concatenate4.cc
nnc_concatenate4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_concatenate5_SOURCES = concatenate5.cc
nnc_concatenate5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_concatenate6_SOURCES = concatenate6.cc
nnc_concatenate6_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_constraints1_SOURCES = constraints1.cc
nnc_constraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_constraints2_SOURCES = constraints2.cc
nnc_constraints2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_constraints3_SOURCES = constraints3.cc
nnc_constraints3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_constraints4_SOURCES = constraints4.cc
nnc_constraints4_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_contains1_SOURCES = contains1.cc
nnc_contains1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_contains2_SOURCES = contains2.cc
nnc_contains2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_disjoint1_SOURCES = disjoint1.cc
nnc_disjoint1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_disjoint2_SOURCES = disjoint2.cc
nnc_disjoint2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_empty1_SOURCES = empty1.cc
nnc_empty1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_equals1_SOURCES = equals1.cc
nnc_equals1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_exceptions1_SOURCES = exceptions1.cc
nnc_exceptions1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_exceptions3_SOURCES = exceptions3.cc
nnc_exceptions3_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_expandspacedim1_SOURCES = expandspacedim1.cc
nnc_expandspacedim1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_foldspacedims1_SOURCES = foldspacedims1.cc
nnc_foldspacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
nnc_generalizedaffineimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc
nnc_generalizedaffineimage2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generalizedaffineimage3_SOURCES = generalizedaffineimage3.cc
nnc_generalizedaffineimage3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generalizedaffineimage5_SOURCES = generalizedaffineimage5.cc
nnc_generalizedaffineimage5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generalizedaffineimage6_SOURCES = generalizedaffineimage6.cc
nnc_generalizedaffineimage6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generalizedaffineimage11_SOURCES = generalizedaffineimage11.cc
nnc_generalizedaffineimage11_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_generators1_SOURCES = generators1.cc
nnc_generators1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generators2_SOURCES = generators2.cc
nnc_generators2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generators3_SOURCES = generators3.cc
nnc_generators3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generators4_SOURCES = generators4.cc
nnc_generators4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generators5_SOURCES = generators5.cc
nnc_generators5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_generators6_SOURCES = generators6.cc
nnc_generators6_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_geomcovers1_SOURCES = geomcovers1.cc
nnc_geomcovers1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_h79widening1_SOURCES = h79widening1.cc
nnc_h79widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening2_SOURCES = h79widening2.cc
nnc_h79widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening4_SOURCES = h79widening4.cc
nnc_h79widening4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening5_SOURCES = h79widening5.cc
nnc_h79widening5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening6_SOURCES = h79widening6.cc
nnc_h79widening6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening7_SOURCES = h79widening7.cc
nnc_h79widening7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening8_SOURCES = h79widening8.cc
nnc_h79widening8_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_intersection1_SOURCES = intersection1.cc
nnc_intersection1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection2_SOURCES = intersection2.cc
nnc_intersection2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection3_SOURCES = intersection3.cc
nnc_intersection3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection4_SOURCES = intersection4.cc
nnc_intersection4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection5_SOURCES = intersection5.cc
nnc_intersection5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection6_SOURCES = intersection6.cc
nnc_intersection6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection7_SOURCES = intersection7.cc
nnc_intersection7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection8_SOURCES = intersection8.cc
nnc_intersection8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection9_SOURCES = intersection9.cc
nnc_intersection9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection10_SOURCES = intersection10.cc
nnc_intersection10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_intersection11_SOURCES = intersection11.cc
nnc_intersection11_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_limitedbhrz03extrapolation1_SOURCES = limitedbhrz03extrapolation1.cc
nnc_limitedbhrz03extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc
nnc_limitedh79extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79extrapolation2_SOURCES = limitedh79extrapolation2.cc
nnc_limitedh79extrapolation2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79extrapolation3_SOURCES = limitedh79extrapolation3.cc
nnc_limitedh79extrapolation3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79extrapolation4_SOURCES = limitedh79extrapolation4.cc
nnc_limitedh79extrapolation4_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_linearpartition1_SOURCES = linearpartition1.cc
nnc_linearpartition1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_linearpartition2_SOURCES = linearpartition2.cc
nnc_linearpartition2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_linearpartition3_SOURCES = linearpartition3.cc
nnc_linearpartition3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_linearpartition4_SOURCES = linearpartition4.cc
nnc_linearpartition4_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mapspacedims1_SOURCES = mapspacedims1.cc
nnc_mapspacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_mapspacedims2_SOURCES = mapspacedims2.cc
nnc_mapspacedims2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_mapspacedims3_SOURCES = mapspacedims3.cc
nnc_mapspacedims3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_mapspacedims4_SOURCES = mapspacedims4.cc
nnc_mapspacedims4_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_max_min1_SOURCES = max_min1.cc
nnc_max_min1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_max_min2_SOURCES = max_min2.cc
nnc_max_min2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mc91_SOURCES = mc91.cc
nnc_mc91_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_membytes1_SOURCES = membytes1.cc
nnc_membytes1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_membytes2_SOURCES = membytes2.cc
nnc_membytes2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_minconstraints1_SOURCES = minconstraints1.cc
nnc_minconstraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_minconstraints2_SOURCES = minconstraints2.cc
nnc_minconstraints2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mingenerators1_SOURCES = mingenerators1.cc
nnc_mingenerators1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_mingenerators2_SOURCES = mingenerators2.cc
nnc_mingenerators2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_onepoint_SOURCES = onepoint.cc
nnc_onepoint_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_permute_SOURCES = permute.cc
nnc_permute_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_polydifference1_SOURCES = polydifference1.cc
nnc_polydifference1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polydifference3_SOURCES = polydifference3.cc
nnc_polydifference3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polydifference4_SOURCES = polydifference4.cc
nnc_polydifference4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polydifference6_SOURCES = polydifference6.cc
nnc_polydifference6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polydifference7_SOURCES = polydifference7.cc
nnc_polydifference7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polydifference9_SOURCES = polydifference9.cc
nnc_polydifference9_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_polyhull1_SOURCES = polyhull1.cc
nnc_polyhull1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull2_SOURCES = polyhull2.cc
nnc_polyhull2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull3_SOURCES = polyhull3.cc
nnc_polyhull3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull4_SOURCES = polyhull4.cc
nnc_polyhull4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull5_SOURCES = polyhull5.cc
nnc_polyhull5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull6_SOURCES = polyhull6.cc
nnc_polyhull6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull7_SOURCES = polyhull7.cc
nnc_polyhull7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull8_SOURCES = polyhull8.cc
nnc_polyhull8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull9_SOURCES = polyhull9.cc
nnc_polyhull9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_polyhull10_SOURCES = polyhull10.cc
nnc_polyhull10_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_randphull1_SOURCES = randphull1.cc
nnc_randphull1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_randphull2_SOURCES = randphull2.cc
nnc_randphull2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_randphull2_LDADD = $(LDADD) -lm

nnc_relations1_SOURCES = relations1.cc
nnc_relations1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations2_SOURCES = relations2.cc
nnc_relations2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations3_SOURCES = relations3.cc
nnc_relations3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations4_SOURCES = relations4.cc
nnc_relations4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations5_SOURCES = relations5.cc
nnc_relations5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations6_SOURCES = relations6.cc
nnc_relations6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations7_SOURCES = relations7.cc
nnc_relations7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations8_SOURCES = relations8.cc
nnc_relations8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations9_SOURCES = relations9.cc
nnc_relations9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations10_SOURCES = relations10.cc
nnc_relations10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations12_SOURCES = relations12.cc
nnc_relations12_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations13_SOURCES = relations13.cc
nnc_relations13_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations14_SOURCES = relations14.cc
nnc_relations14_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations15_SOURCES = relations15.cc
nnc_relations15_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations16_SOURCES = relations16.cc
nnc_relations16_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations19_SOURCES = relations19.cc
nnc_relations19_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_removespacedims1_SOURCES = removespacedims1.cc
nnc_removespacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims2_SOURCES = removespacedims2.cc
nnc_removespacedims2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims3_SOURCES = removespacedims3.cc
nnc_removespacedims3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims4_SOURCES = removespacedims4.cc
nnc_removespacedims4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims5_SOURCES = removespacedims5.cc
nnc_removespacedims5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims6_SOURCES = removespacedims6.cc
nnc_removespacedims6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims8_SOURCES = removespacedims8.cc
nnc_removespacedims8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims9_SOURCES = removespacedims9.cc
nnc_removespacedims9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removespacedims10_SOURCES = removespacedims10.cc
nnc_removespacedims10_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_smm1_SOURCES = smm1.cc
nnc_smm1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_timeelapse2_SOURCES = timeelapse2.cc
nnc_timeelapse2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_timeelapse5_SOURCES = timeelapse5.cc
nnc_timeelapse5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_timeelapse6_SOURCES = timeelapse6.cc
nnc_timeelapse6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_timeelapse7_SOURCES = timeelapse7.cc
nnc_timeelapse7_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_timeelapse8_SOURCES = timeelapse8.cc
nnc_timeelapse8_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_universe1_SOURCES = universe1.cc
nnc_universe1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_universe2_SOURCES = universe2.cc
nnc_universe2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_universe3_SOURCES = universe3.cc
nnc_universe3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_universe4_SOURCES = universe4.cc
nnc_universe4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_universe5_SOURCES = universe5.cc
nnc_universe5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_universe7_SOURCES = universe7.cc
nnc_universe7_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_writepolyhedron2_SOURCES = writepolyhedron2.cc
nnc_writepolyhedron2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_writepolyhedron3_SOURCES = writepolyhedron3.cc
nnc_writepolyhedron3_CXXFLAGS = $(DERIVED_CXXFLAGS)

watchdog1_SRCS = watchdog1.cc

if BUILD_WATCHDOG_LIBRARY
WATCHDOG_TESTS = watchdog1

watchdog1_SOURCES = $(watchdog1_SRCS)
watchdog1_LDADD = $(LDADD) $(top_builddir)/Watchdog/libpwl.la

endif BUILD_WATCHDOG_LIBRARY

TESTS = $(ORIGINAL_TESTS) $(DERIVED_TESTS) $(WATCHDOG_TESTS)

XFAIL_ALWAYS =

XFAIL_WITH_INT64 = \
$(XFAIL_ALWAYS) \
randphull2 \
nnc_randphull2

XFAIL_WITH_INT32 = \
$(XFAIL_WITH_INT64) \
dualhypercubes \
equals1 \
nnc_equals1

XFAIL_WITH_INT16 = \
$(XFAIL_WITH_INT32) \
bgp99extrapolation1 \
nnc_bgp99extrapolation1

XFAIL_WITH_INT8 = \
$(XFAIL_WITH_INT16) \
NNCminimize2 \
NNCminimize4 \
bgp99extrapolation2 \
bhrz03widening14 \
bhrz03widening15 \
bhrz03widening18 \
bhz03widening1 \
bhz03widening4 \
bhz03widening6 \
boundedh79extrapolation1 \
foldspacedims1 \
foldspacedims2 \
generalizedaffineimage3 \
generalizedaffineimage7 \
mapspacedims2 \
max_min2 \
membytes1 \
onepoint \
polydifference9 \
randphull1 \
removespacedims2 \
smm1 \
nnc_bgp99extrapolation2 \
nnc_boundedh79extrapolation1 \
nnc_generalizedaffineimage3 \
nnc_max_min2 \
nnc_membytes1 \
nnc_onepoint \
nnc_polydifference9 \
nnc_randphull1 \
nnc_smm1

XFAIL_WITH_INT8_A = \
$(XFAIL_WITH_INT8) \
ascii_dump_load4 \
boundingbox4 \
intersection1 \
intersection2 \
nnc_intersection1 \
nnc_intersection2 \
nnc_membytes2 \
nnc_removespacedims2


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

if ASSERTIONS_ENABLED

XFAIL_SET = $(XFAIL_WITH_INT8_A)

else !ASSERTIONS_ENABLED

XFAIL_SET = $(XFAIL_WITH_INT8)

endif !ASSERTIONS_ENABLED

else !USE_INT8

XFAIL_SET = $(XFAIL_ALWAYS)

endif !USE_INT8
endif !USE_INT16
endif !USE_INT32
endif !USE_INT64

XFAIL_TESTS = $(XFAIL_SET)

check_PROGRAMS = $(TESTS) $(BUGS)

EXTRA_DIST = $(watchdog1_SRCS)

BUGS =

MOSTLYCLEANFILES = \
ascii_dump_load1.dat \
ascii_dump_load2.dat \
ascii_dump_load3.dat \
ascii_dump_load4.dat \
ascii_dump_load5.dat \
ascii_dump_load6.dat \
ascii_dump_load7.dat \
writeconsys1.dat \
writegensys1.dat \
writegensys2.dat \
writegensys3.dat \
writepolyhedron1.dat \
writepolyhedron2.dat \
writepolyhedron3.dat \
writerelation1.dat \
writevariable1.dat
