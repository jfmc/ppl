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

SUBDIRS =

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/Watchdog @extra_includes@

LDADD = $(top_builddir)/src/libppl.la ehandlers.o print.o @extra_libraries@

CPPFLAGS = @extra_cpp_flags@

ORIGINAL_TESTS = \
	CbecomesNNC1 \
	NNCinclusion1 \
	NNCminimize1 NNCminimize2 NNCminimize3 NNCminimize4 NNCminimize5 \
        NNCminimize6 \
	addconstraint1 addconstraint2 addconstraint3 \
        addconstraints1 addconstraints2 addconstraints3 addconstraints4 \
	addconstraints5 addconstraints6 addconstraints7 addconstraints8 \
	addconstraints9 addconstraints10 addconstraints11 \
	addconstraints12 \
	adddimensions1 adddimensions2 adddimensions3 adddimensions4 \
	adddimensions5 adddimensions6 adddimensions7 adddimensions8 \
	adddimensions9 adddimensions10 adddimensions11 adddimensions12 \
	addgenerator1 addgenerator2 addgenerator3 \
	addgenerators1 addgenerators2 addgenerators3 addgenerators4 \
	addgenerators5 addgenerators6 addgenerators7 addgenerators8 \
	addgenerators9 addgenerators10 addgenerators11 \
	affineimage1 affineimage2 affineimage3 affineimage4 affineimage5 \
	affineimage6 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinepreimage5 affinepreimage6 affinepreimage7 affinepreimage8 \
	affinetrans \
        append1 append2 \
	ascii_dump_load1 ascii_dump_load2 ascii_dump_load3 \
	bbrz02widening1 bbrz02widening2 bbrz02widening3 bbrz02widening4 \
	bbrz02widening5 bbrz02widening6 bbrz02widening7 bbrz02widening8 \
	bbrz02widening9 bbrz02widening10 bbrz02widening11 bbrz02widening12 \
	bbrz02widening13 bbrz02widening14 bbrz02widening15 bbrz02widening16 \
	bbrz02widening17 bbrz02widening18 bbrz02widening19 \
	bounded1 \
	bounds1 bounds2 \
	boundingbox1 boundingbox2 \
	concatenate1 concatenate2 concatenate3 concatenate4 concatenate5 \
	constraints1 constraints2 constraints3 \
	contains1 \
	disjoint1 \
        empty1 \
	exceptions1 exceptions2 \
	generalizedaffineimage1 generalizedaffineimage2 \
	generalizedaffineimage3 generalizedaffineimage4 \
	generalizedaffineimage5 generalizedaffineimage6 \
	generalizedaffineimage7 generalizedaffineimage8 \
	generalizedaffineimage9 generalizedaffineimage10 \
	generalizedaffineimage11 generalizedaffineimage12 \
	generators1 generators2 generators3 generators4 generators5 \
	h79widening1 h79widening2 h79widening3 h79widening4 h79widening5 \
	inclusion1 inclusion2 \
        intersection1 intersection2 intersection3 intersection4 \
	intersection5 intersection6 intersection7 intersection8 \
	intersection9 intersection10 \
	limitedbbrz02widening1 \
	limitedh79widening1 limitedh79widening2 \
	limitedh79widening3 limitedh79widening4 \
	linearpartition1 linearpartition2 linearpartition3 \
	mc91 \
	memory1 \
	minconstraints1 minconstraints2 minconstraints3 \
	mingenerators1 mingenerators2 mingenerators3 \
	onepoint \
        permute \
	polydifference1 polydifference2 polydifference3 polydifference4 \
	polydifference5 polydifference6 polydifference7 \
	polyhull1 polyhull2 polyhull3 polyhull4 polyhull5 polyhull6 \
	polyhull7 polyhull8 \
	randphull1 \
	relations1 relations2 relations3 relations4 relations5 relations6 \
	relations7 relations8 relations9 relations10 relations11 \
	relations12 relations13 relations14 relations15 relations16 \
	relations17 \
	removedim1 removedim2 removedim3 removedim4 removedim5 removedim6 \
	removedim7 removedim8 removedim9 \
	smm1 \
	stronglyminimize1 stronglyminimize2 stronglyminimize3 \
	timeelapse1 timeelapse2 timeelapse3 timeelapse4 timeelapse5 \
	timeelapse6 timeelapse7 \
	topclosed1 topclosed2 topclosed3 \
	topclosure1 topclosure2 topclosure3 \
	universe1 universe2 universe3 universe4 universe5 universe6 \
	writegensys1 writegensys2 \
	writepolyhedron1 writepolyhedron2

DERIVED_TESTS = \
	nnc_addconstraint2 nnc_addconstraint3 \
	nnc_addconstraints1 nnc_addconstraints2 nnc_addconstraints3 \
	nnc_addconstraints4 nnc_addconstraints5 nnc_addconstraints6 \
	nnc_addconstraints7 nnc_addconstraints8 nnc_addconstraints9 \
	nnc_addconstraints10 \
	nnc_adddimensions1 nnc_adddimensions2 nnc_adddimensions3 \
	nnc_adddimensions4 nnc_adddimensions9 nnc_adddimensions12 \
	nnc_addgenerator3 \
	nnc_addgenerators1 nnc_addgenerators2 nnc_addgenerators3 \
	nnc_addgenerators4 nnc_addgenerators5 nnc_addgenerators6 \
	nnc_addgenerators9 nnc_addgenerators10 \
	nnc_affineimage1 nnc_affineimage2 nnc_affineimage3 nnc_affineimage4 \
	nnc_affineimage5 nnc_affineimage6 \
	nnc_affinepreimage1 nnc_affinepreimage2 nnc_affinepreimage3 \
	nnc_affinepreimage4 nnc_affinepreimage5 nnc_affinepreimage6 \
	nnc_affinepreimage7 nnc_affinepreimage8 \
	nnc_affinetrans \
	nnc_append1 nnc_append2 \
	nnc_ascii_dump_load1 nnc_ascii_dump_load2 nnc_ascii_dump_load3 \
	nnc_bbrz02widening1 nnc_bbrz02widening2 nnc_bbrz02widening3 \
	nnc_bbrz02widening5 nnc_bbrz02widening6 \
	nnc_bbrz02widening8 nnc_bbrz02widening9 \
	nnc_bbrz02widening10 nnc_bbrz02widening11 nnc_bbrz02widening12 \
	nnc_bounded1 \
	nnc_bounds1 nnc_bounds2 \
	nnc_concatenate1 nnc_concatenate2 nnc_concatenate3 nnc_concatenate4 \
	nnc_concatenate5 \
	nnc_constraints1 nnc_constraints2 nnc_constraints3 \
	nnc_disjoint1 \
	nnc_empty1 \
	nnc_exceptions1 \
	nnc_generalizedaffineimage1 nnc_generalizedaffineimage2 \
	nnc_generalizedaffineimage3 \
	nnc_generalizedaffineimage5 nnc_generalizedaffineimage6 \
	nnc_generalizedaffineimage11 \
	nnc_generators1 nnc_generators2 nnc_generators3 nnc_generators4 \
	nnc_generators5 \
	nnc_h79widening1 nnc_h79widening2 nnc_h79widening4 nnc_h79widening5 \
	nnc_inclusion1 nnc_inclusion2 \
	nnc_intersection1 nnc_intersection2 nnc_intersection3 \
	nnc_intersection4 nnc_intersection5 nnc_intersection6 \
	nnc_intersection7 nnc_intersection8 nnc_intersection9 \
	nnc_intersection10 \
	nnc_limitedbbrz02widening1 \
	nnc_limitedh79widening1 nnc_limitedh79widening2 \
	nnc_limitedh79widening3 nnc_limitedh79widening4 \
	nnc_linearpartition1 nnc_linearpartition2 nnc_linearpartition3 \
	nnc_mc91 \
	nnc_minconstraints1 nnc_minconstraints2 \
	nnc_mingenerators1 nnc_mingenerators2 \
	nnc_onepoint \
	nnc_permute \
	nnc_polydifference1 nnc_polydifference3 nnc_polydifference4 \
	nnc_polydifference6 nnc_polydifference7 \
	nnc_polyhull1 nnc_polyhull2 nnc_polyhull3 nnc_polyhull4 \
	nnc_polyhull5 nnc_polyhull6 nnc_polyhull7 nnc_polyhull8 \
	nnc_randphull1 \
	nnc_relations1 nnc_relations2 nnc_relations3 nnc_relations4 \
	nnc_relations5 nnc_relations6 nnc_relations7 nnc_relations8 \
	nnc_relations9 nnc_relations10 nnc_relations12 nnc_relations13 \
	nnc_relations14 nnc_relations15 nnc_relations16 \
	nnc_removedim1 nnc_removedim2 nnc_removedim3 nnc_removedim4 \
	nnc_removedim5 nnc_removedim6 nnc_removedim8 nnc_removedim9 \
	nnc_smm1 \
	nnc_timeelapse2 nnc_timeelapse5 nnc_timeelapse6 nnc_timeelapse7 \
	nnc_universe1 nnc_universe2 nnc_universe3 nnc_universe4 nnc_universe5 \
	nnc_writepolyhedron2

#
# Sources for the tests
#

CbecomesNNC1_SOURCES = CbecomesNNC1.cc

NNCinclusion1_SOURCES = NNCinclusion1.cc

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

adddimensions1_SOURCES = adddimensions1.cc
adddimensions2_SOURCES = adddimensions2.cc
adddimensions3_SOURCES = adddimensions3.cc
adddimensions4_SOURCES = adddimensions4.cc
adddimensions5_SOURCES = adddimensions5.cc
adddimensions6_SOURCES = adddimensions6.cc
adddimensions7_SOURCES = adddimensions7.cc
adddimensions8_SOURCES = adddimensions8.cc
adddimensions9_SOURCES = adddimensions9.cc
adddimensions10_SOURCES = adddimensions10.cc
adddimensions11_SOURCES = adddimensions11.cc
adddimensions12_SOURCES = adddimensions12.cc

addgenerator1_SOURCES = addgenerator1.cc
addgenerator2_SOURCES = addgenerator2.cc
addgenerator3_SOURCES = addgenerator3.cc

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

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc
affineimage3_SOURCES = affineimage3.cc
affineimage4_SOURCES = affineimage4.cc
affineimage5_SOURCES = affineimage5.cc
affineimage6_SOURCES = affineimage6.cc

affinepreimage1_SOURCES = affinepreimage1.cc
affinepreimage2_SOURCES = affinepreimage2.cc
affinepreimage3_SOURCES = affinepreimage3.cc
affinepreimage4_SOURCES = affinepreimage4.cc
affinepreimage5_SOURCES = affinepreimage5.cc
affinepreimage6_SOURCES = affinepreimage6.cc
affinepreimage7_SOURCES = affinepreimage7.cc
affinepreimage8_SOURCES = affinepreimage8.cc

affinetrans_SOURCES = affinetrans.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc files.cc
ascii_dump_load2_SOURCES = ascii_dump_load2.cc files.cc
ascii_dump_load3_SOURCES = ascii_dump_load3.cc files.cc

bbrz02widening1_SOURCES = bbrz02widening1.cc
bbrz02widening2_SOURCES = bbrz02widening2.cc
bbrz02widening3_SOURCES = bbrz02widening3.cc
bbrz02widening4_SOURCES = bbrz02widening4.cc
bbrz02widening5_SOURCES = bbrz02widening5.cc
bbrz02widening6_SOURCES = bbrz02widening6.cc
bbrz02widening7_SOURCES = bbrz02widening7.cc
bbrz02widening8_SOURCES = bbrz02widening8.cc
bbrz02widening9_SOURCES = bbrz02widening9.cc
bbrz02widening10_SOURCES = bbrz02widening10.cc
bbrz02widening11_SOURCES = bbrz02widening11.cc
bbrz02widening12_SOURCES = bbrz02widening12.cc
bbrz02widening13_SOURCES = bbrz02widening13.cc
bbrz02widening14_SOURCES = bbrz02widening14.cc
bbrz02widening15_SOURCES = bbrz02widening15.cc
bbrz02widening16_SOURCES = bbrz02widening16.cc
bbrz02widening17_SOURCES = bbrz02widening17.cc
bbrz02widening18_SOURCES = bbrz02widening18.cc
bbrz02widening19_SOURCES = bbrz02widening19.cc

bounded1_SOURCES = bounded1.cc

bounds1_SOURCES = bounds1.cc
bounds2_SOURCES = bounds2.cc

boundingbox1_SOURCES = boundingbox1.cc
boundingbox2_SOURCES = boundingbox2.cc

concatenate1_SOURCES = concatenate1.cc
concatenate2_SOURCES = concatenate2.cc
concatenate3_SOURCES = concatenate3.cc
concatenate4_SOURCES = concatenate4.cc
concatenate5_SOURCES = concatenate5.cc

constraints1_SOURCES = constraints1.cc
constraints2_SOURCES = constraints2.cc
constraints3_SOURCES = constraints3.cc

contains1_SOURCES = contains1.cc

disjoint1_SOURCES = disjoint1.cc

empty1_SOURCES = empty1.cc

exceptions1_SOURCES = exceptions1.cc
exceptions2_SOURCES = exceptions2.cc

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

h79widening1_SOURCES = h79widening1.cc
h79widening2_SOURCES = h79widening2.cc
h79widening3_SOURCES = h79widening3.cc
h79widening4_SOURCES = h79widening4.cc
h79widening5_SOURCES = h79widening5.cc

inclusion1_SOURCES = inclusion1.cc
inclusion2_SOURCES = inclusion2.cc

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

limitedbbrz02widening1_SOURCES = limitedbbrz02widening1.cc

limitedh79widening1_SOURCES = limitedh79widening1.cc
limitedh79widening2_SOURCES = limitedh79widening2.cc
limitedh79widening3_SOURCES = limitedh79widening3.cc
limitedh79widening4_SOURCES = limitedh79widening4.cc

linearpartition1_SOURCES = linearpartition1.cc
linearpartition2_SOURCES = linearpartition2.cc
linearpartition3_SOURCES = linearpartition3.cc

memory1_SOURCES = memory1.cc

mc91_SOURCES = mc91.cc

minconstraints1_SOURCES = minconstraints1.cc
minconstraints2_SOURCES = minconstraints2.cc
minconstraints3_SOURCES = minconstraints3.cc

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

polyhull1_SOURCES = polyhull1.cc
polyhull2_SOURCES = polyhull2.cc
polyhull3_SOURCES = polyhull3.cc
polyhull4_SOURCES = polyhull4.cc
polyhull5_SOURCES = polyhull5.cc
polyhull6_SOURCES = polyhull6.cc
polyhull7_SOURCES = polyhull7.cc
polyhull8_SOURCES = polyhull8.cc

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

randphull1_SOURCES = randphull1.cc

removedim1_SOURCES = removedim1.cc
removedim2_SOURCES = removedim2.cc
removedim3_SOURCES = removedim3.cc
removedim4_SOURCES = removedim4.cc
removedim5_SOURCES = removedim5.cc
removedim6_SOURCES = removedim6.cc
removedim7_SOURCES = removedim7.cc
removedim8_SOURCES = removedim8.cc
removedim9_SOURCES = removedim9.cc

smm1_SOURCES = smm1.cc

stronglyminimize1_SOURCES = stronglyminimize1.cc
stronglyminimize2_SOURCES = stronglyminimize2.cc
stronglyminimize3_SOURCES = stronglyminimize3.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc
timeelapse3_SOURCES = timeelapse3.cc
timeelapse4_SOURCES = timeelapse4.cc
timeelapse5_SOURCES = timeelapse5.cc
timeelapse6_SOURCES = timeelapse6.cc
timeelapse7_SOURCES = timeelapse7.cc

topclosed1_SOURCES = topclosed1.cc
topclosed2_SOURCES = topclosed2.cc
topclosed3_SOURCES = topclosed3.cc

topclosure1_SOURCES = topclosure1.cc
topclosure2_SOURCES = topclosure2.cc
topclosure3_SOURCES = topclosure3.cc

universe1_SOURCES = universe1.cc
universe2_SOURCES = universe2.cc
universe3_SOURCES = universe3.cc
universe4_SOURCES = universe4.cc
universe5_SOURCES = universe5.cc
universe6_SOURCES = universe6.cc

writegensys1_SOURCES = writegensys1.cc files.cc
writegensys2_SOURCES = writegensys2.cc files.cc

writepolyhedron1_SOURCES = writepolyhedron1.cc files.cc
writepolyhedron2_SOURCES = writepolyhedron2.cc files.cc

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

nnc_adddimensions1_SOURCES = adddimensions1.cc
nnc_adddimensions1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_adddimensions2_SOURCES = adddimensions2.cc
nnc_adddimensions2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_adddimensions3_SOURCES = adddimensions3.cc
nnc_adddimensions3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_adddimensions4_SOURCES = adddimensions4.cc
nnc_adddimensions4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_adddimensions9_SOURCES = adddimensions9.cc
nnc_adddimensions9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_adddimensions12_SOURCES = adddimensions12.cc
nnc_adddimensions12_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addgenerator3_SOURCES = addgenerator3.cc
nnc_addgenerator3_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_affinetrans_SOURCES = affinetrans.cc
nnc_affinetrans_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_append1_SOURCES = append1.cc
nnc_append1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_append2_SOURCES = append2.cc
nnc_append2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_ascii_dump_load1_SOURCES = ascii_dump_load1.cc files.cc
nnc_ascii_dump_load1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load2_SOURCES = ascii_dump_load2.cc files.cc
nnc_ascii_dump_load2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_ascii_dump_load3_SOURCES = ascii_dump_load3.cc files.cc
nnc_ascii_dump_load3_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bbrz02widening1_SOURCES = bbrz02widening1.cc
nnc_bbrz02widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening2_SOURCES = bbrz02widening2.cc
nnc_bbrz02widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening3_SOURCES = bbrz02widening3.cc
nnc_bbrz02widening3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening5_SOURCES = bbrz02widening5.cc
nnc_bbrz02widening5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening6_SOURCES = bbrz02widening6.cc
nnc_bbrz02widening6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening8_SOURCES = bbrz02widening8.cc
nnc_bbrz02widening8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening9_SOURCES = bbrz02widening9.cc
nnc_bbrz02widening9_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening10_SOURCES = bbrz02widening10.cc
nnc_bbrz02widening10_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening11_SOURCES = bbrz02widening11.cc
nnc_bbrz02widening11_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bbrz02widening12_SOURCES = bbrz02widening12.cc
nnc_bbrz02widening12_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bounded1_SOURCES = bounded1.cc
nnc_bounded1_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_constraints1_SOURCES = constraints1.cc
nnc_constraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_constraints2_SOURCES = constraints2.cc
nnc_constraints2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_constraints3_SOURCES = constraints3.cc
nnc_constraints3_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_empty1_SOURCES = empty1.cc
nnc_empty1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_disjoint1_SOURCES = disjoint1.cc
nnc_disjoint1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_exceptions1_SOURCES = exceptions1.cc
nnc_exceptions1_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_h79widening1_SOURCES = h79widening1.cc
nnc_h79widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening2_SOURCES = h79widening2.cc
nnc_h79widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening4_SOURCES = h79widening4.cc
nnc_h79widening4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_h79widening5_SOURCES = h79widening5.cc
nnc_h79widening5_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_inclusion1_SOURCES = inclusion1.cc
nnc_inclusion1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_inclusion2_SOURCES = inclusion2.cc
nnc_inclusion2_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_limitedbbrz02widening1_SOURCES = limitedbbrz02widening1.cc
nnc_limitedbbrz02widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_limitedh79widening1_SOURCES = limitedh79widening1.cc
nnc_limitedh79widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79widening2_SOURCES = limitedh79widening2.cc
nnc_limitedh79widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79widening3_SOURCES = limitedh79widening3.cc
nnc_limitedh79widening3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_limitedh79widening4_SOURCES = limitedh79widening4.cc
nnc_limitedh79widening4_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_linearpartition1_SOURCES = linearpartition1.cc
nnc_linearpartition1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_linearpartition2_SOURCES = linearpartition2.cc
nnc_linearpartition2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_linearpartition3_SOURCES = linearpartition3.cc
nnc_linearpartition3_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mc91_SOURCES = mc91.cc
nnc_mc91_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_randphull1_SOURCES = randphull1.cc
nnc_randphull1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_removedim1_SOURCES = removedim1.cc
nnc_removedim1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim2_SOURCES = removedim2.cc
nnc_removedim2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim3_SOURCES = removedim3.cc
nnc_removedim3_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim4_SOURCES = removedim4.cc
nnc_removedim4_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim5_SOURCES = removedim5.cc
nnc_removedim5_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim6_SOURCES = removedim6.cc
nnc_removedim6_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim8_SOURCES = removedim8.cc
nnc_removedim8_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_removedim9_SOURCES = removedim9.cc
nnc_removedim9_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_writepolyhedron2_SOURCES = writepolyhedron2.cc files.cc
nnc_writepolyhedron2_CXXFLAGS = $(DERIVED_CXXFLAGS)

watchdog1_SRCS = watchdog1.cc timings.cc timings.hh

if BUILD_WATCHDOG_LIBRARY
WATCHDOG_TESTS = watchdog1

watchdog1_SOURCES = $(watchdog1_SRCS)
watchdog1_LDADD = $(LDADD) $(top_builddir)/Watchdog/libpwl.la

endif

TESTS = $(ORIGINAL_TESTS) $(DERIVED_TESTS) $(WATCHDOG_TESTS)

XFAIL_TESTS = nnc_limitedh79widening3 nnc_polydifference1 addgenerators11

check_PROGRAMS = $(TESTS) $(BUGS)

noinst_HEADERS = ppl_test.hh ehandlers.hh files.hh print.hh

EXTRA_DIST = ehandlers.cc print.cc $(watchdog1_SRCS)

BUGS =

MOSTLYCLEANFILES = \
ascii_dump_load_1.dat \
ascii_dump_load_2.dat \
ascii_dump_load_3.dat \
writepolyhedron1.dat \
writepolyhedron2.dat
