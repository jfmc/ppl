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

CHECKER =
#CHECKER = valgrind --tool=memcheck -q --trace-children=yes --leak-check=yes --num-callers=100 --leak-resolution=high --suppressions=$(top_srcdir)/tests/valgrind_suppressions

TESTS_ENVIRONMENT = $(CHECKER)

AM_CPPFLAGS = \
-I$(top_builddir)/src \
-I$(top_builddir)/Watchdog \
-I$(top_builddir)/Watchdog/src \
-I$(top_srcdir)/utils \
-I$(top_srcdir)/tests \
@extra_includes@ \
@debug_flag@

LDADD = \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
$(top_builddir)/src/libppl.la \
@extra_libraries@

ORIGINAL_TESTS = \
addcongruence1 \
addcongruences1 \
addconstraint1 \
addconstraints1 addconstraints2 \
addgenerator1 addgenerator2 \
addgenerators1 addgenerators2 \
addspacedims1 addspacedims2 \
affineimage1 affineimage2 \
affinepreimage1 \
affinetrans \
append1 append2 \
ascii_dump_load1 ascii_dump_load2 ascii_dump_load3 \
bgp99extrapolation1 \
bhrz03widening1 bhrz03widening2 bhrz03widening3 \
bhz03widening1 \
bounded1 \
boundedaffineimage1 \
boundedaffinepreimage1 \
boundedbhrz03extrapolation1 \
boundedh79extrapolation1 \
boundingbox1 boundingbox2 boundingbox3 \
bounds1 \
cnncconversion1 \
concatenate1 \
congruences1 \
constraints1 \
contains1 contains2 \
disjoint1 disjoint2 \
dualhypercubes \
empty1 \
equals1 \
exceptions1 exceptions2  exceptions3 \
expandspacedim1 expandspacedim2 \
foldspacedims1 foldspacedims2 \
generalizedaffineimage1 generalizedaffineimage2 \
generalizedaffinepreimage1 generalizedaffinepreimage2 \
generators1 \
geomcovers1 \
h79widening1 h79widening2 \
intersection1 \
limitedbhrz03extrapolation1 \
limitedh79extrapolation1 \
linearpartition1 \
linexpression1 \
linearsystem1 \
mapspacedims1 \
matrix1 \
max_min1 \
maxspacedim1 \
mc91 \
membytes1 membytes2 \
memory1 memory2 \
minconstraints1 minconstraints2 \
mingenerators1 mingenerators2 \
nncminimize1 nncminimize2 \
numberinput1 \
onepoint \
permute \
polydifference1 polydifference2 \
polyhull1 polyhull2 \
pointsetpowerset1 \
powerset1 \
randphull1 randphull2 \
relations1 relations2 relations3 \
removespacedims1 removespacedims2 \
smm1 \
timeelapse1 timeelapse2 \
topclosed1 \
topclosure1 \
universe1 universe2 \
writeconsys1 \
writegensys1 \
writepolyhedron1 writepolyhedron2 \
writerelation1 \
writevariable1

DERIVED_TESTS = \
nnc_addcongruence1 \
nnc_addcongruences1 \
nnc_addconstraint1 \
nnc_addconstraints1 \
nnc_addgenerator1 \
nnc_addgenerators1 \
nnc_addspacedims1 \
nnc_affineimage1 \
nnc_affinepreimage1 \
nnc_affinetrans \
nnc_append1 nnc_append2 \
nnc_ascii_dump_load1 nnc_ascii_dump_load2 \
nnc_bgp99extrapolation1 \
nnc_bhrz03widening1 nnc_bhrz03widening2 \
nnc_bounded1 \
nnc_boundedaffineimage1 \
nnc_boundedaffinepreimage1 \
nnc_boundedbhrz03extrapolation1 \
nnc_boundedh79extrapolation1 \
nnc_boundingbox1 nnc_boundingbox2 \
nnc_bounds1 \
nnc_concatenate1 \
nnc_congruences1 \
nnc_constraints1 \
nnc_contains1 \
nnc_disjoint1 \
nnc_empty1 \
nnc_equals1 \
nnc_exceptions1 nnc_exceptions3 \
nnc_expandspacedim1 \
nnc_foldspacedims1 \
nnc_generalizedaffineimage1 \
nnc_generalizedaffinepreimage1 \
nnc_generators1 \
nnc_geomcovers1 \
nnc_h79widening1 \
nnc_intersection1 \
nnc_limitedbhrz03extrapolation1 \
nnc_limitedh79extrapolation1 \
nnc_linearpartition1 \
nnc_mapspacedims1 \
nnc_max_min1 \
nnc_mc91 \
nnc_membytes1 nnc_membytes2 \
nnc_minconstraints1 \
nnc_mingenerators1 \
nnc_onepoint \
nnc_permute \
nnc_polydifference1 \
nnc_polyhull1 \
nnc_randphull1 nnc_randphull2 \
nnc_relations1 nnc_relations2 \
nnc_removespacedims1 \
nnc_smm1 \
nnc_timeelapse1 \
nnc_universe1 \
nnc_writepolyhedron1

#
# Sources for the tests
#

addcongruence1_SOURCES = addcongruence1.cc

addcongruences1_SOURCES = addcongruences1.cc

addconstraint1_SOURCES = addconstraint1.cc

addconstraints1_SOURCES = addconstraints1.cc
addconstraints2_SOURCES = addconstraints2.cc

addgenerator1_SOURCES = addgenerator1.cc
addgenerator2_SOURCES = addgenerator2.cc

addgenerators1_SOURCES = addgenerators1.cc
addgenerators2_SOURCES = addgenerators2.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc

affinepreimage1_SOURCES = affinepreimage1.cc

affinetrans_SOURCES = affinetrans.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

ascii_dump_load1_SOURCES = ascii_dump_load1.cc
ascii_dump_load2_SOURCES = ascii_dump_load2.cc
ascii_dump_load3_SOURCES = ascii_dump_load3.cc

bgp99extrapolation1_SOURCES = bgp99extrapolation1.cc

bhrz03widening1_SOURCES = bhrz03widening1.cc
bhrz03widening2_SOURCES = bhrz03widening2.cc
bhrz03widening3_SOURCES = bhrz03widening3.cc

bhz03widening1_SOURCES = bhz03widening1.cc

bounded1_SOURCES = bounded1.cc

boundedaffineimage1_SOURCES = boundedaffineimage1.cc

boundedaffinepreimage1_SOURCES = boundedaffinepreimage1.cc

boundedbhrz03extrapolation1_SOURCES = boundedbhrz03extrapolation1.cc

boundedh79extrapolation1_SOURCES = boundedh79extrapolation1.cc

boundingbox1_SOURCES = boundingbox1.cc
boundingbox2_SOURCES = boundingbox2.cc
boundingbox3_SOURCES = boundingbox3.cc

bounds1_SOURCES = bounds1.cc

cnncconversion1_SOURCES = cnncconversion1.cc

concatenate1_SOURCES = concatenate1.cc

congruences1_SOURCES = congruences1.cc

constraints1_SOURCES = constraints1.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc

disjoint1_SOURCES = disjoint1.cc
disjoint2_SOURCES = disjoint2.cc

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

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

generators1_SOURCES = generators1.cc

geomcovers1_SOURCES = geomcovers1.cc

h79widening1_SOURCES = h79widening1.cc
h79widening2_SOURCES = h79widening2.cc

intersection1_SOURCES = intersection1.cc

limitedbhrz03extrapolation1_SOURCES = limitedbhrz03extrapolation1.cc

limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc

linearpartition1_SOURCES = linearpartition1.cc

linexpression1_SOURCES = linexpression1.cc

linearsystem1_SOURCES = linearsystem1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

matrix1_SOURCES = matrix1.cc

max_min1_SOURCES = max_min1.cc

maxspacedim1_SOURCES = maxspacedim1.cc

mc91_SOURCES = mc91.cc

membytes1_SOURCES = membytes1.cc
membytes2_SOURCES = membytes2.cc

memory1_SOURCES = memory1.cc
memory2_SOURCES = memory2.cc

minconstraints1_SOURCES = minconstraints1.cc
minconstraints2_SOURCES = minconstraints2.cc

mingenerators1_SOURCES = mingenerators1.cc
mingenerators2_SOURCES = mingenerators2.cc

numberinput1_SOURCES = numberinput1.cc

nncminimize1_SOURCES = nncminimize1.cc
nncminimize2_SOURCES = nncminimize2.cc

onepoint_SOURCES = onepoint.cc

permute_SOURCES = permute.cc

polydifference1_SOURCES = polydifference1.cc
polydifference2_SOURCES = polydifference2.cc

polyhull1_SOURCES = polyhull1.cc
polyhull2_SOURCES = polyhull2.cc

pointsetpowerset1_SOURCES = pointsetpowerset1.cc

powerset1_SOURCES = powerset1.cc

randphull1_SOURCES = randphull1.cc
randphull2_SOURCES = randphull2.cc
randphull2_LDADD = $(LDADD) -lm

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc
relations3_SOURCES = relations3.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc

smm1_SOURCES = smm1.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc

topclosed1_SOURCES = topclosed1.cc

topclosure1_SOURCES = topclosure1.cc

universe1_SOURCES = universe1.cc
universe2_SOURCES = universe2.cc

writeconsys1_SOURCES = writeconsys1.cc

writegensys1_SOURCES = writegensys1.cc

writepolyhedron1_SOURCES = writepolyhedron1.cc
writepolyhedron2_SOURCES = writepolyhedron2.cc

writerelation1_SOURCES = writerelation1.cc

writevariable1_SOURCES = writevariable1.cc

#
# Sources and compilation flags for the derived check programs
#

DERIVED_CXXFLAGS = -DDERIVED_TEST $(AM_CXXFLAGS)

nnc_addcongruence1_SOURCES = addcongruence1.cc
nnc_addcongruence1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addcongruences1_SOURCES = addcongruences1.cc
nnc_addcongruences1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addconstraint1_SOURCES = addconstraint1.cc
nnc_addconstraint1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addconstraints1_SOURCES = addconstraints1.cc
nnc_addconstraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addgenerator1_SOURCES = addgenerator1.cc
nnc_addgenerator1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addgenerators1_SOURCES = addgenerators1.cc
nnc_addgenerators1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_addspacedims1_SOURCES = addspacedims1.cc
nnc_addspacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_affineimage1_SOURCES = affineimage1.cc
nnc_affineimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_affinepreimage1_SOURCES = affinepreimage1.cc
nnc_affinepreimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_bgp99extrapolation1_SOURCES = bgp99extrapolation1.cc
nnc_bgp99extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bhrz03widening1_SOURCES = bhrz03widening1.cc
nnc_bhrz03widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_bhrz03widening2_SOURCES = bhrz03widening2.cc
nnc_bhrz03widening2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bounded1_SOURCES = bounded1.cc
nnc_bounded1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundedaffineimage1_SOURCES = boundedaffineimage1.cc
nnc_boundedaffineimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundedaffinepreimage1_SOURCES = boundedaffinepreimage1.cc
nnc_boundedaffinepreimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundedbhrz03extrapolation1_SOURCES = boundedbhrz03extrapolation1.cc
nnc_boundedvhrz03extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundedh79extrapolation1_SOURCES = boundedh79extrapolation1.cc
nnc_boundedh79extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_boundingbox1_SOURCES = boundingbox1.cc
nnc_boundingbox1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_boundingbox2_SOURCES = boundingbox2.cc
nnc_boundingbox2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_bounds1_SOURCES = bounds1.cc
nnc_bounds1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_concatenate1_SOURCES = concatenate1.cc
nnc_concatenate1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_congruences1_SOURCES = congruences1.cc
nnc_congruences1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_constraints1_SOURCES = constraints1.cc
nnc_constraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_contains1_SOURCES = contains1.cc
nnc_contains1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_disjoint1_SOURCES = disjoint1.cc
nnc_disjoint1_CXXFLAGS = $(DERIVED_CXXFLAGS)

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

nnc_generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
nnc_generalizedaffinepreimage1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_generators1_SOURCES = generators1.cc
nnc_generators1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_geomcovers1_SOURCES = geomcovers1.cc
nnc_geomcovers1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_h79widening1_SOURCES = h79widening1.cc
nnc_h79widening1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_intersection1_SOURCES = intersection1.cc
nnc_intersection1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_limitedbhrz03extrapolation1_SOURCES = limitedbhrz03extrapolation1.cc
nnc_limitedbhrz03extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_limitedh79extrapolation1_SOURCES = limitedh79extrapolation1.cc
nnc_limitedh79extrapolation1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_linearpartition1_SOURCES = linearpartition1.cc
nnc_linearpartition1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mapspacedims1_SOURCES = mapspacedims1.cc
nnc_mapspacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_max_min1_SOURCES = max_min1.cc
nnc_max_min1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mc91_SOURCES = mc91.cc
nnc_mc91_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_membytes1_SOURCES = membytes1.cc
nnc_membytes1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_membytes2_SOURCES = membytes2.cc
nnc_membytes2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_minconstraints1_SOURCES = minconstraints1.cc
nnc_minconstraints1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_mingenerators1_SOURCES = mingenerators1.cc
nnc_mingenerators1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_onepoint_SOURCES = onepoint.cc
nnc_onepoint_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_permute_SOURCES = permute.cc
nnc_permute_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_polydifference1_SOURCES = polydifference1.cc
nnc_polydifference1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_polyhull1_SOURCES = polyhull1.cc
nnc_polyhull1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_randphull1_SOURCES = randphull1.cc
nnc_randphull1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_randphull2_SOURCES = randphull2.cc
nnc_randphull2_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_randphull2_LDADD = $(LDADD) -lm

nnc_relations1_SOURCES = relations1.cc
nnc_relations1_CXXFLAGS = $(DERIVED_CXXFLAGS)
nnc_relations2_SOURCES = relations2.cc
nnc_relations2_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_removespacedims1_SOURCES = removespacedims1.cc
nnc_removespacedims1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_smm1_SOURCES = smm1.cc
nnc_smm1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_timeelapse1_SOURCES = timeelapse1.cc
nnc_timeelapse1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_universe1_SOURCES = universe1.cc
nnc_universe1_CXXFLAGS = $(DERIVED_CXXFLAGS)

nnc_writepolyhedron1_SOURCES = writepolyhedron1.cc
nnc_writepolyhedron1_CXXFLAGS = $(DERIVED_CXXFLAGS)

watchdog1_SRCS = watchdog1.cc

if BUILD_WATCHDOG_LIBRARY
WATCHDOG_TESTS = watchdog1

watchdog1_SOURCES = $(watchdog1_SRCS)
watchdog1_LDADD = $(LDADD) $(top_builddir)/Watchdog/src/libpwl.la

endif BUILD_WATCHDOG_LIBRARY

TESTS = $(ORIGINAL_TESTS) $(DERIVED_TESTS) $(WATCHDOG_TESTS)

XFAIL_ALWAYS =

XFAIL_WITH_INT64 = \
$(XFAIL_ALWAYS) \
randphull2 \
nnc_randphull2

XFAIL_WITH_INT32 = \
$(XFAIL_WITH_INT64)

XFAIL_WITH_INT16 = \
$(XFAIL_WITH_INT32)

XFAIL_WITH_INT8 = \
$(XFAIL_WITH_INT16) \
membytes1 \
randphull1 \
smm1 \
nnc_membytes1 \
nnc_randphull1 \
nnc_smm1

XFAIL_WITH_INT8_A = \
$(XFAIL_WITH_INT8) \
membytes2 \
nnc_membytes2

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
matrix1.dat \
linearsystem1.dat \
writeconsys1.dat \
writegensys1.dat \
writepolyhedron1.dat \
writepolyhedron2.dat \
writerelation1.dat \
writevariable1.dat
