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

CHECKER =
#CHECKER = valgrind --tool=memcheck -q --trace-children=yes --leak-check=yes --num-callers=100 --leak-resolution=high --suppressions=$(top_srcdir)/tests/valgrind_suppressions

TESTS_ENVIRONMENT = $(CHECKER)

AM_CPPFLAGS = \
-I$(top_builddir)/src \
-I$(top_srcdir)/src -I$(top_srcdir)/tests \
-I$(top_srcdir)/utils \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

TESTS = \
addcongruence1 \
addcongruences1 \
addconstraint1 \
addconstraints1 \
addgenerator1 \
addgenerators1 \
addspacedims1 addspacedims2 \
affinedim1 \
affineimage1 affineimage1_16 \
affinepreimage1 \
asciidumpload1 \
bhz03widening1 \
bounded1 \
boundingbox1 boundingbox2 \
bounds1 \
certificate1 \
concatenate1 \
congruence1 \
congruences1 \
congruencesystem1 congruencesystem2 \
contains1 contains2 \
copyconstruct1 \
discrete1 \
disjoint1 \
equals1 \
expandspacedim1 \
foldspacedims1 \
generalizedaffineimage1 generalizedaffineimage2 \
generalizedaffinepreimage1 generalizedaffinepreimage2 \
generator1 \
generators1 \
coveringbox1 coveringbox2 \
grid1 grid1_gmp grid1_64 grid1_16 grid2 grid2_16 grid3 \
griddifference1 \
intersection1 intersection2 \
isempty1 \
isuniverse1 \
join1 join1_16 join2 join3 \
limitedextrapolation1 \
mapspacedims1 \
maxmin1 \
membytes1 membytes2 \
mincongruences1 \
mingenerators1 \
outputoperator1 \
relations1 relations2 \
removespacedims1 removespacedims2 \
timeelapse1 timeelapse2 \
topclosed1 \
topclosure1 \
widening1 \
writecongruencesystem

#
# Sources for the tests
#

addcongruence1_SOURCES = addcongruence1.cc

addcongruences1_SOURCES = addcongruences1.cc

addconstraint1_SOURCES = addconstraint1.cc

addconstraints1_SOURCES = addconstraints1.cc

addgenerator1_SOURCES = addgenerator1.cc

addgenerators1_SOURCES = addgenerators1.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc

affinedim1_SOURCES = affinedim1.cc

affineimage1_SOURCES = affineimage1.cc

affineimage1_16_SOURCES = affineimage1_16.cc

affinepreimage1_SOURCES = affinepreimage1.cc

asciidumpload1_SOURCES = asciidumpload1.cc

bhz03widening1_SOURCES = bhz03widening1.cc

bounded1_SOURCES = bounded1.cc

boundingbox1_SOURCES = boundingbox1.cc
boundingbox2_SOURCES = boundingbox2.cc

bounds1_SOURCES = bounds1.cc

certificate1_SOURCES = certificate1.cc

concatenate1_SOURCES = concatenate1.cc

congruence1_SOURCES = congruence1.cc

congruences1_SOURCES = congruences1.cc

congruencesystem1_SOURCES = congruencesystem1.cc
congruencesystem2_SOURCES = congruencesystem2.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc

copyconstruct1_SOURCES = copyconstruct1.cc

discrete1_SOURCES = discrete1.cc

disjoint1_SOURCES = disjoint1.cc

equals1_SOURCES = equals1.cc

expandspacedim1_SOURCES = expandspacedim1.cc

foldspacedims1_SOURCES = foldspacedims1.cc

generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

generator1_SOURCES = generator1.cc

generators1_SOURCES = generators1.cc

coveringbox1_SOURCES = coveringbox1.cc
coveringbox2_SOURCES = coveringbox2.cc

grid1_SOURCES = grid1.cc
grid1_gmp_SOURCES = grid1_gmp.cc
grid1_16_SOURCES = grid1_16.cc
grid1_64_SOURCES = grid1_64.cc
grid2_SOURCES = grid2.cc
grid2_16_SOURCES = grid2_16.cc
grid3_SOURCES = grid3.cc

griddifference1_SOURCES = griddifference1.cc

intersection1_SOURCES = intersection1.cc
intersection2_SOURCES = intersection2.cc

isempty1_SOURCES = isempty1.cc

isuniverse1_SOURCES = isuniverse1.cc

join1_SOURCES = join1.cc
join1_16_SOURCES = join1_16.cc
join2_SOURCES = join2.cc
join3_SOURCES = join3.cc

limitedextrapolation1_SOURCES = limitedextrapolation1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

maxmin1_SOURCES = maxmin1.cc

membytes1_SOURCES = membytes1.cc
membytes2_SOURCES = membytes2.cc

mincongruences1_SOURCES = mincongruences1.cc

mingenerators1_SOURCES = mingenerators1.cc

outputoperator1_SOURCES = outputoperator1.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc

timeelapse1_SOURCES = timeelapse1.cc

timeelapse2_SOURCES = timeelapse2.cc

topclosed1_SOURCES = topclosed1.cc

topclosure1_SOURCES = topclosure1.cc

widening1_SOURCES = widening1.cc

writecongruencesystem_SOURCES = writecongruencesystem.cc

XFAIL_ALWAYS =

XFAIL_WITH_INT64 = \
$(XFAIL_ALWAYS) \
grid1_gmp

XFAIL_WITH_INT32 = \
$(XFAIL_WITH_INT64) \
grid1_64

XFAIL_WITH_INT16 = \
$(XFAIL_WITH_INT32)

XFAIL_WITH_INT8 = \
$(XFAIL_WITH_INT16) \
grid1_16 \
grid2_16 \
join1_16 \
affineimage1_16 \
timeelapse2

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

XFAIL_SET = $(XFAIL_WITH_INT8)

else !USE_INT8

XFAIL_SET = $(XFAIL_ALWAYS)

endif !USE_INT8
endif !USE_INT16
endif !USE_INT32
endif !USE_INT64

XFAIL_TESTS = $(XFAIL_SET)

check_PROGRAMS = $(TESTS)

MOSTLYCLEANFILES = \
writecongruencesystem.dat
