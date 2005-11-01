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
-D NOISY \
-I$(top_srcdir)/src -I$(top_srcdir)/tests \
-I$(top_builddir)/src \
-I$(top_srcdir)/utils \
@extra_includes@

LDADD = \
$(top_builddir)/src/libppl.la \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
@extra_libraries@

TESTS = \
addcongruence1 \
addconstraints1 \
addgenerator1 \
addspacedims1 addspacedims2 \
affineimage1 \
affinepreimage1 \
bhz03widening1 \
boundingbox1 boundingbox2 \
certificate1 \
concatenate1 \
congruence1 \
congruencesystem1 congruencesystem2 \
contains1 contains2 \
equals1 \
expandspacedim1 \
foldspacedims1 \
generalizedaffineimage1 generalizedaffineimage2 \
generalizedaffinepreimage1 generalizedaffinepreimage2 \
coveringbox1 coveringbox2 \
grid1 grid2 \
griddifference1 \
intersection1 intersection2 \
isuniverse1 \
join1 join2 join3 \
limitedextrapolation1 \
mapspacedims1 \
relations1 relations2 \
removespacedims1 removespacedims2 \
timeelapse1 \
topclosed1 \
widening1 \
writecongruencesystem

#
# Sources for the tests
#

addcongruence1_SOURCES = addcongruence1.cc

addconstraints1_SOURCES = addconstraints1.cc

addgenerator1_SOURCES = addgenerator1.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc

affineimage1_SOURCES = affineimage1.cc

affinepreimage1_SOURCES = affinepreimage1.cc

bhz03widening1_SOURCES = bhz03widening1.cc

boundingbox1_SOURCES = boundingbox1.cc
boundingbox2_SOURCES = boundingbox2.cc

certificate1_SOURCES = certificate1.cc

concatenate1_SOURCES = concatenate1.cc

congruence1_SOURCES = congruence1.cc

congruencesystem1_SOURCES = congruencesystem1.cc
congruencesystem2_SOURCES = congruencesystem2.cc

contains1_SOURCES = contains1.cc
contains2_SOURCES = contains2.cc

equals1_SOURCES = equals1.cc

expandspacedim1_SOURCES = expandspacedim1.cc

foldspacedims1_SOURCES = foldspacedims1.cc

generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

coveringbox1_SOURCES = coveringbox1.cc
coveringbox2_SOURCES = coveringbox2.cc

grid1_SOURCES = grid1.cc
grid2_SOURCES = grid2.cc

griddifference1_SOURCES = griddifference1.cc

intersection1_SOURCES = intersection1.cc
intersection2_SOURCES = intersection2.cc

isuniverse1_SOURCES = isuniverse1.cc

join1_SOURCES = join1.cc
join2_SOURCES = join2.cc
join3_SOURCES = join3.cc

limitedextrapolation1_SOURCES = limitedextrapolation1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc

timeelapse1_SOURCES = timeelapse1.cc

topclosed1_SOURCES = topclosed1.cc

widening1_SOURCES = widening1.cc

writecongruencesystem_SOURCES = writecongruencesystem.cc

XFAIL_ALWAYS =

XFAIL_WITH_INT64 = \
$(XFAIL_ALWAYS)

XFAIL_WITH_INT32 = \
$(XFAIL_WITH_INT64) \
grid1

XFAIL_WITH_INT16 = \
$(XFAIL_WITH_INT32)

XFAIL_WITH_INT8 = \
$(XFAIL_WITH_INT16)

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
