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
addspacedims1 \
addspacedims2 \
affineimage1 \
affinepreimage1 \
concatenate1 \
congruence1 \
congruencesystem1 \
congruencesystem2 \
equals1 \
expandspacedim1 \
foldspacedims1 \
generalizedaffineimage1 \
generalizedaffineimage2 \
generalizedaffinepreimage1 \
generalizedaffinepreimage2 \
grid1 \
grid2 \
griddifference1 \
intersection1 \
intersection2 \
isuniverse1 \
join1 \
join2 \
limitedextrapolation1 \
mapspacedims1 \
relations1 \
relations2 \
removespacedims1 \
removespacedims2 \
timeelapse1 \
widening1 \
writecongruencesystem

#
# Sources for the tests
#

congruence1_SOURCES = congruence1.cc

writecongruencesystem_SOURCES = writecongruencesystem.cc

congruencesystem1_SOURCES = congruencesystem1.cc
congruencesystem2_SOURCES = congruencesystem2.cc

grid1_SOURCES = grid1.cc
grid2_SOURCES = grid2.cc

isuniverse1_SOURCES = isuniverse1.cc

addgenerator1_SOURCES = addgenerator1.cc

addcongruence1_SOURCES = addcongruence1.cc

addconstraints1_SOURCES = addconstraints1.cc

addspacedims1_SOURCES = addspacedims1.cc
addspacedims2_SOURCES = addspacedims2.cc

removespacedims1_SOURCES = removespacedims1.cc
removespacedims2_SOURCES = removespacedims2.cc

intersection1_SOURCES = intersection1.cc
intersection2_SOURCES = intersection2.cc

join1_SOURCES = join1.cc
join2_SOURCES = join2.cc

concatenate1_SOURCES = concatenate1.cc

mapspacedims1_SOURCES = mapspacedims1.cc

expandspacedim1_SOURCES = expandspacedim1.cc

equals1_SOURCES = equals1.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc

affineimage1_SOURCES = affineimage1.cc

affinepreimage1_SOURCES = affinepreimage1.cc

foldspacedims1_SOURCES = foldspacedims1.cc

generalizedaffineimage1_SOURCES = generalizedaffineimage1.cc
generalizedaffineimage2_SOURCES = generalizedaffineimage2.cc

generalizedaffinepreimage1_SOURCES = generalizedaffinepreimage1.cc
generalizedaffinepreimage2_SOURCES = generalizedaffinepreimage2.cc

griddifference1_SOURCES = griddifference1.cc

timeelapse1_SOURCES = timeelapse1.cc

widening1_SOURCES = widening1.cc

limitedextrapolation1_SOURCES = limitedextrapolation1.cc

XFAIL_TESTS =

check_PROGRAMS = $(TESTS)

MOSTLYCLEANFILES = \
writecongruencesystem.dat
