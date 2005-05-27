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
congruence1 \
writecongruencesystem \
congruencesystem1 \
congruencesystem2 \
grid1 \
grid2 \
isuniverse1 \
addspacedims1 \
addspacedims2 \
removespacedims1 \
removespacedims2 \
intersection1 \
intersection2 \
join1 \
join2 \
concatenate1 \
mapspacedims1 \
addgenerator1 \
addcongruence1

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

XFAIL_TESTS =

check_PROGRAMS = $(TESTS) $(BUGS)

BUGS = bug1

bug1_SOURCES = bug1.cc
bug1_CXXFLAGS = -DSRCDIR=\"$(srcdir)\"

MOSTLYCLEANFILES = \
writecongruencesystem.dat
