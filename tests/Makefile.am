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

TESTS = $(WATCHDOG_TESTS) \
	NNCinclusion1 \
	NNCminimize1 NNCminimize2 NNCminimize3 NNCminimize4 NNCminimize5 \
        NNCminimize6 \
        addconstraints1 addconstraints2 addconstraints3 addconstraints4 \
	addconstraints5 addconstraints6 addconstraints7 addconstraints8 \
	addconstraints9 addconstraints10 addconstraints11 \
	adddimensions1 adddimensions2 adddimensions3 adddimensions4 \
	adddimensions5 adddimensions6 adddimensions7 \
	addgenerators1 addgenerators2 addgenerators3 addgenerators4 \
	addgenerators5 addgenerators6 addgenerators7 \
	affineimage1 affineimage2 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinetrans \
        append1 append2 \
	bounded1 \
	boundingbox1 boundingbox2 \
        empty1 \
	exceptions1 exceptions2 \
	generators1 generators2 \
	inclusion1 \
        inters1 inters2 inters3 inters4 inters5 inters6 inters7 \
	limitedwidening1 limitedwidening2\
	memory1 \
	onepoint \
        permute \
	polydifference1 \
	polyhull1 polyhull2 polyhull3 polyhull4 \
	randphull1 \
	relations1 relations2 relations3 relations4 relations5 relations6 \
	relations7 relations8 relations9 \
	removedim1 removedim2 removedim3 removedim4 removedim5 removedim6 \
	removedim7 \
	smm1 \
	timeelapse1 timeelapse2 timeelapse3 timeelapse4 timeelapse5 \
	universe1 universe2 universe3 universe4 universe5 universe6 \
	widening1 widening2

NNCinclusion1_SOURCES = NNCinclusion1.cc

NNCminimize1_SOURCES = NNCminimize1.cc
NNCminimize2_SOURCES = NNCminimize2.cc
NNCminimize3_SOURCES = NNCminimize3.cc
NNCminimize4_SOURCES = NNCminimize4.cc
NNCminimize5_SOURCES = NNCminimize5.cc
NNCminimize6_SOURCES = NNCminimize6.cc

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

adddimensions1_SOURCES = adddimensions1.cc
adddimensions2_SOURCES = adddimensions2.cc
adddimensions3_SOURCES = adddimensions3.cc
adddimensions4_SOURCES = adddimensions4.cc
adddimensions5_SOURCES = adddimensions5.cc
adddimensions6_SOURCES = adddimensions6.cc
adddimensions7_SOURCES = adddimensions7.cc

addgenerators1_SOURCES = addgenerators1.cc
addgenerators2_SOURCES = addgenerators2.cc
addgenerators3_SOURCES = addgenerators3.cc
addgenerators4_SOURCES = addgenerators4.cc
addgenerators5_SOURCES = addgenerators5.cc
addgenerators6_SOURCES = addgenerators6.cc
addgenerators7_SOURCES = addgenerators7.cc

affineimage1_SOURCES = affineimage1.cc
affineimage2_SOURCES = affineimage2.cc

affinepreimage1_SOURCES = affinepreimage1.cc
affinepreimage2_SOURCES = affinepreimage2.cc
affinepreimage3_SOURCES = affinepreimage3.cc
affinepreimage4_SOURCES = affinepreimage4.cc

affinetrans_SOURCES = affinetrans.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

bounded1_SOURCES = bounded1.cc

boundingbox1_SOURCES = boundingbox1.cc
boundingbox2_SOURCES = boundingbox2.cc

empty1_SOURCES = empty1.cc

exceptions1_SOURCES = exceptions1.cc
exceptions2_SOURCES = exceptions2.cc

generators1_SOURCES = generators1.cc
generators2_SOURCES = generators2.cc

inclusion1_SOURCES = inclusion1.cc

inters1_SOURCES = inters1.cc
inters2_SOURCES = inters2.cc
inters3_SOURCES = inters3.cc
inters4_SOURCES = inters4.cc
inters5_SOURCES = inters5.cc
inters6_SOURCES = inters6.cc
inters7_SOURCES = inters7.cc

limitedwidening1_SOURCES = limitedwidening1.cc
limitedwidening2_SOURCES = limitedwidening2.cc

memory1_SOURCES = memory1.cc

onepoint_SOURCES = onepoint.cc

permute_SOURCES = permute.cc

polydifference1_SOURCES = polydifference1.cc

polyhull1_SOURCES = polyhull1.cc
polyhull2_SOURCES = polyhull2.cc
polyhull3_SOURCES = polyhull3.cc
polyhull4_SOURCES = polyhull4.cc

relations1_SOURCES = relations1.cc
relations2_SOURCES = relations2.cc
relations3_SOURCES = relations3.cc
relations4_SOURCES = relations4.cc
relations5_SOURCES = relations5.cc
relations6_SOURCES = relations6.cc
relations7_SOURCES = relations7.cc
relations8_SOURCES = relations8.cc
relations9_SOURCES = relations9.cc

randphull1_SOURCES = randphull1.cc

removedim1_SOURCES = removedim1.cc
removedim2_SOURCES = removedim2.cc
removedim3_SOURCES = removedim3.cc
removedim4_SOURCES = removedim4.cc
removedim5_SOURCES = removedim5.cc
removedim6_SOURCES = removedim6.cc
removedim7_SOURCES = removedim7.cc

smm1_SOURCES = smm1.cc

timeelapse1_SOURCES = timeelapse1.cc
timeelapse2_SOURCES = timeelapse2.cc
timeelapse3_SOURCES = timeelapse3.cc
timeelapse4_SOURCES = timeelapse4.cc
timeelapse5_SOURCES = timeelapse5.cc

universe1_SOURCES = universe1.cc
universe2_SOURCES = universe2.cc
universe3_SOURCES = universe3.cc
universe4_SOURCES = universe4.cc
universe5_SOURCES = universe5.cc
universe6_SOURCES = universe6.cc

widening1_SOURCES = widening1.cc
widening2_SOURCES = widening2.cc

if BUILD_WATCHDOG_LIBRARY
WATCHDOG_TESTS = watchdog1

watchdog1_SOURCES = watchdog1.cc timings.cc timings.hh
watchdog1_LDADD = $(LDADD) $(top_builddir)/Watchdog/libpwl.la

endif

check_PROGRAMS = $(TESTS) $(BUGS)

noinst_HEADERS = ehandlers.hh print.hh

EXTRA_DIST = ehandlers.cc print.cc

BUGS =
