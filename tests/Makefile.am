# automake source file for the PPL.
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

INCLUDES = -I$(top_builddir)/src @extra_includes@

LDADD = $(top_builddir)/src/libppl.la @extra_libraries@

CPPFLAGS = @extra_cpp_flags@

TESTS = addconstraints1 addconstraints2 addconstraints3 addconstraints4 \
	addconstraints5 addconstraints6 addconstraints7 addconstraints8 \
	addconstraints9 addconstraints10 addconstraints11 \
	adddimensions1 adddimensions2 adddimensions3 adddimensions4 \
	addgenerators1 addgenerators2 addgenerators3 addgenerators4 \
	addgenerators5 addgenerators6 \
	affineimage1 affineimage2 \
	affinepreimage1 affinepreimage2 affinepreimage3 affinepreimage4 \
	affinetrans \
        append1 append2 \
	bounded1 \
	convexhull1 convexhull2 convexhull3 \
	convexhull1 \
	convexdifference1 \
        empty1 \
	exceptions1 \
	generators1 \
	includes1 includes2 includes3 includes4 \
        inters1 inters2 inters3 inters4 inters5 inters6 \
	limitedwidening1 limitedwidening2\
	onepoint \
        permute \
	randchull1 \
	removedim1 removedim2 removedim3 removedim4 removedim5 removedim6 \
	satisfies1 satisfies2 satisfies3 satisfies4 \
	smm1 \
	universe1 universe2 universe3 universe4 universe5

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

addgenerators1_SOURCES = addgenerators1.cc
addgenerators2_SOURCES = addgenerators2.cc
addgenerators3_SOURCES = addgenerators3.cc
addgenerators4_SOURCES = addgenerators4.cc
addgenerators5_SOURCES = addgenerators5.cc
addgenerators6_SOURCES = addgenerators6.cc

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

convexhull1_SOURCES = convexhull1.cc
convexhull2_SOURCES = convexhull2.cc
convexhull3_SOURCES = convexhull3.cc

convexdifference1_SOURCES = convexdifference1.cc

empty1_SOURCES = empty1.cc

exceptions1_SOURCES = exceptions1.cc

generators1_SOURCES = generators1.cc

includes1_SOURCES = includes1.cc
includes2_SOURCES = includes2.cc
includes3_SOURCES = includes3.cc
includes4_SOURCES = includes4.cc

inters1_SOURCES = inters1.cc
inters2_SOURCES = inters2.cc
inters3_SOURCES = inters3.cc
inters4_SOURCES = inters4.cc
inters5_SOURCES = inters5.cc
inters6_SOURCES = inters6.cc

limitedwidening1_SOURCES = limitedwidening1.cc
limitedwidening2_SOURCES = limitedwidening2.cc

onepoint_SOURCES = onepoint.cc

permute_SOURCES = permute.cc

randchull1_SOURCES = randchull1.cc

removedim1_SOURCES = removedim1.cc
removedim2_SOURCES = removedim2.cc
removedim3_SOURCES = removedim3.cc
removedim4_SOURCES = removedim4.cc
removedim5_SOURCES = removedim5.cc
removedim6_SOURCES = removedim6.cc

satisfies1_SOURCES = satisfies1.cc
satisfies2_SOURCES = satisfies2.cc
satisfies3_SOURCES = satisfies3.cc
satisfies4_SOURCES = satisfies4.cc

smm1_SOURCES = smm1.cc

universe1_SOURCES = universe1.cc
universe2_SOURCES = universe2.cc
universe3_SOURCES = universe3.cc
universe4_SOURCES = universe4.cc
universe5_SOURCES = universe5.cc

check_PROGRAMS = $(TESTS) $(BUGS)

noinst_HEADERS = ehandlers.hh print.hh

BUGS =
