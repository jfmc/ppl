# Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

INCLUDES = -I$(top_builddir)/src @extra_includes@

LDADD = $(top_builddir)/src/libppl.la @gmp_library_option@

CPPFLAGS = @extra_cpp_flags@

TESTS = addconstraints1 addconstraints2 addconstraints3 \
	addgenerators1 addgenerators2 \
	affinetrans \
        append1 append2 \
        empty1 \
        error1 error2 error3 error4 error5 error6\
        inters1 inters2 \
	onepoint \
        permute \
	randchull1 \
	removedim1 removedim2 removedim3 \
	smm1

addconstraints1_SOURCES = addconstraints1.cc
addconstraints2_SOURCES = addconstraints2.cc
addconstraints3_SOURCES = addconstraints3.cc

addgenerators1_SOURCES = addgenerators1.cc
addgenerators2_SOURCES = addgenerators2.cc

affinetrans_SOURCES = affinetrans.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

empty1_SOURCES = empty1.cc

error1_SOURCES = error1.cc
error2_SOURCES = error2.cc
error3_SOURCES = error3.cc
error4_SOURCES = error4.cc
error5_SOURCES = error5.cc
error6_SOURCES = error6.cc

inters1_SOURCES = inters1.cc
inters2_SOURCES = inters2.cc

onepoint_SOURCES = onepoint.cc

permute_SOURCES = permute.cc

randchull1_SOURCES = randchull1.cc

removedim1_SOURCES = removedim1.cc
removedim2_SOURCES = removedim2.cc
removedim3_SOURCES = removedim3.cc

smm1_SOURCES = smm1.cc

check_PROGRAMS = $(TESTS) $(BUGS)

noinst_HEADERS = ehandlers.hh print.hh

BUGS = bug1 bug2 bug3 bug4 bug5 bug6

bug1_SOURCES = bug1.cc
bug2_SOURCES = bug2.cc
bug3_SOURCES = bug3.cc
bug4_SOURCES = bug4.cc
bug5_SOURCES = bug5.cc
bug6_SOURCES = bug6.cc
