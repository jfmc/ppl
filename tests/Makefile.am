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

INCLUDES = -I$(top_builddir)/src

LDADD = $(top_builddir)/src/libppl.a
LDFLAGS = @gmp_library_option@

CPPFLAGS = @extra_cpp_flags@

TESTS = inters1 inters2 \
        randchull1 \
        empty1 \
        append1 append2 \
        permute \
        error1 error2 error3

inters1_SOURCES = inters1.cc
inters2_SOURCES = inters2.cc

randchull1_SOURCES = randchull1.cc

empty1_SOURCES = empty1.cc

error1_SOURCES = error1.cc
error2_SOURCES = error2.cc
error3_SOURCES = error3.cc

append1_SOURCES = append1.cc
append2_SOURCES = append2.cc

permute_SOURCES = permute.cc

check_PROGRAMS = $(TESTS)

noinst_HEADERS = ehandlers.hh print.hh
