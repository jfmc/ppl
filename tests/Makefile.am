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

INCLUDES = -I$(top_builddir)/src -I$(top_srcdir)/src/BitSet -I$(top_srcdir)/src/Integer

LDADD = $(top_builddir)/src/libppl.a $(top_builddir)/src/Integer/libInteger.a $(top_builddir)/src/BitSet/libBitSet.a -lgmp

CPPFLAGS = -DPPL_INTERNAL=private @extra_cpp_flags@

TESTS = inters1 inters2 randchull1

inters1_SOURCES = inters1.cc
inters2_SOURCES = inters2.cc

randchull1_SOURCES = randchull1.cc

check_PROGRAMS = $(TESTS)
