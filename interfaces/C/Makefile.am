# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
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

interface_generator_files = \
ppl_interface_generator_c_procedure_generators.m4 \
ppl_interface_generator_c_h.m4 \
ppl_interface_generator_c_h_code.m4 \
ppl_interface_generator_c_h_preamble \
ppl_interface_generator_c_cc.m4 \
ppl_interface_generator_c_cc_files.m4 \
ppl_interface_generator_c_cc_code.m4 \
ppl_interface_generator_c_cc_preamble

EXTRA_DIST = \
$(interface_generator_files) \
C_interface.dox

if BUILD_C_INTERFACE

# Libtool -version-info for libppl_c.la.
#
#  1. Start with version information of `0:0:0' for each Libtool library.
#
#  2. Update the version information only immediately before a public
#     release of your software.  More frequent updates are unnecessary,
#     and only guarantee that the current interface number gets larger
#     faster.
#
#  3. If the library source code has changed at all since the last
#     update, then increment REVISION (`C:R:A' becomes `C:r+1:A').
#
#  4. If any interfaces have been added, removed, or changed since the
#     last update, increment CURRENT, and set REVISION to 0.
#
#  5. If any interfaces have been added since the last public release,
#     then increment AGE.
#
#  6. If any interfaces have been removed since the last public release,
#     then set AGE to 0.
#
#	PPL release		-version-info
#	    0.1			    -----
#	    0.2			    -----
#	    0.3			    -----
#	    0.4			    0:0:0
#	    0.5			    1:0:0

LIBPPL_C_LT_CURRENT =  1
LIBPPL_C_LT_REVISION = 0
LIBPPL_C_LT_AGE =      1

BUILT_SOURCES =
# ppl_c.h
# ppl_c.cc
# ppl_c_domains.cc

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
ppl_interface_generator_c_cc_files.m4 \
../ppl_interface_generator_common_procedure_generators.m4 \
$(interface_generator_files)

ppl_c.h: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_c_h.m4 > $@

ppl_c.cc: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_c_cc.m4 > $@

ppl_c_domains.cc: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_c_cc_files.m4 \
                > ppl_c_cc_blob
	$(top_srcdir)/utils/cm_cleaner.sh < ppl_c_cc_blob
	$(top_srcdir)/utils/cm_splitter.sh < ppl_c_cc_blob
	rm -f ppl_c_cc_blob

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces \
-I$(top_builddir)/interfaces/C \
-I$(top_builddir)/src \
@extra_includes@

lib_LTLIBRARIES = libppl_c.la

libppl_c_la_SOURCES = \
ppl_c_implementation.defs.hh \
ppl_c_implementation.inlines.hh \
ppl_c_implementation.cc

nodist_libppl_c_la_SOURCES = \
ppl_c.cc

nodist_EXTRA_libppl_c_la_SOURCES = \
ppl_c_BD_Shape_double.cc \
ppl_c_BD_Shape_mpq_class.cc \
ppl_c_BD_Shape_mpz_class.cc \
ppl_c_Constraints_Product_C_Polyhedron_Grid.cc \
ppl_c_Double_Box.cc \
ppl_c_Grid.cc \
ppl_c_Octagonal_Shape_double.cc \
ppl_c_Octagonal_Shape_mpq_class.cc \
ppl_c_Octagonal_Shape_mpz_class.cc \
ppl_c_Pointset_Powerset_C_Polyhedron.cc \
ppl_c_Pointset_Powerset_NNC_Polyhedron.cc \
ppl_c_Polyhedron.cc \
ppl_c_Rational_Box.cc

if NO_UNDEFINED

NO_UNDEFINED_FLAG = -no-undefined

endif NO_UNDEFINED

libppl_c_la_LIBADD = \
@required_instantiations_c_cxx_objects@ \
$(top_builddir)/src/libppl.la \
@extra_libraries@

libppl_c_la_DEPENDENCIES = \
@required_instantiations_c_cxx_objects@

libppl_c_la_LDFLAGS = \
$(NO_UNDEFINED_FLAG) \
-version-info $(LIBPPL_C_LT_CURRENT):$(LIBPPL_C_LT_REVISION):$(LIBPPL_C_LT_AGE)

nodist_include_HEADERS = ppl_c.h ppl_c_version.h

endif BUILD_C_INTERFACE

DISTCLEANFILES = \
ppl_c.h \
ppl_c.cc
