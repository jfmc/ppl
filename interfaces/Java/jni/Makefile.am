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



if BUILD_JAVA_INTERFACE

interface_generator_files = \
ppl_interface_generator_java_classes_cc_files.m4 \
ppl_interface_generator_java_classes_cc_code.m4 \
ppl_interface_generator_java_ppl_java_classes_cc.m4 \
ppl_interface_generator_java_ppl_java_classes_cc_code.m4

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../../ppl_interface_generator_common_procedure_generators.m4 \
../ppl_interface_generator_java_procedure_generators.m4 \
$(interface_generator_files)

AM_CPPFLAGS = \
-I$(top_srcdir)/interfaces \
-I$(top_builddir)/src \
-I../parma_polyhedra_library \
-I$(top_srcdir)/src \
@extra_includes@ @JNIFLAGS@

lib_LTLIBRARIES = libppl_java.la

if HOST_OS_DARWIN
libppl_java_la_LDFLAGS=-shrext .jnilib
endif HOST_OS_DARWIN

libppl_java_la_SOURCES = \
ppl_java_common.hh \
ppl_java_common.cc \
ppl_java_globals.cc

nodist_libppl_java_la_SOURCES = \
ppl_java_classes.cc

ppl_java_classes.cc: $(interface_generator_dependencies)
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_java_ppl_java_classes_cc.m4 \
			> $@

ppl_java_implementation_domains: $(interface_generator_dependencies)
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_java_classes_cc_files.m4 \
                > ppl_java_cc_blob
	$(top_srcdir)/utils/cm_cleaner.sh < ppl_java_cc_blob
	$(top_srcdir)/utils/cm_splitter.sh < ppl_java_cc_blob
	rm -f ppl_java_cc_blob
	touch $@

EXTRA_DIST = \
$(interface_generator_files)

libppl_java_la_LIBADD = $(top_builddir)/src/libppl.la @extra_libraries@

CLEANFILES = \
ppl_java_classes.cc

endif BUILD_JAVA_INTERFACE
