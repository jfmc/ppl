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

SUBDIRS = parma_polyhedra_library jni . tests

interface_generator_files = \
ppl_interface_generator_java_procedure_generators.m4 \
ppl_interface_generator_java_dox.m4 \
ppl_interface_generator_java_dox_code.m4 \
ppl_java_common_dox

EXTRA_DIST = \
$(interface_generator_files)

interface_generator_dependencies = \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
../ppl_interface_generator_common_procedure_generators.m4 \
ppl_interface_generator_java_procedure_generators.m4 \
ppl_interface_generator_java_dox.m4 \
ppl_interface_generator_java_dox_code.m4

Java_interface.dox: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_java_dox.m4 \
                > Java_interface.dox

dist_noinst_DATA = \
Java_interface.dox

if BUILD_JAVA_INTERFACE

jardir = $(pkglibdir)
jar_DATA = ppl_java.jar

ppl_java.jar: parma_polyhedra_library/java_classes.stamp
	$(JAR) cf ppl_java.jar parma_polyhedra_library/*.class

endif BUILD_JAVA_INTERFACE

CLEANFILES = \
ppl_java.jar \
Java_interface.dox
