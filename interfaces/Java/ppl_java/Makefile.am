# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

JAVAROOT = ../

if BUILD_JAVA_INTERFACE

interface_generator_files = \
ppl_interface_generator_java_classes_java.m4 \
ppl_interface_generator_java_classes_java_code.m4

interface_generator_dependencies = \
../../ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../ppl_interface_generator_java_dat.m4 \
ppl_interface_generator_java_classes_java.m4 \
ppl_interface_generator_java_classes_java_code.m4

# In general, several java sources will be built and the same process
# will build all of them.  We assume that Polyhedron.java will always
# be built, independently from all configuration options.

BUILT_SOURCES = Polyhedron.java

Polyhedron.java: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_java_classes_java.m4 \
			> java_blob
	$(top_srcdir)/utils/cm_cleaner.sh < java_blob
	$(top_srcdir)/utils/cm_splitter.sh < java_blob
	rm -f java_blob

java_classes = \
By_Reference.java \
C_Polyhedron.java \
Coefficient.java \
Congruence.java \
Congruence_System.java \
Constraint.java \
Constraint_System.java \
Constraint_System.java \
Degenerate_Element.java \
Generator.java \
Generator_System.java \
Generator_Type.java \
Grid_Generator.java \
Grid_Generator_Type.java \
Grid_Generator_System.java \
Linear_Expression.java \
Linear_Expression_Coefficient.java \
Linear_Expression_Difference.java \
Linear_Expression_Sum.java \
Linear_Expression_Times.java \
Linear_Expression_Unary_Minus.java \
Linear_Expression_Variable.java \
PPL_Object.java \
Partial_Function.java \
Poly_Con_Relation.java \
Poly_Gen_Relation.java \
Polyhedron.java \
Relation_Symbol.java \
Variable.java \
Variables_Set.java

EXTRA_DIST = \
$(interface_generator_files) \
$(java_classes)

noinst_JAVA = $(java_classes)

endif BUILD_JAVA_INTERFACE
