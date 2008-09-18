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

JAVAROOT = ../

required_instantiations_java_sources = \
@required_instantiations_java_sources@

required_instantiations_java_cxx_headers = \
@required_instantiations_java_cxx_headers@

interface_generator_files = \
ppl_interface_generator_java_classes_java.m4 \
ppl_interface_generator_java_classes_java_code.m4

fixed_java_cxx_headers = \
parma_polyhedra_library.Congruence \
parma_polyhedra_library.Congruence_System \
parma_polyhedra_library.Constraint \
parma_polyhedra_library.Constraint_System \
parma_polyhedra_library.Generator \
parma_polyhedra_library.Generator_System \
parma_polyhedra_library.Grid_Generator \
parma_polyhedra_library.Grid_Generator_System \
parma_polyhedra_library.Linear_Expression \
parma_polyhedra_library.MIP_Problem \
parma_polyhedra_library.Parma_Polyhedra_Library

fixed_java_cxx_headers_sources = \
parma_polyhedra_library_Congruence.h \
parma_polyhedra_library_Congruence_System.h \
parma_polyhedra_library_Constraint.h \
parma_polyhedra_library_Constraint_System.h \
parma_polyhedra_library_Generator.h \
parma_polyhedra_library_Generator_System.h \
parma_polyhedra_library_Grid_Generator.h \
parma_polyhedra_library_Grid_Generator_System.h \
parma_polyhedra_library_Linear_Expression.h \
parma_polyhedra_library_MIP_Problem.h \
parma_polyhedra_library_Parma_Polyhedra_Library.h

fixed_java_sources = \
By_Reference.java \
Coefficient.java \
Congruence.java \
Congruence_System.java \
Constraint.java \
Constraint_System.java \
Constraint_System.java \
Degenerate_Element.java \
Domain_Error_Exception.java \
Generator.java \
Generator_System.java \
Generator_Type.java \
Grid_Generator.java \
Grid_Generator_System.java \
Grid_Generator_Type.java \
Invalid_Argument_Exception.java \
Length_Error_Exception.java \
Linear_Expression.java \
Linear_Expression_Coefficient.java \
Linear_Expression_Difference.java \
Linear_Expression_Sum.java \
Linear_Expression_Times.java \
Linear_Expression_Unary_Minus.java \
Linear_Expression_Variable.java \
Logic_Error_Exception.java \
MIP_Problem.java \
MIP_Problem_Status.java \
Optimization_Mode.java \
Overflow_Error_Exception.java \
PPL_Object.java \
Parma_Polyhedra_Library.java \
Pair.java \
Partial_Function.java \
Poly_Con_Relation.java \
Poly_Gen_Relation.java \
Relation_Symbol.java \
Variable.java \
Variables_Set.java

all_java_sources = \
$(fixed_java_sources) \
$(required_instantiations_java_sources)

EXTRA_DIST = \
$(interface_generator_files) \
$(fixed_java_sources)

if BUILD_JAVA_INTERFACE

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../../ppl_interface_generator_common_procedure_generators.m4 \
../ppl_interface_generator_java_procedure_generators.m4 \
$(interface_generator_files)

BUILT_SOURCES = \
java_classes.stamp \
java_cxx_headers.stamp


java_classes.stamp: $(all_java_sources)
	@if test -n "$?"; then \
		echo '$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(AM_JAVACFLAGS) $(JAVACFLAGS) $?' ; \
		$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) \
		$(AM_JAVACFLAGS) $(JAVACFLAGS) $?; \
	else :; fi
	echo timestamp >$@

java_cxx_headers.stamp: java_classes.stamp
	$(JAVAH) -classpath .. \
		$(fixed_java_cxx_headers) \
		$(required_instantiations_java_cxx_headers)
	echo timestamp >$@

source_generation.stamp: $(interface_generator_dependencies)
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_java_classes_java.m4 \
			> java_blob
	$(top_srcdir)/utils/cm_cleaner.sh < java_blob
	$(top_srcdir)/utils/cm_splitter.sh < java_blob
	rm -f java_blob
	echo timestamp >$@

$(required_instantiations_java_sources): source_generation.stamp

endif BUILD_JAVA_INTERFACE

CLEANFILES = \
$(required_instantiations_java_sources) \
$(required_instantiations_java_cxx_headers_sources) \
$(fixed_java_cxx_headers_sources) \
java_classes.stamp \
java_cxx_headers.stamp \
source_generation.stamp
