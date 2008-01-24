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
ppl_java.Congruence \
ppl_java.Congruence_System \
ppl_java.Constraint \
ppl_java.Constraint_System \
ppl_java.Generator \
ppl_java.Generator_System \
ppl_java.Grid_Generator \
ppl_java.Grid_Generator_System \
ppl_java.Linear_Expression \
ppl_java.MIP_Problem \
ppl_java.Parma_Polyhedra_Library

fixed_java_cxx_headers_sources = \
ppl_java_Congruence.h \
ppl_java_Congruence_System.h \
ppl_java_Constraint.h \
ppl_java_Constraint_System.h \
ppl_java_Generator.h \
ppl_java_Generator_System.h \
ppl_java_Grid_Generator.h \
ppl_java_Grid_Generator_System.h \
ppl_java_Linear_Expression.h \
ppl_java_MIP_Problem.h \
ppl_java_Parma_Polyhedra_Library.h

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

# This is to force Automake to generate the required Java infrastructure
# and all the class files.
noinst_JAVA = Variable.java

all:	java_classes.stamp java_cxx_headers.stamp

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../../ppl_interface_generator_common_procedure_list.m4 \
../ppl_interface_generator_java_all_dat.m4 \
$(interface_generator_files)

java_classes.stamp: $(all_java_sources)
	@if test -n "$?"; then \
		echo '$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(AM_JAVACFLAGS) $(JAVACFLAGS) $?' ; \
		$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) \
		$(AM_JAVACFLAGS) $(JAVACFLAGS) $?; \
	else :; fi
	echo timestamp > java_classes.stamp

java_cxx_headers.stamp: java_classes.stamp
	@if test -n "$?"; then \
		echo '$(JAVAH) -classpath ..  $(fixed_java_cxx_headers) $(required_instantiations_java_cxx_headers)' ; \
		$(JAVAH) -classpath .. $(fixed_java_cxx_headers) $(required_instantiations_java_cxx_headers) ; \
	else :; fi
	echo timestamp > java_cxx_headers.stamp

# In general, several java sources will be built and the same process
# will build all of them.  We assume that C_Polyhedron.java will always
# be built, independently from all configuration options.
BUILT_SOURCES = C_Polyhedron.java

source_generation.stamp: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_java_classes_java.m4 \
			> java_blob
	$(top_srcdir)/utils/cm_cleaner.sh < java_blob
	$(top_srcdir)/utils/cm_splitter.sh < java_blob
	rm -f java_blob
	echo timestamp > source_generation.stamp

$(required_instantiations_java_sources): source_generation.stamp

endif BUILD_JAVA_INTERFACE

CLEANFILES = \
$(required_instantiations_java_sources) \
$(required_instantiations_java_cxx_headers_sources) \
$(fixed_java_cxx_headers_sources) \
java_classes.stamp \
java_cxx_headers.stamp \
source_generation.stamp
