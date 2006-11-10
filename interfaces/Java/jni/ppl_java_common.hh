/* PPL Java interface common routines declaration.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <jni.h>
#include <ppl.hh>



// // Converts a C++ bool to a Java boolean.
// jobject
// bool_to_j_boolean(JNIEnv* env,
// 		  const bool bool_value);

// // Converts a Java boolean set to a C++ bool.
// bool
// j_boolean_to_bool(JNIEnv* env,
// 		  const jobject& j_boolean);

// Converts a Java variables set to a PPL variables set.
Parma_Polyhedra_Library::Variables_Set
j_variables_set_to_ppl_variables_set(JNIEnv* env,
				     const jobject& variables_set);

// Converts a Java relation symbol to a PPL relation_symbol.
Parma_Polyhedra_Library::Relation_Symbol
j_relsym_to_ppl_relsym(JNIEnv* env, const jobject& j_relsym);

// Converts a Java variable to a PPL variable.
Parma_Polyhedra_Library::Variable
j_variable_to_ppl_variable(JNIEnv* env, const jobject& j_var);

// Converts a Java coefficient to a PPL coefficient.
Parma_Polyhedra_Library::Coefficient
j_coeff_to_ppl_coeff(JNIEnv* env, const jobject& j_coeff);

// Converts a PPL coefficient to a Java coefficient.
jobject
ppl_coeff_to_j_coeff(JNIEnv* env,
		     const Parma_Polyhedra_Library::Coefficient& ppl_coeff);

// Builds a PPL constraint from a Java constraint.
Parma_Polyhedra_Library::Constraint
build_ppl_constraint(JNIEnv* env, const jobject& j_constraint);

// Builds a PPL linear expression from a Java linear expression.
Parma_Polyhedra_Library::Linear_Expression
build_linear_expression(JNIEnv* env, const jobject& j_le);

// Builds a PPL congruence from a Java congruence.
Parma_Polyhedra_Library::Congruence
build_ppl_congruence(JNIEnv* env, const jobject& j_le);

// Builds a PPL genearator from a Java generator.
Parma_Polyhedra_Library::Generator
build_generator(JNIEnv* env, const jobject& j_le);

// Get a pointer to the underlined C++ object from a Java object.
jlong
get_ptr(JNIEnv* env, const jobject& ppl_object);

// Builds a PPL constraint system from a Java constraint system.
Parma_Polyhedra_Library::Constraint_System
build_ppl_constraint_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL generator system from a Java generator system.
Parma_Polyhedra_Library::Generator_System
build_ppl_generator_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL congruence system from a Java congruence system.
Parma_Polyhedra_Library::Congruence_System
build_ppl_congruence_system(JNIEnv* env, const jobject& j_iterable);
