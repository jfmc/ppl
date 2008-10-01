/* Domain-independent part of the Java interface: declarations.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_ppl_java_common_defs_hh
#define PPL_ppl_java_common_defs_hh 1

#include "ppl.hh"
#include <jni.h>
#include "interfaced_boxes.hh"
#include "marked_pointers.hh"

#define CATCH_ALL \
  catch(const std::overflow_error& e) { \
    handle_exception(env, e); \
  } \
  catch(const std::length_error& e) { \
    handle_exception(env, e); \
  } \
  catch (const std::bad_alloc& e) { \
    handle_exception(env, e); \
  } \
  catch (const std::domain_error& e) { \
    handle_exception(env, e); \
  } \
 catch (const std::invalid_argument& e) { \
    handle_exception(env, e); \
  } \
 catch(const std::logic_error& e) { \
    handle_exception(env, e); \
  } \
  catch (const std::exception& e) { \
    handle_exception(env, e); \
  } \
  catch (...) { \
    handle_exception(env); \
  };

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Java {

void
handle_exception(JNIEnv* env, const std::logic_error& e);

void
handle_exception(JNIEnv* env, const std::invalid_argument& e);

void
handle_exception(JNIEnv* env, const std::domain_error& e);

void
handle_exception(JNIEnv* env, const std::overflow_error& e);

void
handle_exception(JNIEnv* env, const std::length_error& e);

void
handle_exception(JNIEnv* env, const std::bad_alloc&);

void
handle_exception(JNIEnv* env, const std::exception& e);

void
handle_exception(JNIEnv* env);

/*! \brief
  Converts a Java native number to an unsigned C++ number.

  \param value
  The Java native number of type V to be converted.

  \exception std::invalid_argument
  Thrown if the Java number is negative.
*/
template <typename U, typename V>
U
jtype_to_unsigned(const V& value);

//! Converts a C++ bool to a Java boolean.
jobject
bool_to_j_boolean(JNIEnv* env,
 		  const bool bool_value);

jint
j_integer_to_j_int(JNIEnv* env, const jobject& j_integer);

jobject
j_int_to_j_integer(JNIEnv* env, const jint& jint_value);

jlong
j_long_class_to_j_long(JNIEnv* env, const jobject& j_long);

jobject
j_long_to_j_long_class(JNIEnv* env, const jlong& jlong_value);

/*! \brief
  Returns <CODE>true</CODE> if and only if the Java object is a
  reference to a C++ object.
*/
bool
is_java_marked(JNIEnv* env, const jobject& ppl_object);


//! Converts a C++ Poly_Gen_Relation to a Java Poly_Gen_Relation.
jobject
build_java_poly_gen_relation(JNIEnv* env,
			     Poly_Gen_Relation& pcr);

//! Converts a C++ Poly_Con_Relation to a Java Poly_Con_Relation.
jobject
build_java_poly_con_relation(JNIEnv* env,
                             Poly_Con_Relation& pgr);

//! Converts a Java Variables_Set to a C++ Variables_Set.
Parma_Polyhedra_Library::Variables_Set
build_cxx_variables_set(JNIEnv* env,
			const jobject& variables_set);

//! Converts a Java Variables_Set to a C++ Variables_Set.
jobject
build_java_variables_set(JNIEnv* env,
			 const Variables_Set& variables_set);

//! Converts a Java Relation_Symbol to a C++ Relation_Symbol.
Parma_Polyhedra_Library::Relation_Symbol
build_cxx_relsym(JNIEnv* env, const jobject& j_relsym);

//! Converts a Java Optimization_Mode mode to a C++ Optimization_Mode.
Parma_Polyhedra_Library::Optimization_Mode
build_cxx_optimization_mode(JNIEnv* env, const jobject& j_opt_mode);

//! Converts a C++ Optimization_Mode mode to a Java Optimization_Mode.
jobject
build_java_optimization_mode(JNIEnv* env, const Optimization_Mode& opt_mode);

//! Converts a Java Control_Parameter_Name to a C++ Control_Parameter_Name.
Parma_Polyhedra_Library::MIP_Problem::Control_Parameter_Name
build_cxx_control_parameter_name(JNIEnv* env, const jobject& j_cp_name);

//! Converts a C++ Control_Parameter_Name to a Java Control_Parameter_Name.
jobject
build_java_control_parameter_name(JNIEnv* env,
                                  const MIP_Problem::Control_Parameter_Name&
                                  cp_name);

//! Converts a Java Control_Parameter_Value to a C++ Control_Parameter_Value.
Parma_Polyhedra_Library::MIP_Problem::Control_Parameter_Value
build_cxx_control_parameter_value(JNIEnv* env, const jobject& j_cp_value);

//! Converts a C++ Control_Parameter_Value to a Java Control_Parameter_Value.
jobject
build_java_control_parameter_value(JNIEnv* env,
                                  const MIP_Problem::Control_Parameter_Value& cp_value);

//! Converts a C++ MIP_Problem_Status to a Java MIP_Problem_Status.
jobject
build_java_mip_status(JNIEnv* env, const MIP_Problem_Status& mip_status);

//! Converts a Java Variable to a C++ Variable.
Parma_Polyhedra_Library::Variable
build_cxx_variable(JNIEnv* env, const jobject& j_var);


//! Converts a Java Variable to a C++ Variable.
jobject
build_java_variable(JNIEnv* env, const Variable& var);

//! Converts a Java Coefficient to a C++ Coefficient.
Parma_Polyhedra_Library::Coefficient
build_cxx_coeff(JNIEnv* env, const jobject& j_coeff);

//! Converts a C++ Coefficient to a Java Coefficient.
jobject
build_java_coeff(JNIEnv* env,
		     const Parma_Polyhedra_Library::Coefficient& ppl_coeff);

//! Builds a C++ Constraint from a Java Constraint.
Parma_Polyhedra_Library::Constraint
build_cxx_constraint(JNIEnv* env, const jobject& j_constraint);

//! Builds a C++ Linear_Expression from a Java Linear_Expression.
Parma_Polyhedra_Library::Linear_Expression
build_linear_expression(JNIEnv* env, const jobject& j_le);

//! Builds a C++ Congruence from a Java Congruence.
Parma_Polyhedra_Library::Congruence
build_cxx_congruence(JNIEnv* env, const jobject& j_cg);

//! Builds a C++ Generator from a Java Generator.
Parma_Polyhedra_Library::Generator
build_cxx_generator(JNIEnv* env, const jobject& j_g);

//! Builds a C++ Grid_Generator from a Java Grid_Generator.
Parma_Polyhedra_Library::Grid_Generator
build_cxx_grid_generator(JNIEnv* env, const jobject& j_g);

//! Builds a Java Grid_Generator from a C++ Grid_Generator.
jobject
build_java_grid_generator(JNIEnv* env, const Grid_Generator& grid_g);

//! Builds a C++ Grid_Generator_System from a Java Grid_Generator_System.
Parma_Polyhedra_Library::Grid_Generator_System
build_cxx_grid_generator_system(JNIEnv* env, const jobject& j_g);

//! Builds a C++ Constraint_System from a Java Constraint_System.
Parma_Polyhedra_Library::Constraint_System
build_cxx_constraint_system(JNIEnv* env, const jobject& j_iterable);

//! Builds a C++ Generator_System from a Java Generator_System.
Parma_Polyhedra_Library::Generator_System
build_cxx_generator_system(JNIEnv* env, const jobject& j_iterable);

//! Builds a C++ Congruence_System from a Java %Congruence_System.
Parma_Polyhedra_Library::Congruence_System
build_cxx_congruence_system(JNIEnv* env, const jobject& j_iterable);

//! Builds a Java Constraint from a C++ Constraint.
jobject
build_java_constraint(JNIEnv* env, const Constraint& c);

//! Builds a Java Congruence from a C++ Congruence.
jobject
build_java_congruence(JNIEnv* env, const Congruence& cg);

//! Builds a Java Generator from a C++ Generator.
jobject
build_java_generator(JNIEnv* env, const Generator& cg);

//! Builds a Java Constraint_System from a C++ Constraint_System.
jobject
build_java_constraint_system(JNIEnv* env, const Constraint_System& cs);

// FIXME: implement me
//! Builds a C++ Grid_Generator from a Java Grid_Generator.
jobject
build_java_grid_generator_system(JNIEnv* env,
				  const Grid_Generator_System& grid_g_system);

//! Builds a Java Generator_System from a C++ Generator_System.
jobject
build_java_generator_system(JNIEnv* env, const Generator_System& gs);

//! Builds a Java Congruence_System from a C++ Congruence_System.
jobject
build_java_congruence_system(JNIEnv* env, const Congruence_System& cgs);

// FIXME: what?
//! Utility routine to take the inhomogeneous term from a constraint or a
//! congruence.
jobject
get_le_inhomogeneous_term(JNIEnv* env, const Coefficient& c);

void set_generator(JNIEnv* env, jobject& to_be_set,
		   const jobject& gen);

void set_grid_generator(JNIEnv* env, jobject& to_be_set,
			const jobject& g_gen);

void set_coefficient(JNIEnv* env, jobject& to_be_set,
		     const jobject& c);

void set_by_reference(JNIEnv* env, jobject& by_ref_to_be_set,
		      const jobject& to_insert);

jobject get_by_reference(JNIEnv* env, const jobject& by_ref_integer);


//! Utility to set a value a parma_polyhedra_library Pair. the argument
//! `arg' has two possible values: 0 to set `first', 1 to `second'.
void set_pair_element(JNIEnv* env, jobject& pair_to_be_set, int arg,
		      const jobject& obj_to_insert);

//! Utility to get a value from a parma_polyhedra_library Pair. the
//! argument `arg' has two possible values: 0 to set `first', 1 to
//! `second'.
jobject get_pair_element(JNIEnv* env, int arg, const jobject& pair);

jboolean is_null(JNIEnv* env, jobject obj);

//! Returns a pointer to the underlying C++ object from a Java object.
void*
get_ptr(JNIEnv* env, const jobject& ppl_object);

//! Set the pointer of the underlying C++ object in the Java object.
template <typename T>
void
set_ptr(JNIEnv* env, const jobject& ppl_object,
	const T* address, bool to_be_marked = false);

//! Builds the Java linear expression starting from a congruence,
//! a constraint or a generator.
template <typename R>
jobject
get_linear_expression(JNIEnv* env, const R& r);

// FIXME: documentation is missing.
class Partial_Function {
public:
  // FIXME: documentation is missing.
  Partial_Function(jobject j_p_func, JNIEnv* env);

  // FIXME: documentation is missing.
  bool has_empty_codomain() const;

  // FIXME: documentation is missing.
  dimension_type max_in_codomain() const;

  // FIXME: documentation is missing.
  bool maps(dimension_type i, dimension_type& j) const;

private:
  // FIXME: documentation is missing.
  jobject j_p_func;

  // FIXME: documentation is missing.
  JNIEnv* env;
};

} // namespace Java

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library


#include "ppl_java_common.inlines.hh"

#endif // !defined(PPL_ppl_prolog_common_defs_hh)
