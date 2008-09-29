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

 // Converts a C++ bool to a Java boolean.
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

// Returns a <CODE>true</CODE> if and only if the Java object
// is a reference to a C++ object, <CODE>false</CODE> otherwise.
bool
is_java_marked(JNIEnv* env, const jobject& ppl_object);


// Converts a PPL Poly_Gen_Relation to a Java Poly_Gen_Relation.
jobject
build_java_poly_gen_relation(JNIEnv* env,
			     Poly_Gen_Relation& pcr);

// Converts a PPL Poly_Con_Relation to a Java Poly_Con_Relation.
jobject
build_java_poly_con_relation(JNIEnv* env,
			    Poly_Con_Relation& pgr);

// Converts a Java variables set to a PPL variables set.
Parma_Polyhedra_Library::Variables_Set
build_ppl_variables_set(JNIEnv* env,
			const jobject& variables_set);

// Converts a Java variables set to a PPL variables set.
jobject
build_java_variables_set(JNIEnv* env,
			 const Variables_Set& variables_set);

// Converts a Java relation symbol to a PPL relation_symbol.
Parma_Polyhedra_Library::Relation_Symbol
build_ppl_relsym(JNIEnv* env, const jobject& j_relsym);

// Converts a Java optimization mode to a PPL optimization mode.
Parma_Polyhedra_Library::Optimization_Mode
build_ppl_optimization_mode(JNIEnv* env, const jobject& j_opt_mode);

// Converts a PPL optimization mode to a Java optimization mode.
jobject
build_java_optimization_mode(JNIEnv* env, const Optimization_Mode& opt_mode);

// Converts a Java control parameter name to a PPL control parameter name.
Parma_Polyhedra_Library::MIP_Problem::Control_Parameter_Name
build_ppl_control_parameter_name(JNIEnv* env, const jobject& j_cp_name);

// Converts a PPL control parameter name to a Java control parameter name.
jobject
build_java_control_parameter_name(JNIEnv* env,
                                  const MIP_Problem::Control_Parameter_Name& cp_name);

// Converts a Java control parameter value to a PPL control parameter value.
Parma_Polyhedra_Library::MIP_Problem::Control_Parameter_Value
build_ppl_control_parameter_value(JNIEnv* env, const jobject& j_cp_value);

// Converts a PPL control parameter value to a Java control parameter value.
jobject
build_java_control_parameter_value(JNIEnv* env,
                                  const MIP_Problem::Control_Parameter_Value& cp_value);

jobject
build_java_mip_status(JNIEnv* env, const MIP_Problem_Status& mip_status);

// Converts a Java variable to a PPL variable.
Parma_Polyhedra_Library::Variable
build_ppl_variable(JNIEnv* env, const jobject& j_var);


// Converts a Java variable to a PPL variable.
jobject
build_java_variable(JNIEnv* env, const Variable& var);

// Converts a Java coefficient to a PPL coefficient.
Parma_Polyhedra_Library::Coefficient
build_ppl_coeff(JNIEnv* env, const jobject& j_coeff);

// Converts a PPL coefficient to a Java coefficient.
jobject
build_java_coeff(JNIEnv* env,
		     const Parma_Polyhedra_Library::Coefficient& ppl_coeff);

// Builds a PPL constraint from a Java constraint.
Parma_Polyhedra_Library::Constraint
build_ppl_constraint(JNIEnv* env, const jobject& j_constraint);

// Builds a PPL linear expression from a Java linear expression.
Parma_Polyhedra_Library::Linear_Expression
build_linear_expression(JNIEnv* env, const jobject& j_le);

// Builds a PPL congruence from a Java congruence.
Parma_Polyhedra_Library::Congruence
build_ppl_congruence(JNIEnv* env, const jobject& j_cg);

// Builds a PPL generator from a Java generator.
Parma_Polyhedra_Library::Generator
build_ppl_generator(JNIEnv* env, const jobject& j_g);

// Builds a PPL grid generator from a Java grid generator.
Parma_Polyhedra_Library::Grid_Generator
build_ppl_grid_generator(JNIEnv* env, const jobject& j_g);

// Builds a Java grid generator from a PPL grid generator.
jobject
build_java_grid_generator(JNIEnv* env, const Grid_Generator& grid_g);

// Get a pointer to the underlying C++ object from a Java object.
void*
get_ptr(JNIEnv* env, const jobject& ppl_object);


// Builds a PPL grid generator system from a Java grid generator system.
Parma_Polyhedra_Library::Grid_Generator_System
build_ppl_grid_generator_system(JNIEnv* env, const jobject& j_g);


// Builds a PPL constraint system from a Java constraint system.
Parma_Polyhedra_Library::Constraint_System
build_ppl_constraint_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL generator system from a Java generator system.
Parma_Polyhedra_Library::Generator_System
build_ppl_generator_system(JNIEnv* env, const jobject& j_iterable);

// Builds a PPL congruence system from a Java congruence system.
Parma_Polyhedra_Library::Congruence_System
build_ppl_congruence_system(JNIEnv* env, const jobject& j_iterable);

// Builds a Java constraint from a PPL constraint.
jobject
build_java_constraint(JNIEnv* env, const Constraint& c);

// Builds a Java congruence from a PPL congruence.
jobject
build_java_congruence(JNIEnv* env, const Congruence& cg);

// Builds a Java generator from a PPL generator.
jobject
build_java_generator(JNIEnv* env, const Generator& cg);

// Builds a Java constraint system from a PPL constraint system.
jobject
build_java_constraint_system(JNIEnv* env, const Constraint_System& cs);

// FIXME: implement me
// Builds a PPL grid generator from a Java grid generator.
jobject
build_java_grid_generator_system(JNIEnv* env,
				  const Grid_Generator_System& grid_g_system);

// Builds a Java generator system from a PPL generator system.
jobject
build_java_generator_system(JNIEnv* env, const Generator_System& gs);

// Builds a Java congruence system from a PPL congruence system.
jobject
build_java_congruence_system(JNIEnv* env, const Congruence_System& cgs);

// Utility routine to take the inhomogeneous term from a constraint or a
// congruence.
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


// Utility to set a value a parma_polyhedra_library Pair. the argument
// `arg' has two possible values: 0 to set `first', 1 to `second'.
void set_pair_element(JNIEnv* env, jobject& pair_to_be_set, int arg,
		      const jobject& obj_to_insert);

// Utility to get a value from a parma_polyhedra_library Pair. the
// argument `arg' has two possible values: 0 to set `first', 1 to
// `second'.
jobject get_pair_element(JNIEnv* env, int arg, const jobject& pair);

jboolean is_null(JNIEnv* env, jobject obj);




// Set the pointer of the underlying C++ object in the Java object
template <typename T>
void
set_ptr(JNIEnv* env, const jobject& ppl_object,
	const T* address, bool to_be_marked = false);

// Builds the Java linear expression starting from a congruence,
// a constraint or a generator.
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
