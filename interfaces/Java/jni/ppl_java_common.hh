/* PPL Java interface common routines declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <jni.h>
#include <ppl.hh>


using namespace Parma_Polyhedra_Library;

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
  Thrown if the Java number is negative or if exceeds the maximum
  value that type U allows to store.
*/
template <typename U, typename V>
U
jtype_to_unsigned(const V& value) {

  U d = 0;
   if (value < 0)
     throw std::invalid_argument("not an unsigned integer.");
   else if (value > std::numeric_limits<U>::max())
     throw std::invalid_argument("unsigned integer out of range.");
   else
       d = value;
   return d;
}

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

// Sets a Java object to be (or not) deleted after the automatic
// call to `finalize()'. For example, object that are taken from iterators
// should not be deleted.
void
set_is_a_reference(JNIEnv* env, const jobject& ppl_object,
		   const bool reference);

// Returns a <CODE>true</CODE> if and only if the Java object
// is a reference to a C++ object, <CODE>false</CODE> otherwise.
bool
is_a_reference(JNIEnv* env, const jobject& ppl_object);


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

// Get a pointer to the underlined C++ object from a Java object.
jlong
get_ptr(JNIEnv* env, const jobject& ppl_object);

// Get a pointer to the underlined C++ object from a Java object.
void
set_ptr(JNIEnv* env, const jobject& ppl_object, const long long address);

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
// FIXME: implement me.
void set_grid_generator(JNIEnv* env, jobject& to_be_set,
			const jobject& g_gen);

void set_coefficient(JNIEnv* env, jobject& to_be_set,
		     const jobject& c);

void set_by_reference(JNIEnv* env, jobject& by_ref_to_be_set,
		      const jobject& to_insert);

jobject get_by_reference(JNIEnv* env, const jobject& by_ref_integer);

jboolean is_null(JNIEnv* env, jobject obj);
// Builds the Java linear expression starting from a congruence,
// a constraint or a generator.
// FIXME: left in the header file to allow g++ to build template code
// properly.
template <typename R>
jobject
get_linear_expression(JNIEnv* env, const R& r) {
  jclass j_le_coeff_class
    = env->FindClass("ppl_java/Linear_Expression_Coefficient");
  jclass j_le_class
    = env->FindClass("ppl_java/Linear_Expression");
  jclass j_le_variable_class
    = env->FindClass("ppl_java/Linear_Expression_Variable");
  jclass j_variable_class
    = env->FindClass("ppl_java/Variable");
  Coefficient coefficient;
  dimension_type varid = 0;
  dimension_type space_dimension = r.space_dimension();
  jobject j_le_term;
  jmethodID j_variable_ctr_id
    = env->GetMethodID(j_variable_class,
		       "<init>",
		       "(I)V");
  jmethodID j_le_variable_ctr_id
    = env->GetMethodID(j_le_variable_class,
		       "<init>",
		       "(Lppl_java/Variable;)V");

  jmethodID j_le_times_id
    = env->GetMethodID(j_le_class,
		       "times",
		       "(Lppl_java/Coefficient;)Lppl_java/Linear_Expression;");

  while (varid < space_dimension
 	 && (coefficient = r.coefficient(Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    jobject j_coefficient_zero = build_java_coeff(env, Coefficient(0));
    jmethodID j_le_coeff_ctr_id
      = env->GetMethodID(j_le_coeff_class, "<init>",
			 "(Lppl_java/Coefficient;)V");
    return env->NewObject(j_le_coeff_class, j_le_coeff_ctr_id,
			  j_coefficient_zero);
  }
  else {
    jobject j_coefficient = build_java_coeff(env, coefficient);
    jobject j_variable = env->NewObject(j_variable_class, j_variable_ctr_id,
					varid);
    jobject j_le_variable = env->NewObject(j_le_variable_class,
					   j_le_variable_ctr_id,
					   j_variable);
    j_le_term =  env->CallObjectMethod(j_le_variable,
				       j_le_times_id, j_coefficient);
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
 	j_coefficient = build_java_coeff(env, coefficient);
 	j_variable = env->NewObject(j_variable_class,
				    j_variable_ctr_id,
				    varid);
  	j_le_variable = env->NewObject(j_le_variable_class,
				       j_le_variable_ctr_id,
				       j_variable);
 	jobject j_le_term2 = env->CallObjectMethod(j_le_variable,
						   j_le_times_id,
						   j_coefficient);
  	jmethodID j_le_sum_id
  	  = env->GetMethodID(j_le_class,
  			     "sum",
  			     "(Lppl_java/Linear_Expression;)"
			     "Lppl_java/Linear_Expression;");
 	j_le_term = env->CallObjectMethod(j_le_term, j_le_sum_id, j_le_term2);
      }
    }
  }
  return j_le_term;
}


class PFunc {
private:
  jobject j_p_func;
  JNIEnv* env;
public:

  PFunc(jobject j_p_func, JNIEnv* env):
  j_p_func(j_p_func),
  env(env){
  }

  bool has_empty_codomain() const {
    jclass j_partial_function_class
       = env->FindClass("ppl_java/Partial_Function");
     jmethodID j_has_empty_codomain_id
       = env->GetMethodID(j_partial_function_class,
 			 "has_empty_codomain",
 			 "()Z");
     return env->CallBooleanMethod(j_p_func, j_has_empty_codomain_id);
  }

  dimension_type max_in_codomain() const {
     jclass j_partial_function_class
       = env->FindClass("ppl_java/Partial_Function");
     jmethodID j_max_in_codomain_id
       = env->GetMethodID(j_partial_function_class,
 			 "max_in_codomain",
 			 "()J");
     jlong value = env->CallLongMethod(j_p_func, j_max_in_codomain_id);
     return jtype_to_unsigned<dimension_type> (value);
  }

  bool maps(dimension_type i, dimension_type& j) const {
    jclass j_partial_function_class
       = env->FindClass("ppl_java/Partial_Function");
    jclass j_by_reference_class
      = env->FindClass("ppl_java/By_Reference");
    jmethodID j_by_reference_ctr_id
      = env->GetMethodID(j_by_reference_class,
			 "<init>",
			 "(Ljava/lang/Object;)V");
    jobject coeff = j_long_to_j_long_class(env, 0);
    jobject new_by_ref = env->NewObject(j_by_reference_class,
  					j_by_reference_ctr_id,
  					coeff);
    jmethodID j_maps_id
      = env->GetMethodID(j_partial_function_class,
			 "maps",
			 "(Ljava/lang/Long;Lppl_java/By_Reference;)Z");
    if(env->CallBooleanMethod(j_p_func, j_maps_id,
			      j_long_to_j_long_class(env, i),
			      new_by_ref)) {
      jobject long_value = get_by_reference(env, new_by_ref);
      j = jtype_to_unsigned<dimension_type>(j_long_class_to_j_long(env,
								   long_value));
      return true;
    }
    return false;
  }
};
