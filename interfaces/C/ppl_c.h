/* Header file for the C interface.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_ppl_c_h
#define PPL_ppl_c_h 1

/*! \defgroup PPL_C_interface C Language Interface

All the declarations needed to use the C interface of the Parma Polyhedra
Library (preprocessor symbols, data types, variables and functions) are
collected in the header file <CODE>ppl_c.h</CODE>.
This file, that is designed to work with pre-ANSI and ANSI C compilers
as well as C99 and C++ compilers, should be included, either directly
or through some other header file, with the directive
\code
#include <ppl_c.h>
\endcode
If this directive does not work, it means that your compiler is not finding
<CODE>ppl_c.h</CODE> where it expects.
Either you forgot to install the library (in which case you may want to
<CODE>make install</CODE>, perhaps with root privileges),
or you installed it in the wrong place (in which case you may want to
reconfigure the library using the appropriate pathname for the
<CODE>--prefix</CODE> option),
or you forgot to tell your compiler where the right place is
(in which case you should add an option, usually <CODE>-I</CODE>,
to the ones you pass to the compiler).

The name space of the PPL's C interface is <CODE>PPL_*</CODE> for
preprocessor symbols, enumeration values and variables,
and <CODE>ppl_*</CODE> for data types and function names,
The interface systematically uses <EM>opaque data types</EM>
(generic pointers that completely hide the internal representations
from the client code) and provides required access functions.
This way, the client code can exploit all the functionalities of
the library yet avoiding the direct manipulation of the library's
data structures.
The advantages are that (1) applications do not depend on
the internals of the library (these may change from release to release),
and (2) the interface invariants can be thoroughly checked
(by the access functions).

TO BE CONTINUED HERE.

Look into the `interfaces/C/lpenum/' directory.  It contains a toy
LP solver written in C and using the PPL's C interface.  There you can
find examples of how to use several functions provided by that interface.
*/

/*@{*/

/*
  For some reason, GMP up to and including version 4.1.2 requires
  <stdio.h> to be included before <gmp.h>.
*/
#include <stdio.h>
#include <gmp.h>
#include <stddef.h>

/*
  __P is a macro used to wrap function prototypes, so that compilers
  that don't understand ANSI C prototypes still work, and ANSI C
  compilers can issue warnings about type mismatches.
*/
#undef __P
#if defined(__STDC__) || defined(_AIX) || (defined(__mips) && defined(_SYSTYPE_SVR4)) || defined(WIN32) || defined(__cplusplus) || defined(SWIG)
# define __P(protos) protos
#else
# define __P(protos) ()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*! \brief
  Defines the error code that any function can return.
*/
enum ppl_enum_error_code {
  /*! \hideinitializer
    The virtual memory available to the process has been exhausted. */
  PPL_ERROR_OUT_OF_MEMORY = -2,
  /*! \hideinitializer
    A function has been invoked with an invalid argument. */
  PPL_ERROR_INVALID_ARGUMENT = -3,
  /*! \hideinitializer
    An internal error that was diagnosed by the PPL itself.
    This indicates a bug in the PPL. */
  PPL_ERROR_INTERNAL_ERROR = -4,
  /*! \hideinitializer
    A standard exception has been raised by the C++ run-time environment.
    This indicates a bug in the PPL. */
  PPL_ERROR_UNKNOWN_STANDARD_EXCEPTION = -5,
  /*! \hideinitializer
    A totally unknown, totally unexpected error happened.
    This indicates a bug in the PPL. */
  PPL_ERROR_UNEXPECTED_ERROR = -6
};

/*! \brief
  An unsigned integral type for representing space dimensions.
*/
typedef size_t ppl_dimension_type;

/*! \name Initialization, Error Handling and Auxiliary Functions */
/*@{*/

/*! \brief
  Writes to \p m the maximum space dimension this library can handle.
*/
int
ppl_max_space_dimension __P((ppl_dimension_type* m));

/*! \brief
  Writes to \p m a value that does not designate a valid dimension.
*/
int
ppl_not_a_dimension __P((ppl_dimension_type* m));

/*! \brief
  Initializes the Parma Polyhedra Library.
  This function must be called before any other function.
*/
int
ppl_initialize __P((void));

/*! \brief
  Finalizes the Parma Polyhedra Library.
  This function must be called after any other function.
*/
int
ppl_finalize __P((void));

/*! \brief
  Installs the user-defined error handler pointed by \p h.

  The error handler takes an error code and a textual description that
  gives further information about the actual error.  The C string
  containing the textual description is read-only and its existence it
  not guaranteed after the handler has returned.
*/
int
ppl_set_error_handler __P((void (*h)(enum ppl_enum_error_code code,
				     const char* description)));

/*@}*/ /* Initialization, Error Handling and Auxiliary Functions */

#undef PPL_TYPE_DECLARATION
#define PPL_TYPE_DECLARATION(Type) /*! \brief Opaque pointer to Type. */ typedef struct ppl_ ## Type ## _tag* ppl_ ## Type ## _t; /*! \brief Opaque pointer to const Type. */ typedef struct ppl_ ## Type ## _tag const* ppl_const_ ## Type ## _t

PPL_TYPE_DECLARATION(Coefficient);

PPL_TYPE_DECLARATION(LinExpression);

PPL_TYPE_DECLARATION(Constraint);

PPL_TYPE_DECLARATION(ConSys);

PPL_TYPE_DECLARATION(ConSys_const_iterator);

PPL_TYPE_DECLARATION(Generator);

PPL_TYPE_DECLARATION(GenSys);

PPL_TYPE_DECLARATION(GenSys_const_iterator);

PPL_TYPE_DECLARATION(Polyhedron);

#undef PPL_TYPE_DECLARATION

/*! \name Functions Related to Coefficients */
/*@{*/

/*! \brief
  Creates a new coefficient with value 0 and writes an handle for the
  newly created coefficient at address \p pc.
*/
int
ppl_new_Coefficient __P((ppl_Coefficient_t* pc));

/*! \brief
  Creates a new coefficient with the value given by the GMP integer
  \p z and writes an handle for the newly created coefficient
  at address \p pc.
*/
int
ppl_new_Coefficient_from_mpz_t __P((ppl_Coefficient_t* pc, mpz_t z));

/*! \brief
  Builds a coefficient that is a copy of \p c; writes an handle
  for the newly created coefficient at address \p pc.
*/
int
ppl_new_Coefficient_from_Coefficient __P((ppl_Coefficient_t* pc,
					  ppl_const_Coefficient_t c));

/*! \brief
  Assign to \p dst the value given by the GMP integer \p z.
*/
int
ppl_assign_Coefficient_from_mpz_t __P((ppl_Coefficient_t dst, mpz_t z));

/*! \brief
  Assigns a copy of the coefficient \p src to \p dst.
*/
int
ppl_assign_Coefficient_from_Coefficient __P((ppl_Coefficient_t dst,
					     ppl_const_Coefficient_t src));

/*! \brief
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Coefficient __P((ppl_const_Coefficient_t c));

/*! \brief
  Sets the value of the GMP integer \p z to the value of \p c.
*/
int
ppl_Coefficient_to_mpz_t __P((ppl_const_Coefficient_t c, mpz_t z));

/*! \brief
  Returns a positive integer if \p c is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p c is broken.  Useful for debugging purposes.
*/
int
ppl_Coefficient_OK __P((ppl_const_Coefficient_t c));

/*@}*/ /* Functions Related to Coefficients */

/*! \name Functions Related to Linear Expressions */
/*@{*/

/*! \brief
  Creates a new linear expression corresponding to the constant 0 in a
  zero-dimensional space; writes an handle for the new linear
  expression at address \p ple.
*/
int
ppl_new_LinExpression __P((ppl_LinExpression_t* ple));

/*! \brief
  Creates a new linear expression corresponding to the constant 0 in a
  <TT>d</TT>-dimensional space; writes an handle for the new linear
  expression at address \p ple.
*/
int
ppl_new_LinExpression_with_dimension __P((ppl_LinExpression_t* ple,
					  ppl_dimension_type d));

/*! \brief
  Builds a linear expression that is a copy of \p le; writes an handle
  for the newly created linear expression at address \p ple.
*/
int
ppl_new_LinExpression_from_LinExpression __P((ppl_LinExpression_t* ple,
					      ppl_const_LinExpression_t le));

/*! \brief
  Builds a linear expression corresponding to constraint \p c;
  writes an handle for the newly created linear expression at address \p ple.
*/
int
ppl_new_LinExpression_from_Constraint __P((ppl_LinExpression_t* ple,
					   ppl_const_Constraint_t c));

/*! \brief
  Builds a linear expression corresponding to generator \p g;
  writes an handle for the newly created linear expression at address \p ple.
*/
int
ppl_new_LinExpression_from_Generator __P((ppl_LinExpression_t* ple,
					  ppl_const_Generator_t g));

/*! \brief
  Invalidates the handle \p le: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_LinExpression __P((ppl_const_LinExpression_t le));

/*! \brief
  Assigns a copy of the linear expression \p src to \p dst.
*/
int
ppl_assign_LinExpression_from_LinExpression
__P((ppl_LinExpression_t dst, ppl_const_LinExpression_t src));

/*! \brief
  Adds \p n to the coefficient of variable \p var in the linear
  expression \p le.  The space dimension is set to be the maximum
  between \p var + 1 and the old space dimension.
*/
int
ppl_LinExpression_add_to_coefficient __P((ppl_LinExpression_t le,
					  ppl_dimension_type var,
					  ppl_const_Coefficient_t n));

/*! \brief
  Adds \p n to the inhomogeneous term of the linear expression
  \p le.
*/
int
ppl_LinExpression_add_to_inhomogeneous __P((ppl_LinExpression_t le,
					    ppl_const_Coefficient_t n));

/*! \brief
  Adds the linear expression \p src to \p dst.
*/
int
ppl_add_LinExpression_to_LinExpression __P((ppl_LinExpression_t dst,
					    ppl_const_LinExpression_t src));

/*! \brief
  Subtracts the linear expression \p src from \p dst.
*/
int
ppl_subtract_LinExpression_from_LinExpression
__P((ppl_LinExpression_t dst,
     ppl_const_LinExpression_t src));

/*! \brief
  Multiply the linear expression \p dst by \p n.
*/
int
ppl_multiply_LinExpression_by_Coefficient __P((ppl_LinExpression_t le,
					       ppl_const_Coefficient_t n));

/*! \brief
  Returns the space dimension of \p le.
*/
int
ppl_LinExpression_space_dimension __P((ppl_const_LinExpression_t le));

/*! \brief
  Copies into \p n the coefficient of variable \p var in
  the linear expression \p le.
*/
int
ppl_LinExpression_coefficient __P((ppl_const_LinExpression_t le,
				   ppl_dimension_type var,
				   ppl_Coefficient_t n));

/*! \brief
  Copies into \p n the inhomogeneous term of linear expression \p le.
*/
int
ppl_LinExpression_inhomogeneous_term __P((ppl_const_LinExpression_t le,
					  ppl_Coefficient_t n));

/*! \brief
  Returns a positive integer if \p le is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p le is broken.  Useful for debugging purposes.
*/
int
ppl_LinExpression_OK __P((ppl_const_LinExpression_t le));

/*@}*/ /* Functions Related to Linear Expressions */

/*! \brief
  Describes the relations represented by a constraint.
*/
enum ppl_enum_Constraint_Type {
  /*! The constraint is of the form \f$e < 0\f$. */
  PPL_CONSTRAINT_TYPE_LESS_THAN,
  /*! The constraint is of the form \f$e \leq 0\f$. */
  PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL,
  /*! The constraint is of the form \f$e = 0\f$. */
  PPL_CONSTRAINT_TYPE_EQUAL,
  /*! The constraint is of the form \f$e \geq 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL,
  /*! The constraint is of the form \f$e > 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN
};


/*! \name Functions Related to Constraints */
/*@{*/

/*! \brief
  Creates the new constraint `\p le \p rel 0' and writes an handle for
  it at address \p pc.  The space dimension of the new constraint is
  equal to the space dimension of \p le.
*/
int
ppl_new_Constraint __P((ppl_Constraint_t* pc,
			ppl_const_LinExpression_t le,
			enum ppl_enum_Constraint_Type rel));

/*! \brief
  Creates the unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$
  and writes an handle for it at address \p pc.
*/
int
ppl_new_Constraint_zero_dim_false __P((ppl_Constraint_t* pc));

/*! \brief
  Creates the true (zero-dimension space) constraint \f$0 \leq 1\f$,
  also known as <EM>positivity constraint</EM>.
  An handle for the newly created constraint is written at address \p pc.
*/
int
ppl_new_Constraint_zero_dim_positivity __P((ppl_Constraint_t* pc));

/*! \brief
  Builds a constraint that is a copy of \p c; writes an handle
  for the newly created constraint at address \p pc.
*/
int
ppl_new_Constraint_from_Constraint __P((ppl_Constraint_t* pc,
					ppl_const_Constraint_t c));

/*! \brief
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Constraint __P((ppl_const_Constraint_t c));

/*! \brief
  Assigns a copy of the constraint \p src to \p dst.
*/
int
ppl_assign_Constraint_from_Constraint __P((ppl_Constraint_t dst,
					   ppl_const_Constraint_t src));

/*! \brief
  Returns the space dimension of \p c.
*/
int
ppl_Constraint_space_dimension __P((ppl_const_Constraint_t c));

/*! \brief
  Returns the type of constraint \p c.
*/
int
ppl_Constraint_type __P((ppl_const_Constraint_t c));

/*! \brief
  Copies into \p n the coefficient of variable \p var in
  constraint \p c.
*/
int
ppl_Constraint_coefficient __P((ppl_const_Constraint_t c,
				ppl_dimension_type var,
				ppl_Coefficient_t n));

/*! \brief
  Copies into \p n the inhomogeneous term of constraint \p c.
*/
int
ppl_Constraint_inhomogeneous_term __P((ppl_const_Constraint_t c,
				       ppl_Coefficient_t n));

/*! \brief
  Returns a positive integer if \p c is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p c is broken.  Useful for debugging purposes.
*/
int
ppl_Constraint_OK __P((ppl_const_Constraint_t c));

/*@}*/ /* Functions Related to Constraints */

/*! \name Functions Related to Constraint Systems */
/*@{*/

/*! \brief
  Builds an empty system of constraints and writes an handle to it at
  address \p pcs.
*/
int
ppl_new_ConSys __P((ppl_ConSys_t* pcs));

/*! \brief
  Builds a zero-dimensional, unsatisfiable constraint system and
  writes an handle to it at address \p pcs.
*/
int
ppl_new_ConSys_zero_dim_empty __P((ppl_ConSys_t* pcs));

/*! \brief
  Builds the singleton constraint system containing only a copy of
  constraint \p c; writes an handle for the newly created system at
  address \p pcs.
*/
int
ppl_new_ConSys_from_Constraint __P((ppl_ConSys_t* pcs,
				    ppl_const_Constraint_t c));

/*! \brief
  Builds a constraint system that is a copy of \p cs; writes an handle
  for the newly created system at address \p pcs.
*/
int
ppl_new_ConSys_from_ConSys __P((ppl_ConSys_t* pcs, ppl_const_ConSys_t cs));

/*! \brief
  Invalidates the handle \p cs: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_ConSys __P((ppl_const_ConSys_t cs));

/*! \brief
  Assigns a copy of the constraint system \p src to \p dst.
*/
int
ppl_assign_ConSys_from_ConSys __P((ppl_ConSys_t dst,
				   ppl_const_ConSys_t src));

/*! \brief
  Returns the dimension of the vector space enclosing \p cs.
*/
int
ppl_ConSys_space_dimension __P((ppl_const_ConSys_t cs));

/*! \brief
  Removes all the constraints from the constraint system \p cs
  and sets its space dimension to 0.
*/
int
ppl_ConSys_clear __P((ppl_ConSys_t cs));

/*! \brief
  Inserts a copy of the constraint \p c into \p cs; the space
  dimension is increased, if necessary.
*/
int
ppl_ConSys_insert_Constraint __P((ppl_ConSys_t cs,
				  ppl_const_Constraint_t c));

/*! \brief
  Returns a positive integer if \p cs is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p cs is broken.  Useful for debugging purposes.
*/
int
ppl_ConSys_OK __P((ppl_const_ConSys_t c));


/*! \brief
  Builds a new `const iterator' and writes an handle to it at address
  \p pcit.
*/
int
ppl_new_ConSys_const_iterator __P((ppl_ConSys_const_iterator_t* pcit));

/*! \brief
  Builds a const iterator system that is a copy of \p cit; writes an
  handle for the newly created const iterator at address \p pcit.
*/
int
ppl_new_ConSys_const_iterator_from_ConSys_const_iterator
__P((ppl_ConSys_const_iterator_t* pcit,
     ppl_const_ConSys_const_iterator_t cit));

/*! \brief
  Invalidates the handle \p cit: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_ConSys_const_iterator
__P((ppl_const_ConSys_const_iterator_t cit));

/*! \brief
  Assigns a copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_ConSys_const_iterator_from_ConSys_const_iterator
__P((ppl_ConSys_const_iterator_t dst,
     ppl_const_ConSys_const_iterator_t src));

/*! \brief
  Assigns to \p cit a const iterator "pointing" to the beginning of
  the constraint system \p cs.
*/
int
ppl_ConSys_begin __P((ppl_const_ConSys_t cs, ppl_ConSys_const_iterator_t cit));

/*! \brief
  Assigns to \p cit a const iterator "pointing" past the end of the
  constraint system \p cs.
*/
int
ppl_ConSys_end __P((ppl_const_ConSys_t cs, ppl_ConSys_const_iterator_t cit));

/*! \brief
  Dereference \p cit writing a const handle to the resulting
  constraint at address \p pc.
*/
int
ppl_ConSys_const_iterator_dereference
__P((ppl_const_ConSys_const_iterator_t cit, ppl_const_Constraint_t* pc));

/*! \brief
  Increment \p cit so that it "points" to the next constraint.
*/
int
ppl_ConSys_const_iterator_increment __P((ppl_ConSys_const_iterator_t cit));

/*! \brief
  Returns a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_ConSys_const_iterator_equal_test
__P((ppl_const_ConSys_const_iterator_t x,
     ppl_const_ConSys_const_iterator_t y));

/*@}*/ /* Functions Related to Constraint Systems */

/*! \brief
  Describes the different kinds of generators.
*/
enum ppl_enum_Generator_Type {
  /*! The generator is a line. */
  PPL_GENERATOR_TYPE_LINE,
  /*! The generator is a ray. */
  PPL_GENERATOR_TYPE_RAY,
  /*! The generator is a point. */
  PPL_GENERATOR_TYPE_POINT,
  /*! The generator is a closure point. */
  PPL_GENERATOR_TYPE_CLOSURE_POINT
};


/*! \name Functions Related to Generators */
/*@{*/

/*! \brief
  Creates a new generator of direction \p le and type \p t.  If the
  generator to be created is a point or a closure point, the divisor
  \p d is applied to \p le.  For other types of generators \p d is
  simply disregarded.  An handle for the new generator is written at
  address \p pg.  The space dimension of the new generator is equal to
  the space dimension of \p le.
*/
int
ppl_new_Generator __P((ppl_Generator_t* pg,
		       ppl_const_LinExpression_t le,
		       enum ppl_enum_Generator_Type t,
		       ppl_const_Coefficient_t d));

/*! \brief
  Creates the point that is the origin of the zero-dimensional space
  \f$\Rset^0\f$.  Writes an handle for the new generator at address
  \p pg.
*/
int
ppl_new_Generator_zero_dim_point __P((ppl_Generator_t* pg));

/*! \brief
  Creates, as a closure point, the point that is the origin of the
  zero-dimensional space \f$\Rset^0\f$.  Writes an handle for the new
  generator at address \p pg.
*/
int
ppl_new_Generator_zero_dim_closure_point __P((ppl_Generator_t* pg));

/*! \brief
  Builds a generator that is a copy of \p g; writes an handle
  for the newly created generator at address \p pg.
*/
int
ppl_new_Generator_from_Generator __P((ppl_Generator_t* pg,
				      ppl_const_Generator_t g));

/*! \brief
  Invalidates the handle \p g: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Generator __P((ppl_const_Generator_t g));

/*! \brief
  Assigns a copy of the generator \p src to \p dst.
*/
int
ppl_assign_Generator_from_Generator __P((ppl_Generator_t dst,
					 ppl_const_Generator_t src));

/*! \brief
  Returns the space dimension of \p g.
*/
int
ppl_Generator_space_dimension __P((ppl_const_Generator_t g));

/*! \brief
  Returns the type of generator \p g.
*/
int
ppl_Generator_type __P((ppl_const_Generator_t g));

/*! \brief
  Copies into \p n the coefficient of variable \p var in
  generator \p g.
*/
int
ppl_Generator_coefficient __P((ppl_const_Generator_t g,
			       ppl_dimension_type var,
			       ppl_Coefficient_t n));

/*! \brief
  If \p g is a point or a closure point assigns its divisor to \p n.
*/
int
ppl_Generator_divisor __P((ppl_const_Generator_t g, ppl_Coefficient_t n));

/*! \brief
  Returns a positive integer if \p g is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p g is broken.  Useful for debugging purposes.
*/
int
ppl_Generator_OK __P((ppl_const_Generator_t g));

/*@}*/ /* Functions Related to Generators */

/*! \name Functions Related to Generator Systems */
/*@{*/

/*! \brief
  Builds an empty system of generators and writes an handle to it at
  address \p pgs.
*/
int
ppl_new_GenSys __P((ppl_GenSys_t* pgs));

/*
  Creates the universe zero-dimensional system of generators (i.e.,
  containing the origin only).  Writes an handle to the new system at
  address \p pgs.
*/
int
ppl_new_GenSys_zero_dim_univ __P((ppl_GenSys_t* pgs));

/*! \brief
  Builds the singleton generator system containing only a copy of
  generator \p g; writes an handle for the newly created system at
  address \p pgs.
*/
int
ppl_new_GenSys_from_Generator __P((ppl_GenSys_t* pgs,
				   ppl_const_Generator_t g));

/*! \brief
  Builds a generator system that is a copy of \p gs; writes an handle
  for the newly created system at address \p pgs.
*/
int
ppl_new_GenSys_from_GenSys __P((ppl_GenSys_t* pgs, ppl_const_GenSys_t gs));

/*! \brief
  Invalidates the handle \p gs: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_GenSys __P((ppl_const_GenSys_t gs));

/*! \brief
  Assigns a copy of the generator system \p src to \p dst.
*/
int
ppl_assign_GenSys_from_GenSys __P((ppl_GenSys_t dst,
				   ppl_const_GenSys_t src));

/*! \brief
  Returns the dimension of the vector space enclosing \p gs.
*/
int
ppl_GenSys_space_dimension __P((ppl_const_GenSys_t gs));

/*! \brief
  Removes all the generators from the generator system \p gs
  and sets its space dimension to 0.
*/
int
ppl_GenSys_clear __P((ppl_GenSys_t gs));

/*! \brief
  Inserts a copy of the generator \p g into \p gs; the space
  dimension is increased, if necessary.
*/
int
ppl_GenSys_insert_Generator __P((ppl_GenSys_t gs, ppl_const_Generator_t g));

/*! \brief
  Returns a positive integer if \p gs is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p gs is broken.  Useful for debugging purposes.
*/
int
ppl_GenSys_OK __P((ppl_const_GenSys_t c));


/*! \brief
  Builds a new `const iterator' and writes an handle to it at address
  \p pgit.
*/
int
ppl_new_GenSys_const_iterator __P((ppl_GenSys_const_iterator_t* pgit));

/*! \brief
  Builds a const iterator system that is a copy of \p git; writes an
  handle for the newly created const iterator at address \p pgit.
*/
int
ppl_new_GenSys_const_iterator_from_GenSys_const_iterator
__P((ppl_GenSys_const_iterator_t* pgit,
     ppl_const_GenSys_const_iterator_t git));

/*! \brief
  Invalidates the handle \p git: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_GenSys_const_iterator
__P((ppl_const_GenSys_const_iterator_t git));

/*! \brief
  Assigns a copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_GenSys_const_iterator_from_GenSys_const_iterator
__P((ppl_GenSys_const_iterator_t dst,
     ppl_const_GenSys_const_iterator_t src));

/*! \brief
  Assigns to \p git a const iterator "pointing" to the beginning of
  the generator system \p gs.
*/
int
ppl_GenSys_begin __P((ppl_const_GenSys_t gs,
		      ppl_GenSys_const_iterator_t git));

/*! \brief
  Assigns to \p git a const iterator "pointing" past the end of the
  generator system \p gs.
*/
int
ppl_GenSys_end __P((ppl_const_GenSys_t gs,
		    ppl_GenSys_const_iterator_t git));

/*! \brief
  Dereference \p git writing a const handle to the resulting
  generator at address \p pg.
*/
int
ppl_GenSys_const_iterator_dereference
__P((ppl_const_GenSys_const_iterator_t git,
     ppl_const_Generator_t* pg));

/*! \brief
  Increment \p git so that it "points" to the next generator.
*/
int
ppl_GenSys_const_iterator_increment __P((ppl_GenSys_const_iterator_t git));

/*! \brief
  Return a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_GenSys_const_iterator_equal_test
__P((ppl_const_GenSys_const_iterator_t x,
     ppl_const_GenSys_const_iterator_t y));

/*@}*/ /* Functions Related to Generator Systems */

/*! \brief
  Code of the worst-case polynomial complexity class.
*/
extern unsigned int PPL_COMPLEXITY_CLASS_POLYNOMIAL;

/*! \brief
  Code of the worst-case exponential but typically polynomial
  complexity class.
*/
extern unsigned int PPL_COMPLEXITY_CLASS_SIMPLEX;

/*! \brief
  Code of the universal complexity class.
*/
extern unsigned int PPL_COMPLEXITY_CLASS_ANY;

/*! \brief
  Individual bit saying that the polyhedron and the set of points
  satisfying the constraint are disjoint.
*/
extern unsigned int PPL_POLY_CON_RELATION_IS_DISJOINT;

/*! \brief
  Individual bit saying that the polyhedron intersects the set of
  points satisfying the constraint, but it is not included in it.
*/
extern unsigned int PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS;

/*! \brief
  Individual bit saying that the polyhedron is included in the set of
  points satisfying the constraint.
*/
extern unsigned int PPL_POLY_CON_RELATION_IS_INCLUDED;

/*! \brief
  Individual bit saying that the polyhedron is included in the set of
  points saturating the constraint.
*/
extern unsigned int PPL_POLY_CON_RELATION_SATURATES;

/*! \brief
  Individual bit saying that adding the generator would not change the
  polyhedron.
*/
extern unsigned int PPL_POLY_GEN_RELATION_SUBSUMES;

/*! \name Functions Related to Polyhedra */
/*@{*/

/*! \brief
  Builds an universe closed polyhedron of dimension \p d and writes an
  handle to it at address \p pph.
*/
int
ppl_new_C_Polyhedron_from_dimension __P((ppl_Polyhedron_t* pph,
					 ppl_dimension_type d));

/*! \brief
  Builds an universe NNC polyhedron of dimension \p d and writes an
  handle to it at address \p pph.
*/
int
ppl_new_NNC_Polyhedron_from_dimension __P((ppl_Polyhedron_t* pph,
					   ppl_dimension_type d));

/*! \brief
  Builds an empty closed polyhedron of dimension \p d and writes an
  handle to it at address \p pph.
*/
int
ppl_new_C_Polyhedron_empty_from_dimension __P((ppl_Polyhedron_t* pph,
					       ppl_dimension_type d));

/*! \brief
  Builds an empty NNC polyhedron of dimension \p d and writes an
  handle to it at address \p pph.
*/
int
ppl_new_NNC_Polyhedron_empty_from_dimension __P((ppl_Polyhedron_t* pph,
					       ppl_dimension_type d));

/*! \brief
  Builds a closed polyhedron that is a copy of \p ph; writes an handle
  for the newly created polyhedron at address \p pph.
*/
int
ppl_new_C_Polyhedron_from_C_Polyhedron __P((ppl_Polyhedron_t* pph,
					    ppl_const_Polyhedron_t ph));

/*! \brief
  Builds a closed polyhedron that is a copy of of the NNC polyhedron
  \p ph; writes an handle for the newly created polyhedron at address
  \p pph.
*/
int
ppl_new_C_Polyhedron_from_NNC_Polyhedron __P((ppl_Polyhedron_t* pph,
					      ppl_const_Polyhedron_t ph));

/*! \brief
  Builds an NNC polyhedron that is a copy of of the closed polyhedron
  \p ph; writes an handle for the newly created polyhedron at address
  \p pph.
*/
int
ppl_new_NNC_Polyhedron_from_C_Polyhedron __P((ppl_Polyhedron_t* pph,
					      ppl_const_Polyhedron_t ph));

/*! \brief
  Builds an NNC polyhedron that is a copy of \p ph; writes an handle
  for the newly created polyhedron at address \p pph.
*/
int
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron __P((ppl_Polyhedron_t* pph,
						ppl_const_Polyhedron_t ph));

/*! \brief
  Builds a new closed polyhedron from the system of constraints
  \p cs and writes an handle for the newly created polyhedron at
  address \p pph.  The new polyhedron will inherit the space dimension
  of \p cs.
*/
int
ppl_new_C_Polyhedron_from_ConSys __P((ppl_Polyhedron_t* pph,
				      ppl_const_ConSys_t cs));

/*! \brief
  Builds a new closed polyhedron recycling the system of constraints
  \p cs and writes an handle for the newly created polyhedron at
  address \p pph.  Since \p cs will be <EM>the</EM> system of
  constraints of the new polyhedron, the space dimension is also
  inherited.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_C_Polyhedron_recycle_ConSys __P((ppl_Polyhedron_t* pph,
					 ppl_ConSys_t cs));

/*! \brief
  Builds a new NNC polyhedron from the system of constraints
  \p cs and writes an handle for the newly created polyhedron at
  address \p pph.  The new polyhedron will inherit the space dimension
  of \p cs.
*/
int
ppl_new_NNC_Polyhedron_from_ConSys __P((ppl_Polyhedron_t* pph,
					ppl_const_ConSys_t cs));

/*! \brief
  Builds a new NNC polyhedron recycling the system of constraints
  \p cs and writes an handle for the newly created polyhedron at
  address \p pph.  Since \p cs will be <EM>the</EM> system of
  constraints of the new polyhedron, the space dimension is also
  inherited.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_NNC_Polyhedron_recycle_ConSys __P((ppl_Polyhedron_t* pph,
					   ppl_ConSys_t cs));

/*! \brief
  Builds a new closed polyhedron from the system of generators
  \p gs and writes an handle for the newly created polyhedron at
  address \p pph.  The new polyhedron will inherit the space dimension
  of \p gs.
*/
int
ppl_new_C_Polyhedron_from_GenSys __P((ppl_Polyhedron_t* pph,
				      ppl_const_GenSys_t gs));

/*! \brief
  Builds a new closed polyhedron recycling the system of generators
  \p gs and writes an handle for the newly created polyhedron at
  address \p pph.  Since \p gs will be <EM>the</EM> system of
  generators of the new polyhedron, the space dimension is also
  inherited.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_C_Polyhedron_recycle_GenSys __P((ppl_Polyhedron_t* pph,
					 ppl_GenSys_t gs));

/*! \brief
  Builds a new NNC polyhedron from the system of generators
  \p gs and writes an handle for the newly created polyhedron at
  address \p pph.  The new polyhedron will inherit the space dimension
  of \p gs.
*/
int
ppl_new_NNC_Polyhedron_from_GenSys __P((ppl_Polyhedron_t* pph,
					ppl_const_GenSys_t gs));

/*! \brief
  Builds a new NNC polyhedron recycling the system of generators
  \p gs and writes an handle for the newly created polyhedron at
  address \p pph.  Since \p gs will be <EM>the</EM> system of
  generators of the new polyhedron, the space dimension is also
  inherited.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_NNC_Polyhedron_recycle_GenSys __P((ppl_Polyhedron_t* pph,
					   ppl_GenSys_t gs));

/*! \brief
  Builds a new C polyhedron corresponding to an interval-based
  bounding box, writing a handle for the newly created polyhedron at
  address \p pph.

  If an interval of the bounding box is provided with any finite
  but open bound, then the polyhedron is not built and the value
  <CODE>PPL_ERROR_INVALID_ARGUMENT</CODE> is returned.
  The bounding box is accessed by using the following functions,
  passed as arguments:
    \code
      ppl_dimension_type space_dimension()
    \endcode
    returns the dimension of the vector space enclosing the polyhedron
    represented by the bounding box.
    \code
      int is_empty()
    \endcode
    returns 0 if and only if the bounding box describes a non-empty set.
    The function <CODE>is_empty()</CODE> will always be called before the
    other functions. However, if <CODE>is_empty()</CODE> does not
    return 0, none of the functions below will be called.
    \code
      int get_lower_bound(ppl_dimension_type k, int closed,
                          ppl_Coefficient_t n, ppl_Coefficient_t d)
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    dimension.  If \f$I\f$ is not bounded from below, simply return 0.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and
    <CODE>d</CODE> as follows: <CODE>closed</CODE> is set to 0 if the
    lower boundary of \f$I\f$ is open and is set to a value different
    from zero otherwise; <CODE>n</CODE> and <CODE>d</CODE> are
    assigned the integers \f$n\f$ and \f$d\f$ such that the canonical
    fraction \f$n/d\f$ corresponds to the greatest lower bound of
    \f$I\f$.  The fraction \f$n/d\f$ is in canonical form if and only
    if \f$n\f$ and \f$d\f$ have no common factors and \f$d\f$ is
    positive, \f$0/1\f$ being the unique representation for zero.
    \code
      int get_upper_bound(ppl_dimension_type k, int closed,
                          ppl_Coefficient_t n, ppl_Coefficient_t d)
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    dimension.  If \f$I\f$ is not bounded from above, simply return 0.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and
    <CODE>d</CODE> as follows: <CODE>closed</CODE> is set to 0 if the
    upper boundary of \f$I\f$ is open and is set to a value different
    from 0 otherwise; <CODE>n</CODE> and <CODE>d</CODE> are assigned
    the integers \f$n\f$ and \f$d\f$ such that the canonical fraction
    \f$n/d\f$ corresponds to the least upper bound of \f$I\f$.
*/
int
ppl_new_C_Polyhedron_from_bounding_box
__P((ppl_Polyhedron_t* pph,
     ppl_dimension_type (*space_dimension)(void),
     int (*is_empty)(void),
     int (*get_lower_bound)(ppl_dimension_type k, int closed,
			    ppl_Coefficient_t n,
			    ppl_Coefficient_t d),
     int (*get_upper_bound)(ppl_dimension_type k, int closed,
			    ppl_Coefficient_t n,
			    ppl_Coefficient_t d)));

/*! \brief
  Builds a new C polyhedron corresponding to an interval-based
  bounding box, writing a handle for the newly created polyhedron at
  address \p pph.

  The bounding box is accessed by using the following functions,
  passed as arguments:
    \code
      ppl_dimension_type space_dimension()
    \endcode
    returns the dimension of the vector space enclosing the polyhedron
    represented by the bounding box.
    \code
      int is_empty()
    \endcode
    returns 0 if and only if the bounding box describes a non-empty set.
    The function <CODE>is_empty()</CODE> will always be called before the
    other functions. However, if <CODE>is_empty()</CODE> does not
    return 0, none of the functions below will be called.
    \code
      int get_lower_bound(ppl_dimension_type k, int closed,
                          ppl_Coefficient_t n, ppl_Coefficient_t d)
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    dimension.  If \f$I\f$ is not bounded from below, simply return 0.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and
    <CODE>d</CODE> as follows: <CODE>closed</CODE> is set to 0 if the
    lower boundary of \f$I\f$ is open and is set to a value different
    from zero otherwise; <CODE>n</CODE> and <CODE>d</CODE> are
    assigned the integers \f$n\f$ and \f$d\f$ such that the canonical
    fraction \f$n/d\f$ corresponds to the greatest lower bound of
    \f$I\f$.  The fraction \f$n/d\f$ is in canonical form if and only
    if \f$n\f$ and \f$d\f$ have no common factors and \f$d\f$ is
    positive, \f$0/1\f$ being the unique representation for zero.
    \code
      int get_upper_bound(ppl_dimension_type k, int closed,
                          ppl_Coefficient_t n, ppl_Coefficient_t d)
    \endcode
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    dimension.  If \f$I\f$ is not bounded from above, simply return 0.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and
    <CODE>d</CODE> as follows: <CODE>closed</CODE> is set to 0 if the
    upper boundary of \f$I\f$ is open and is set to a value different
    from 0 otherwise; <CODE>n</CODE> and <CODE>d</CODE> are assigned
    the integers \f$n\f$ and \f$d\f$ such that the canonical fraction
    \f$n/d\f$ corresponds to the least upper bound of \f$I\f$.
*/
int
ppl_new_NNC_Polyhedron_from_bounding_box
__P((ppl_Polyhedron_t* pph,
     ppl_dimension_type (*space_dimension)(void),
     int (*is_empty)(void),
     int (*get_lower_bound)(ppl_dimension_type k, int closed,
			    ppl_Coefficient_t n,
			    ppl_Coefficient_t d),
     int (*get_upper_bound)(ppl_dimension_type k, int closed,
			    ppl_Coefficient_t n,
			    ppl_Coefficient_t d)));

/*! \brief
  Assigns a copy of the closed polyhedron \p src to the closed
  polyhedron \p dst.
*/
int
ppl_assign_C_Polyhedron_from_C_Polyhedron __P((ppl_Polyhedron_t dst,
					       ppl_const_Polyhedron_t src));

/*! \brief
  Assigns a copy of the NNC polyhedron \p src to the NNC
  polyhedron \p dst.
*/
int
ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron
__P((ppl_Polyhedron_t dst, ppl_const_Polyhedron_t src));

/*! \brief
  Invalidates the handle \p ph: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Polyhedron __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Returns the dimension of the vector space enclosing \p ph.
*/
int
ppl_Polyhedron_space_dimension __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Writes a const handle to the constraint system defining the
  polyhedron \p ph at address \p pcs.
*/
int
ppl_Polyhedron_constraints __P((ppl_const_Polyhedron_t ph,
				ppl_const_ConSys_t* pcs));

/*! \brief
  Writes a const handle to the minimized constraint system defining the
  polyhedron \p ph at address \p pcs.
*/
int
ppl_Polyhedron_minimized_constraints __P((ppl_const_Polyhedron_t ph,
					  ppl_const_ConSys_t* pcs));

/*! \brief
  Writes a const handle to the generator system defining the
  polyhedron \p ph at address \p pgs.
*/
int
ppl_Polyhedron_generators __P((ppl_const_Polyhedron_t ph,
			       ppl_const_GenSys_t* pgs));

/*! \brief
  Writes a const handle to the minimized generator system defining the
  polyhedron \p ph at address \p pgs.
*/
int
ppl_Polyhedron_minimized_generators __P((ppl_const_Polyhedron_t ph,
					 ppl_const_GenSys_t* pgs));

/*! \brief
  Checks the relation between the polyhedron \p ph with the constraint \p c.

  If successful, returns a non-negative integer that is
  obtained as the bitwise or of the bits (chosen among
  PPL_POLY_CON_RELATION_IS_DISJOINT
  PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS,
  PPL_POLY_CON_RELATION_IS_INCLUDED, and
  PPL_POLY_CON_RELATION_SATURATES) that describe the relation between
  \p ph and \p c.
*/
int
ppl_Polyhedron_relation_with_Constraint __P((ppl_const_Polyhedron_t ph,
					     ppl_const_Constraint_t c));

/*! \brief
  Checks the relation between the polyhedron \p ph with the generator \p g.

  If successful, returns a non-negative integer that is
  obtained as the bitwise or of the bits (only
  PPL_POLY_GEN_RELATION_SUBSUMES, at present) that describe the
  relation between \p ph and \p g.
*/
int
ppl_Polyhedron_relation_with_Generator __P((ppl_const_Polyhedron_t ph,
					    ppl_const_Generator_t g));

/*! \brief
  Use \p ph to shrink a generic, interval-based bounding box.
  The bounding box is abstractly provided by means of the parameters,

  \param complexity
  The code of the complexity class of the algorithm to be used.
  Must be one of PPL_COMPLEXITY_CLASS_POLYNOMIAL,
  PPL_COMPLEXITY_CLASS_SIMPLEX, or PPL_COMPLEXITY_CLASS_ANY.

  \param ph
  The polyhedron that is used to shrink the bounding box.

  \param set_empty
  a pointer to a void function with no arguments that causes the bounding
  box to become empty, i.e., to represent the empty set.

  \param raise_lower_bound
  a pointer to a void function with arguments
  <CODE>(ppl_dimension_type k, int closed,
         ppl_const_Coefficient_t n, ppl_const_Coefficient_t d)</CODE>
  that intersects the interval corresponding to the <CODE>k</CODE>-th
  dimension with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is non-zero,
  with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is zero.
  The fraction \f$n/d\f$ is in canonical form, that is, \f$n\f$
  and \f$d\f$ have no common factors and \f$d\f$ is positive, \f$0/1\f$
  being the unique representation for zero.

  \param lower_upper_bound
  a pointer to a void function with argument
  <CODE>(ppl_dimension_type k, int closed,
         ppl_const_Coefficient_t n, ppl_const_Coefficient_t d)</CODE>
  that intersects the interval corresponding to the <CODE>k</CODE>-th
  dimension with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is non-zero,
  with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE> is zero.
  The fraction \f$n/d\f$ is in canonical form.
*/
int
ppl_Polyhedron_shrink_bounding_box
__P((ppl_const_Polyhedron_t ph,
     unsigned int complexity,
     void (*set_empty)(void),
     void (*raise_lower_bound)(ppl_dimension_type k, int closed,
			       ppl_const_Coefficient_t n,
			       ppl_const_Coefficient_t d),
     void (*lower_upper_bound)(ppl_dimension_type k, int closed,
			       ppl_const_Coefficient_t n,
			       ppl_const_Coefficient_t d)));

/*! \brief
  Returns a positive integer if \p ph is empty; returns 0 if \p ph is
  not empty.
*/
int
ppl_Polyhedron_is_empty __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Returns a positive integer if \p ph is a universe polyhedron;
  returns 0 if it is not.
*/
int
ppl_Polyhedron_is_universe __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Returns a positive integer if \p ph is bounded; returns 0 if \p ph is
  unbounded.
*/
int
ppl_Polyhedron_is_bounded __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Returns a positive integer if \p le is bounded from above in \p ph;
  returns 0 otherwise.
*/
int
ppl_Polyhedron_bounds_from_above __P((ppl_const_Polyhedron_t ph,
				      ppl_const_LinExpression_t le));

/*! \brief
  Returns a positive integer if \p le is bounded from below in \p ph;
  returns 0 otherwise.
*/
int
ppl_Polyhedron_bounds_from_below __P((ppl_const_Polyhedron_t ph,
				      ppl_const_LinExpression_t le));

/*! \brief
  Returns a positive integer if  \p ph is not empty
  and \p le is bounded from above in \p ph, in which case
  the supremum value and a point where \p expr reaches it are computed.
  \param ph       The polyhedron constraining \p le;
  \param le        The linear expression to be maximized subject to \p ph;
  \param sup_n     Will be assigned the numerator of the supremum value;
  \param sup_d     Will be assigned the denominator of the supremum value;
  \param pmaximum  Will store 1 in this location if the supremum is also
                   the maximum, will store 0 otherwise;
  \param ppoint    When nonzero, a point or closure point where \p le
                   reaches the extremum value will be stored here.
  If \p ph is empty or \p le is not bounded from above,
  0 is returned and \p sup_n, \p sup_d, \p *pmaximum and \p *ppoint
  are left untouched.
*/
int
ppl_Polyhedron_maximize __P((ppl_const_Polyhedron_t ph,
			     ppl_const_LinExpression_t le,
			     ppl_Coefficient_t sup_n,
			     ppl_Coefficient_t sup_d,
			     int* pmaximum,
			     ppl_const_Generator_t* ppoint));

/*! \brief
  Returns a positive integer if  \p ph is not empty
  and \p le is bounded from above in \p ph, in which case
  the infimum value and a point where \p expr reaches it are computed.
  \param ph       The polyhedron constraining \p le;
  \param le        The linear expression to be minimized subject to \p ph;
  \param inf_n     Will be assigned the numerator of the infimum value;
  \param inf_d     Will be assigned the denominator of the infimum value;
  \param pminimum  Will store 1 in this location if the infimum is also
                   the minimum, will store 0 otherwise;
  \param ppoint    When nonzero, a point or closure point where \p le
                   reaches the extremum value will be stored here.
  If \p ph is empty or \p le is not bounded from below,
  0 is returned and \p inf_n, \p inf_d, \p *pminimum and \p *ppoint
  are left untouched.
*/
int
ppl_Polyhedron_minimize __P((ppl_const_Polyhedron_t ph,
			     ppl_const_LinExpression_t le,
			     ppl_Coefficient_t inf_n,
			     ppl_Coefficient_t inf_d,
			     int* pminimum,
			     ppl_const_Generator_t* ppoint));

/*! \brief
  Returns a positive integer if \p ph is topologically closed;
  returns 0 if \p ph is not topologically closed.
*/
int
ppl_Polyhedron_is_topologically_closed __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Returns a positive integer if \p x contains or is equal to \p y;
  returns 0 if it does not.
*/
int
ppl_Polyhedron_contains_Polyhedron __P((ppl_const_Polyhedron_t x,
					ppl_const_Polyhedron_t y));

/*! \brief
  Returns a positive integer if \p x strictly contains \p y; returns 0
  if it does not.
*/
int
ppl_Polyhedron_strictly_contains_Polyhedron __P((ppl_const_Polyhedron_t x,
						 ppl_const_Polyhedron_t y));

/*! \brief
  Returns a positive integer if \p x and \p y are disjoint; returns 0
  if they are not.
*/
int
ppl_Polyhedron_is_disjoint_from_Polyhedron __P((ppl_const_Polyhedron_t x,
						ppl_const_Polyhedron_t y));

/*! \brief
  Returns a positive integer if \p x and \p y are the same polyhedron;
  return 0 if they are different.

  Note that \p x and \p y may be topology- and/or dimension-incompatible
  polyhedra: in those cases, the value 0 is returned.
*/
int
ppl_Polyhedron_equals_Polyhedron __P((ppl_const_Polyhedron_t x,
				      ppl_const_Polyhedron_t y));

/*! \brief
  Returns a positive integer if \p ph is well formed, i.e., if it
  satisfies all its implementation invariants; returns 0 and perhaps
  make some noise if \p ph is broken.  Useful for debugging purposes.
*/
int
ppl_Polyhedron_OK __P((ppl_const_Polyhedron_t ph));

/*! \brief
  Adds a copy of the constraint \p c to the system of constraints of
  \p ph.
*/
int
ppl_Polyhedron_add_constraint __P((ppl_Polyhedron_t ph,
				   ppl_const_Constraint_t c));

/*! \brief
  Adds a copy of the constraint \p c to the system of constraints of
  \p ph.  Returns a positive integer if the resulting polyhedron is
  non-empty; returns 0 if it is empty.  Upon successful return, \p ph
  is guaranteed to be minimized.

*/
int
ppl_Polyhedron_add_constraint_and_minimize __P((ppl_Polyhedron_t ph,
						ppl_const_Constraint_t c));

/*! \brief
  Adds a copy of the generator \p g to the system of generators of
  \p ph.
*/
int
ppl_Polyhedron_add_generator __P((ppl_Polyhedron_t ph,
				  ppl_const_Generator_t g));

/*! \brief
  Adds a copy of the generator \p g to the system of generators of
  \p ph.  Returns a positive integer if the resulting polyhedron is
  non-empty; returns 0 if it is empty.  Upon successful return, \p ph
  is guaranteed to be minimized.
*/
int
ppl_Polyhedron_add_generator_and_minimize __P((ppl_Polyhedron_t ph,
					       ppl_const_Generator_t g));

/*! \brief
  Adds the system of constraints \p cs to the system of constraints of
  \p ph.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_constraints __P((ppl_Polyhedron_t ph, ppl_ConSys_t cs));

/*! \brief
  Adds the system of constraints \p cs to the system of constraints of
  \p ph.  Returns a positive integer if the resulting polyhedron is
  non-empty; returns 0 if it is empty.  Upon successful return, \p ph
  is guaranteed to be minimized.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_constraints_and_minimize __P((ppl_Polyhedron_t ph,
						 ppl_ConSys_t cs));

/*! \brief
  Adds the system of generators \p gs to the system of generators of
  \p ph.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_generators __P((ppl_Polyhedron_t ph, ppl_GenSys_t gs));

/*! \brief
  Adds the system of generators \p gs to the system of generators of
  \p ph.  Returns a positive integer if the resulting polyhedron is
  non-empty; returns 0 if it is empty.  Upon successful return, \p ph
  is guaranteed to be minimized.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_generators_and_minimize __P((ppl_Polyhedron_t ph,
						ppl_GenSys_t gs));

/*! \brief
  Intersects \p x with polyhedron \p y and assigns the result \p x.
*/
int
ppl_Polyhedron_intersection_assign __P((ppl_Polyhedron_t x,
					ppl_const_Polyhedron_t y));

/*! \brief
  Intersects \p x with polyhedron \p y and assigns the result \p x.
  Returns a positive integer if the resulting polyhedron is non-empty;
  returns 0 if it is empty.  Upon successful return, \p x is also
  guaranteed to be minimized.
*/
int
ppl_Polyhedron_intersection_assign_and_minimize
__P((ppl_Polyhedron_t x, ppl_const_Polyhedron_t y));

/*! \brief
  Assigns to \p x the poly-hull of the set-theoretic union
  of \p x and \p y.
*/
int
ppl_Polyhedron_poly_hull_assign __P((ppl_Polyhedron_t x,
				     ppl_const_Polyhedron_t y));

/*! \brief
  Assigns to \p x the poly-hull of the set-theoretic union of \p x
  and \p y.  Returns a positive integer if the resulting polyhedron is
  non-empty; returns 0 if it is empty.  Upon successful return, \p x is
  also guaranteed to be minimized.
*/
int
ppl_Polyhedron_poly_hull_assign_and_minimize __P((ppl_Polyhedron_t x,
						  ppl_const_Polyhedron_t y));

/*! \brief
  Assigns to \p x the poly-hull of the set-theoretic difference
  of \p x and \p y.
*/
int
ppl_Polyhedron_poly_difference_assign __P((ppl_Polyhedron_t x,
					   ppl_const_Polyhedron_t y));

/*! \brief
  Transforms the polyhedron \p ph, assigning an affine expression
  to the specified variable.
  \param ph  The polyhedron that is transformed;
  \param var The variable to which the affine expression is assigned;
  \param le  The numerator of the affine expression;
  \param d   The denominator of the affine expression.
*/
int
ppl_Polyhedron_affine_image __P((ppl_Polyhedron_t ph,
				 ppl_dimension_type var,
				 ppl_const_LinExpression_t le,
				 ppl_const_Coefficient_t d));

/*! \brief
  Transforms the polyhedron \p ph, substituting an affine expression
  to the specified variable.
  \param ph  The polyhedron that is transformed;
  \param var The variable to which the affine expression is substituted;
  \param le  The numerator of the affine expression;
  \param d   The denominator of the affine expression.
*/
int
ppl_Polyhedron_affine_preimage __P((ppl_Polyhedron_t ph,
				    ppl_dimension_type var,
				    ppl_const_LinExpression_t le,
				    ppl_const_Coefficient_t d));

/*! \brief
  Assigns to \p ph the image of \p ph with respect to the
  \ref generalized_image "generalized affine transfer function"
  \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
  where \f$\mathord{\relsym}\f$ is the relation symbol encoded
  by \p relsym.
  \param ph     The polyhedron that is transformed;
  \param var    The left hand side variable of the generalized
                affine transfer function;
  \param relsym The relation symbol;
  \param le     The numerator of the right hand side affine expression;
  \param d      The denominator of the right hand side affine expression.
*/
int
ppl_Polyhedron_generalized_affine_image
__P((ppl_Polyhedron_t ph,
     ppl_dimension_type var,
     enum ppl_enum_Constraint_Type relsym,
     ppl_const_LinExpression_t le,
     ppl_const_Coefficient_t d));

/*! \brief
  Assigns to \p ph the image of \p ph with respect to the
  \ref generalized_image "generalized affine transfer function"
  \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
  \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.
  \param ph            The polyhedron that is transformed;
  \param lhs           The left hand side affine expression;
  \param relsym        The relation symbol;
  \param rhs           The right hand side affine expression.
*/
int
ppl_Polyhedron_generalized_affine_image_lhs_rhs
__P((ppl_Polyhedron_t ph,
     ppl_const_LinExpression_t lhs,
     enum ppl_enum_Constraint_Type relsym,
     ppl_const_LinExpression_t rhs));

/*! \brief
  Assigns to \p x the \ref time_elapse "time-elapse" between the polyhedra
  \p x and \p y.
*/
int
ppl_Polyhedron_time_elapse_assign __P((ppl_Polyhedron_t x,
				       ppl_const_Polyhedron_t y));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y.  If \p tp is not the null pointer, the
  \ref widening_with_tokens "widening with tokens" delay technique
  is applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_BHRZ03_widening_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y.
*/
int
ppl_Polyhedron_BHRZ03_widening_assign __P((ppl_Polyhedron_t x,
					   ppl_const_Polyhedron_t y));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x.  If \p tp is not the null pointer,
  the \ref widening_with_tokens "widening with tokens" delay technique
  is applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x.
*/
int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x, further intersected with all
  the constraints of the form \f$\pm v \leq r\f$ and \f$\pm v < r\f$,
  with \f$r \in \Qset\f$, that are satisfied by all the points of \p
  x. If \p tp is not the null pointer,
  the \ref widening_with_tokens "widening with tokens" delay technique
  is applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref BHRZ03_widening "BHRZ03-widening" of
  \p x and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x, further intersected with all
  the constraints of the form \f$\pm v \leq r\f$ and \f$\pm v < r\f$,
  with \f$r \in \Qset\f$, that are satisfied by all the points of \p x.
*/
int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y.  If \p tp is not the null pointer, the
  \ref widening_with_tokens "widening with tokens" delay technique is
  applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_H79_widening_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y.
*/
int
ppl_Polyhedron_H79_widening_assign __P((ppl_Polyhedron_t x,
					ppl_const_Polyhedron_t y));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x. If \p tp is not the null
  pointer, the \ref widening_with_tokens "widening with tokens" delay
  technique is applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x.
*/
int
ppl_Polyhedron_limited_H79_extrapolation_assign __P((ppl_Polyhedron_t x,
						     ppl_const_Polyhedron_t y,
						     ppl_const_ConSys_t cs));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x, further intersected with all
  the constraints of the form \f$\pm v \leq r\f$ and \f$\pm v < r\f$,
  with \f$r \in \Qset\f$, that are satisfied by all the points of \p x.
  If \p tp is not the null pointer,
  the \ref widening_with_tokens "widening with tokens" delay technique
  is applied with <CODE>*tp</CODE> available tokens.
*/
int
ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens
__P((ppl_Polyhedron_t x,
     ppl_const_Polyhedron_t y,
     ppl_const_ConSys_t cs,
     unsigned* tp));

/*! \brief
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the \ref H79_widening "H79-widening" of \p x
  and \p y intersected with the constraints in \p cs that are
  satisfied by all the points of \p x, further intersected with all
  the constraints of the form \f$\pm v \leq r\f$ and \f$\pm v < r\f$,
  with \f$r \in \Qset\f$, that are satisfied by all the points of \p
  x.
*/
int
ppl_Polyhedron_bounded_H79_extrapolation_assign __P((ppl_Polyhedron_t x,
						     ppl_const_Polyhedron_t y,
						     ppl_const_ConSys_t cs));

/*! \brief
  Assigns to \p ph its topological closure.
*/
int
ppl_Polyhedron_topological_closure_assign __P((ppl_Polyhedron_t ph));

/*! \brief
  Adds \p d new dimensions to the space enclosing the polyhedron \p ph
  and to \p ph itself.
*/
int
ppl_Polyhedron_add_dimensions_and_embed __P((ppl_Polyhedron_t ph,
					     ppl_dimension_type d));

/*! \brief
  Adds \p d new dimensions to the space enclosing the polyhedron \p ph.
*/
int
ppl_Polyhedron_add_dimensions_and_project __P((ppl_Polyhedron_t ph,
					       ppl_dimension_type d));

/*! \brief
  Seeing a polyhedron as a set of tuples (its points), assigns
  to \p x all the tuples that can be obtained by concatenating,
  in the order given, a tuple of \p x with a tuple of \p y.
*/
int
ppl_Polyhedron_concatenate_assign __P((ppl_Polyhedron_t x,
				       ppl_const_Polyhedron_t y));

#ifdef SWIGOCAML
%typemap(in) (ppl_dimension_type ds[], size_t n) {
  int i;
  /* $*1_type */
  $2 = caml_array_len($input);
  $1 = ($*1_type*) malloc($2 * sizeof(ppl_dimension_type) );
  if ($1) {
    for(i = $2; i-- > 0; ) {
      $1[i] = caml_val_uint(caml_array_nth($input, i));
    }
  }
}

%typemap(freearg) (ppl_dimension_type ds[], size_t n) {
  free($1);
}
#endif

/*! \brief
  Removes from \p ph and its containing space the dimensions that are
  specified in first \p n positions of the array \p ds.  The presence
  of duplicates in \p ds is a waste but an innocuous one.
*/
int
ppl_Polyhedron_remove_dimensions __P((ppl_Polyhedron_t ph,
				      ppl_dimension_type ds[],
				      size_t n));

/*! \brief
  Removes the higher dimensions from \p ph and its enclosing space so
  that, upon successful return, the new space dimension is \p d.
*/
int
ppl_Polyhedron_remove_higher_dimensions __P((ppl_Polyhedron_t ph,
					     ppl_dimension_type d));

#ifdef SWIGOCAML
  %typemap(in) (ppl_dimension_type maps[], size_t n)
       = (ppl_dimension_type ds[], size_t n);
  %typemap(freearg) (ppl_dimension_type maps[], size_t n)
       = (ppl_dimension_type ds[], size_t n);
#endif

/*! \brief
  Remaps the dimensions of the vector space according to a
  \ref map_dimensions "partial function". This function is specified
  by means of the \p maps array, which has \p n entries.

  The partial function is defined on dimension <CODE>i</CODE>
  if <CODE>i < n</CODE> and <CODE>maps[i] != ppl_not_a_dimension</CODE>;
  otherwise it is undefined on dimension <CODE>i</CODE>.
  If the function is defined on dimension <CODE>i</CODE>, then dimension
  <CODE>i</CODE> is mapped onto dimension <CODE>maps[i]</CODE>.

  The result is undefined if \p maps does not encode a partial
  function with the properties described in the
  \ref map_dimensions "specification of the mapping operator".
*/
int
ppl_Polyhedron_map_dimensions __P((ppl_Polyhedron_t ph,
				   ppl_dimension_type maps[],
				   size_t n));

/*@}*/ /* Functions Related to Polyhedra */

#ifdef __cplusplus
} /* extern "C" */
#endif

#undef __P

/*@}*/ /* \defgroup PPL_C_interface */

#endif /* !defined(PPL_ppl_c_h) */
