/* Header file for the C interface.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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


/*! \file

This file implements the C interface.
Detailed description with examples to be written.
*/

#ifndef _ppl_c_h
#define _ppl_c_h 1

#include <gmp.h>
#include <stdio.h>

/*
  __BEGIN_DECLS should be used at the beginning of the C declarations,
  so that C++ compilers don't mangle their names.  __END_DECLS is used
  at the end of C declarations.
*/
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif

/*
  __P is a macro used to wrap function prototypes, so that compilers
  that don't understand ANSI C prototypes still work, and ANSI C
  compilers can issue warnings about type mismatches.
*/
#undef __P
#if defined (__STDC__) || defined (_AIX) \
        || (defined (__mips) && defined (_SYSTYPE_SVR4)) \
        || defined(WIN32) || defined(__cplusplus)
# define __P(protos) protos
#else
# define __P(protos) ()
#endif

__BEGIN_DECLS

/*!
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
  PPL_ERROR_UNEXPECTED_ERROR = -6,
};


/*!
  Initializes the Parma Polyhedra Library.
  This function must be called before any other function.
*/
int
ppl_initialize __P((void));

/*!
  Finalizes the Parma Polyhedra Library.
  This function must be called after any other function.
*/
int
ppl_finalize __P((void));

/*!
  Installs the user-defined error handler pointed by \p h.  The error
  handler takes an error code and a textual description that gives
  further information about the actual error.  The C string containing
  the textual description is read-only and its existence it not
  guaranteed after the handler has returned.
*/
int
ppl_set_error_handler __P((void (*h)(enum ppl_enum_error_code code,
				     const char* description)));


#define PPL_TYPE_DECLARATION(Type) \
/*! Opaque pointer to Type. */ \
typedef struct ppl_ ## Type ## _tag* ppl_ ## Type ## _t; \
/*! Opaque pointer to const Type. */ \
typedef struct ppl_ ## Type ## _tag const* ppl_const_ ## Type ## _t


PPL_TYPE_DECLARATION(Coefficient);

/*!
  Creates a new coefficent with value 0 and writes an handle for the
  newly created coefficient at address \p pc.
*/
int
ppl_new_Coefficient __P((ppl_Coefficient_t* pc));

/*!
  Creates a new coefficent with the value given by the GMP integer
  \p z and writes an handle for the newly created coefficient
  at address \p pc.
*/
int
ppl_new_Coefficient_from_mpz_t __P((ppl_Coefficient_t* pc, mpz_t z));

/*!
  Builds a coefficient that is a copy of \p c; writes an handle
  for the newly created coefficient at address \p pc.
*/
int
ppl_new_Coefficient_from_Coefficient __P((ppl_Coefficient_t* pc,
					  ppl_const_Coefficient_t c));

/*!
  Assign to \p dst the value given by the GMP integer \p z.
*/
int
ppl_assign_Coefficient_from_mpz_t __P((ppl_Coefficient_t dst, mpz_t z));

/*!
  Assigns a copy of the linear expression \p src to \p dst.
*/
int
ppl_assign_Coefficient_from_Coefficient __P((ppl_Coefficient_t dst,
					     ppl_const_Coefficient_t src));

/*!
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Coefficient __P((ppl_const_Coefficient_t c));

/*!
  Sets the value of the GMP integer \p z to the value of \p c.
*/
int
ppl_Coefficient_to_mpz_t __P((ppl_const_Coefficient_t c, mpz_t z));

/*!
  Returns a positive integer if \p c is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p c is broken.  Useful for debugging purposes.
*/
int
ppl_Coefficient_OK __P((ppl_const_Coefficient_t c));


PPL_TYPE_DECLARATION(LinExpression);

/*!
  Creates a new linear expression corresponding to the constant 0 in a
  zero-dimensional space; writes an handle for the new linear
  expression at address \p ple.
*/
int
ppl_new_LinExpression __P((ppl_LinExpression_t* ple));

/*!
  Creates a new linear expression corresponding to the constant 0 in a
  <TT>d</TT>-dimensional space; writes an handle for the new linear
  expression at address \p ple.
*/
int
ppl_new_LinExpression_with_dimension __P((ppl_LinExpression_t* ple,
					  unsigned int d));

/*!
  Builds a linear expression that is a copy of \p le; writes an handle
  for the newly created linear expression at address \p ple.
*/
int
ppl_new_LinExpression_from_LinExpression __P((ppl_LinExpression_t* ple,
					      ppl_const_LinExpression_t le));

/*!
  Invalidates the handle \p le: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_LinExpression __P((ppl_const_LinExpression_t le));

/*!
  Assigns a copy of the linear expression \p src to \p dst.
*/
int
ppl_assign_LinExpression_from_LinExpression
__P((ppl_LinExpression_t dst, ppl_const_LinExpression_t src));

/*!
  Swaps the linear expressions \p x and \p y.
*/
int
ppl_swap_LinExpression __P((ppl_LinExpression_t x, ppl_LinExpression_t y));

/*!
  Adds \p n to the coefficient of variable \p var in the linear
  expression \p le.  The space dimension is set to be the maximum
  between \p var + 1 and the old space dimension.
*/
int
ppl_LinExpression_add_to_coefficient __P((ppl_LinExpression_t le,
					  unsigned int var,
					  ppl_const_Coefficient_t n));

/*!
  Adds \p n to the inhomogeneous term of the linear expression
  \p le.
*/
int
ppl_LinExpression_add_to_inhomogeneous __P((ppl_LinExpression_t le,
					    ppl_const_Coefficient_t n));

/*!
  Returns the space dimension of \p le.
*/
int
ppl_LinExpression_space_dimension __P((ppl_const_LinExpression_t le));

/*!
  Returns a positive integer if \p le is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p le is broken.  Useful for debugging purposes.
*/
int
ppl_LinExpression_OK __P((ppl_const_LinExpression_t le));


PPL_TYPE_DECLARATION(Constraint);

/*!
  Describes the relations represented by a constraint.
*/
enum ppl_enum_Constraint_Type {
  /*! The constraint is of the form \f$e = 0\f$. */
  PPL_CONSTRAINT_TYPE_EQUAL,
  /*! The constraint is of the form \f$e \geq 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL,
  /*! The constraint is of the form \f$e < 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN,
  /*! The constraint is of the form \f$e \leq 0\f$. */
  PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL,
  /*! The constraint is of the form \f$e < 0\f$. */
  PPL_CONSTRAINT_TYPE_LESS_THAN
};


/*!
  Creates the new constraint `\p le \p rel 0' and writes an handle for
  it at address \p pc.  The space dimension of the new constraint is
  equal to the space dimension of \p le.
*/
int
ppl_new_Constraint __P((ppl_Constraint_t* pc,
			ppl_const_LinExpression_t le,
			enum ppl_enum_Constraint_Type));

/*!
  Creates the unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$
  and writes an handle for it at address \p pc.
*/
int
ppl_new_Constraint_zero_dim_false __P((ppl_Constraint_t* pc));

/*!
  Creates the true (zero-dimension space) constraint \f$0 \leq 1\f$,
  also known as <EM>positivity constraint</EM>.
  An handle for the newly created constraint is written at address \p pc.
*/
int
ppl_new_Constraint_zero_dim_positivity __P((ppl_Constraint_t* pc));

/*!
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Constraint __P((ppl_const_Constraint_t c));

/*!
  Assigns a copy of the constraint \p src to \p dst.
*/
int
ppl_assign_Constraint_from_Constraint __P((ppl_Constraint_t dst,
					   ppl_const_Constraint_t src));

/*!
  Swaps the constraints \p x and \p y.
*/
int
ppl_swap_Constraint __P((ppl_Constraint_t x, ppl_Constraint_t y));

/*!
  Returns the space dimension of \p c.
*/
int
ppl_Constraint_space_dimension __P((ppl_const_Constraint_t c));

/*!
  Returns the type of constraint \p c.
*/
int
ppl_Constraint_type __P((ppl_const_Constraint_t c));

/*!
  Copies into \p n the coefficient of variable \p var in
  constraint \p c.
*/
int
ppl_Constraint_coefficient __P((ppl_const_Constraint_t c,
				int var,
				ppl_Coefficient_t n));

/*!
  Copies into \p n the inhomogeneous term of constraint \p c.
*/
int
ppl_Constraint_inhomogeneous_term __P((ppl_const_Constraint_t c,
				       ppl_Coefficient_t n));

/*!
  Returns a positive integer if \p c is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p c is broken.  Useful for debugging purposes.
*/
int
ppl_Constraint_OK __P((ppl_const_Constraint_t c));


PPL_TYPE_DECLARATION(ConSys);

/*!
  Builds an empty system of constraints and writes an handle to it at
  address \p pcs.
*/
int
ppl_new_ConSys __P((ppl_ConSys_t* pcs));

/*!
  Builds a zero-dimensional, unsatisfiable constraint system and
  writes an handle to it at address \p pcs.
*/
int
ppl_new_ConSys_zero_dim_empty __P((ppl_ConSys_t* pcs));

/*!
  Builds the singleton constraint system containing only a copy of
  constraint \p c; writes an handle for the newly created system at
  address \p pcs.
*/
int
ppl_new_ConSys_from_Constraint __P((ppl_ConSys_t* pcs,
				    ppl_const_Constraint_t c));

/*!
  Builds a constraint system that is a copy of \p cs; writes an handle
  for the newly created system at address \p pcs.
*/
int
ppl_new_ConSys_from_ConSys __P((ppl_ConSys_t* pcs, ppl_const_ConSys_t cs));

/*!
  Invalidates the handle \p cs: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_ConSys __P((ppl_const_ConSys_t cs));

/*!
  Assigns a copy of the constraint system \p src to \p dst.
*/
int
ppl_assign_ConSys_from_ConSys __P((ppl_ConSys_t dst, ppl_const_ConSys_t src));

/*!
  Swaps the constraint systems \p x and \p y.
*/
int
ppl_swap_ConSys __P((ppl_ConSys_t x, ppl_ConSys_t y));

/*!
  Returns the dimension of the vector space enclosing \p *this.
*/
int
ppl_ConSys_space_dimension __P((ppl_const_ConSys_t cs));

/*!
  Inserts a copy of the constraint \p c into \p *this; the space
  dimension is increased, if necessary.
*/
int
ppl_ConSys_insert_Constraint __P((ppl_ConSys_t cs, ppl_const_Constraint_t c));

/*!
  Returns a positive integer if \p cs is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p cs is broken.  Useful for debugging purposes.
*/
int
ppl_ConSys_OK __P((ppl_const_ConSys_t c));


PPL_TYPE_DECLARATION(ConSys__const_iterator);

/*!
  Builds a new `const iterator' and writes an handle to it at address
  \p pcit.
*/
int
ppl_new_ConSys__const_iterator __P((ppl_ConSys__const_iterator_t* pcit));

/*!
  Builds a const iterator system that is a copy of \p cit; writes an
  handle for the newly created const iterator at address \p pcit.
*/
int
ppl_new_ConSys__const_iterator_from_ConSys__const_iterator
__P((ppl_ConSys__const_iterator_t* pcit,
     ppl_const_ConSys__const_iterator_t cit));

/*!
  Invalidates the handle \p cit: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_ConSys__const_iterator
__P((ppl_const_ConSys__const_iterator_t cit));

/*!
  Assigns a copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_ConSys__const_iterator_from_ConSys__const_iterator
__P((ppl_ConSys__const_iterator_t dst,
     ppl_const_ConSys__const_iterator_t src));

/*!
  Assigns to \p cit a const iterator "pointing" to the beginning of
  the constraint system \p cs.
*/
int
ppl_ConSys_begin __P((ppl_ConSys_t cs, ppl_ConSys__const_iterator_t cit));

/*!
  Assigns to \p cit a const iterator "pointing" past the end of the
  constraint system \p cs.
*/
int
ppl_ConSys_end __P((ppl_ConSys_t cs, ppl_ConSys__const_iterator_t* pcit));

/*!
  Dereference \p cit writing a const handle to the resulting
  constraint at address \p pc.
*/
int
ppl_ConSys__const_iterator_dereference
__P((ppl_const_ConSys__const_iterator_t cit,
     ppl_const_Constraint_t* pc));

/*!
  Increment \p cit so that it "points" to the next constraint.
*/
int
ppl_ConSys__const_iterator_increment __P((ppl_ConSys__const_iterator_t cit));

/*!
  Returns a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_ConSys__const_iterator_equal_test
__P((ppl_const_ConSys__const_iterator_t x,
     ppl_const_ConSys__const_iterator_t y));


PPL_TYPE_DECLARATION(Generator);

/*!
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


/*!
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

/*!
  Creates the point that is the origin of the zero-dimensional space
  \f$\Rset^0\f$.  Writes an handle for the new generator at address
  \p pg.
*/
int
ppl_new_Generator_zero_dim_point __P((ppl_Generator_t* pg));

/*!
  Creates, as a closure point, the point that is the origin of the
  zero-dimensional space \f$\Rset^0\f$.  Writes an handle for the new
  generator at address \p pg.
*/
int
ppl_new_Generator_zero_dim_closure_point __P((ppl_Generator_t* pg));

/*!
  Invalidates the handle \p g: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Generator __P((ppl_const_Generator_t g));

/*!
  Assigns a copy of the generator \p src to \p dst.
*/
int
ppl_assign_Generator_from_Generator __P((ppl_Generator_t dst,
					 ppl_const_Generator_t src));

/*!
  Swaps the generators \p x and \p y.
*/
int
ppl_swap_Generator __P((ppl_Generator_t x, ppl_Generator_t y));

/*!
  Returns the space dimension of \p g.
*/
int
ppl_Generator_space_dimension __P((ppl_const_Generator_t g));

/*!
  Returns the type of generator \p g.
*/
int
ppl_Generator_type __P((ppl_const_Generator_t g));

/*!
  Copies into \p n the coefficient of variable \p var in
  generator \p g.
*/
int
ppl_Generator_coefficient __P((ppl_const_Generator_t g,
			       int var,
			       ppl_Coefficient_t n));

/*!
  If \p g is a point or a closure point assigns its divisor to \p n.
*/
int
ppl_Generator_divisor __P((ppl_const_Generator_t g, ppl_Coefficient_t n));

/*!
  Returns a positive integer if \p g is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p g is broken.  Useful for debugging purposes.
*/
int
ppl_Generator_OK __P((ppl_const_Generator_t g));


PPL_TYPE_DECLARATION(GenSys);

/*!
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

/*!
  Builds the singleton generator system containing only a copy of
  generator \p g; writes an handle for the newly created system at
  address \p pgs.
*/
int
ppl_new_GenSys_from_Generator __P((ppl_GenSys_t* pgs,
				   ppl_const_Generator_t g));

/*!
  Builds a generator system that is a copy of \p gs; writes an handle
  for the newly created system at address \p pgs.
*/
int
ppl_new_GenSys_from_GenSys __P((ppl_GenSys_t* pgs, ppl_const_GenSys_t gs));

/*!
  Invalidates the handle \p gs: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_GenSys __P((ppl_const_GenSys_t gs));

/*!
  Assigns a copy of the generator system \p src to \p dst.
*/
int
ppl_assign_GenSys_from_GenSys __P((ppl_GenSys_t dst, ppl_const_GenSys_t src));

/*!
  Swaps the generator systems \p x and \p y.
*/
int
ppl_swap_GenSys __P((ppl_GenSys_t x, ppl_GenSys_t y));

/*!
  Returns the dimension of the vector space enclosing \p *this.
*/
int
ppl_GenSys_space_dimension __P((ppl_const_GenSys_t gs));

/*!
  Inserts a copy of the generator \p g into \p *this; the space
  dimension is increased, if necessary.
*/
int
ppl_GenSys_insert_Generator __P((ppl_GenSys_t gs, ppl_const_Generator_t g));

/*!
  Returns a positive integer if \p gs is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p gs is broken.  Useful for debugging purposes.
*/
int
ppl_GenSys_OK __P((ppl_const_GenSys_t c));


PPL_TYPE_DECLARATION(GenSys__const_iterator);

/*!
  Builds a new `const iterator' and writes an handle to it at address
  \p pgit.
*/
int
ppl_new_GenSys__const_iterator __P((ppl_GenSys__const_iterator_t* pgit));

/*!
  Builds a const iterator system that is a copy of \p git; writes an
  handle for the newly created const iterator at address \p pgit.
*/
int
ppl_new_GenSys__const_iterator_from_GenSys__const_iterator
__P((ppl_GenSys__const_iterator_t* pgit,
     ppl_const_GenSys__const_iterator_t git));

/*!
  Invalidates the handle \p git: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_GenSys__const_iterator
__P((ppl_const_GenSys__const_iterator_t git));

/*!
  Assigns a copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_GenSys__const_iterator_from_GenSys__const_iterator
__P((ppl_GenSys__const_iterator_t dst,
     ppl_const_GenSys__const_iterator_t src));

/*!
  Assigns to \p git a const iterator "pointing" to the beginning of
  the generator system \p gs.
*/
int
ppl_GenSys_begin __P((ppl_const_GenSys_t gs,
		      ppl_GenSys__const_iterator_t git));

/*!
  Assigns to \p git a const iterator "pointing" past the end of the
  generator system \p gs.
*/
int
ppl_GenSys_end __P((ppl_const_GenSys_t gs,
		    ppl_GenSys__const_iterator_t pgit));

/*!
  Dereference \p git writing a const handle to the resulting
  generator at address \p pg.
*/
int
ppl_GenSys__const_iterator_dereference
__P((ppl_const_GenSys__const_iterator_t git,
     ppl_const_Generator_t* pg));

/*!
  Increment \p git so that it "points" to the next generator.
*/
int
ppl_GenSys__const_iterator_increment __P((ppl_GenSys__const_iterator_t git));

/*!
  Return a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_GenSys__const_iterator_equal_test
__P((ppl_const_GenSys__const_iterator_t x,
     ppl_const_GenSys__const_iterator_t y));


PPL_TYPE_DECLARATION(Polyhedron);

/*!
  Builds an universe polyhedron of dimension \p d and writes an handle
  to it at address \p pph.
*/
int
ppl_new_Polyhedron_from_dimension __P((ppl_Polyhedron_t* pph, unsigned int d));

/*!
  Builds an empty polyhedron of dimension \p d and writes an handle
  to it at address \p pph.
*/
int
ppl_new_Polyhedron_empty_from_dimension __P((ppl_Polyhedron_t* pph,
					     unsigned int d));
/*!
  Builds a polyhedron that is a copy of \p ph; writes an handle
  for the newly created polyhedron at address \p pph.
*/
int
ppl_new_Polyhedron_from_Polyhedron __P((ppl_Polyhedron_t* pph,
					ppl_const_Polyhedron_t ph));

/*!
  Builds a new polyhedron recycling the system of constraints \p cs
  and writes an handle for the newly created polyhedron at address \p
  pph.  Since \p cs will be <EM>the</EM> system of constraints of the
  new polyhedron, the space dimension is also inherited.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_Polyhedron_from_ConSys __P((ppl_Polyhedron_t* pph, ppl_ConSys_t cs));

/*!
  Builds a new polyhedron recycling the system of generators \p gs
  and writes an handle for the newly created polyhedron at address \p
  pph.  Since \p cs will be <EM>the</EM> system of generators of the
  new polyhedron, the space dimension is also inherited.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_new_Polyhedron_from_GenSys __P((ppl_Polyhedron_t* pph, ppl_GenSys_t gs));

/*!
  Invalidates the handle \p ph: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Polyhedron __P((ppl_const_Polyhedron_t ph));

/*!
  Assigns a copy of the polyhedron \p src to \p dst.
*/
int
ppl_assign_Polyhedron_from_Polyhedron __P((ppl_Polyhedron_t dst,
					   ppl_const_Polyhedron_t src));

/*!
  Swaps the polyhedra \p x and \p y.
*/
int
ppl_swap_Polyhedron __P((ppl_Generator_t x, ppl_Generator_t y));

/*!
  Returns the dimension of the vector space enclosing \p ph.
*/
int
ppl_Polyhedron_space_dimension __P((ppl_const_Polyhedron_t ph));

/*!
  Intersects \p x with polyhedron \p y and assigns the result \p x.
*/
int
ppl_Polyhedron_intersection_assign __P((ppl_Polyhedron_t x,
					ppl_const_Polyhedron_t y));

/*!
  Intersects \p x with polyhedron \p y and assigns the result \p x.
  Upon successful return, \p x is also guaranteed to be minimized.
*/
int
ppl_Polyhedron_intersection_assign_and_minimize
__P((ppl_Polyhedron_t x, ppl_const_Polyhedron_t y));

/*!
  Assigns to \p x the convex hull of the set-theoretic union
  of \p x and \p y.
*/
int
ppl_Polyhedron_convex_hull_assign __P((ppl_Polyhedron_t x,
				       ppl_const_Polyhedron_t y));

/*!
  Assigns to \p x the convex hull of the set-theoretic union
  of \p x and \p y.
  Upon successful return, \p x is also guaranteed to be minimized.
*/
int
ppl_Polyhedron_convex_hull_assign_and_minimize __P((ppl_Polyhedron_t x,
						    ppl_const_Polyhedron_t y));
/*!
  Assigns to \p x the convex hull of the set-theoretic difference
  of \p x and \p y.
*/
int
ppl_Polyhedron_convex_difference_assign __P((ppl_Polyhedron_t x,
					     ppl_const_Polyhedron_t y));

/*!
  Assigns to \p x the convex hull of the set-theoretic difference
  of \p x and \p y.
  Upon successful return, \p x is also guaranteed to be minimized.
*/
int
ppl_Polyhedron_convex_difference_assign_and_minimize
__P((ppl_Polyhedron_t x, ppl_const_Polyhedron_t y));

/*!
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the widening of \p x and \p y.
*/
int
ppl_Polyhedron_widening_assign __P((ppl_Polyhedron_t x,
				    ppl_const_Polyhedron_t y));

/*!
  If the polyhedron \p y is contained in (or equal to) the polyhedron
  \p x, assigns to \p x the widening of \p x and \p y intersected with
  the constraint system \p cs.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_limited_widening_assign __P((ppl_Polyhedron_t x,
					    ppl_const_Polyhedron_t y,
					    ppl_ConSys_t cs));

/*!
  Writes a const handle to the constraint system defining the
  polyhedron \p ph at address \p pcs.
*/
int
ppl_Polyhedron_constraints __P((ppl_const_Polyhedron_t ph,
				ppl_const_ConSys_t* pcs));

/*!
  Writes a const handle to the generator system defining the
  polyhedron \p ph at address \p pgs.
*/
int
ppl_Polyhedron_generators __P((ppl_const_Polyhedron_t ph,
			       ppl_const_GenSys_t* pgs));

/*!
  Adds a copy of the constraint \p c to the system of constraints of
  \p ph.
*/
int
ppl_Polyhedron_add_constraint __P((ppl_Polyhedron_t ph,
				   ppl_const_Constraint_t c));

/*!
  Adds a copy of the generator \p g to the system of generatorss of
  \p ph.
*/
int
ppl_Polyhedron_add_generator __P((ppl_Polyhedron_t ph,
				  ppl_const_Generator_t g));

/*!
  Adds the system of constraints \p cs to the system of constraints of
  \p ph.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_constraints __P((ppl_Polyhedron_t ph, ppl_ConSys_t cs));

/*!
  Adds the system of constraints \p cs to the system of constraints of
  \p ph.  Return a positive integer if the resulting polyhedron is
  non-empty; return 0 if they are different.  Upon successful return,
  \p ph is guaranteed to be minimized.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_constraints_and_minimize __P((ppl_Polyhedron_t ph,
						 ppl_ConSys_t cs));

/*!
  Adds the system of generators \p gs to the system of generators of
  \p ph.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_generators __P((ppl_Polyhedron_t ph, ppl_GenSys_t gs));

/*!
  Adds the system of generators \p gs to the system of generators of
  \p ph.  Upon successful return, \p ph is guaranteed to be minimized.

  \warning
  This function modifies the generator system referenced by \p gs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_generators_and_minimize __P((ppl_Polyhedron_t ph,
						ppl_GenSys_t gs));

/*!
  Adds \p d new dimensions to the space enclosing the polyhedron \p ph
  and to \p ph itself.
*/
int
ppl_Polyhedron_add_dimensions_and_embed __P((ppl_Polyhedron_t ph,
					     unsigned int d));

/*!
  Adds \p d new dimensions to the space enclosing the polyhedron \p ph.
*/
int
ppl_Polyhedron_add_dimensions_and_project __P((ppl_Polyhedron_t ph,
					       unsigned int d));

/*!
  Removes from \p ph and its containing space the dimensions that are
  specified in first \p n positions of the array \p ds.  The presence
  of duplicates in \p ds is innocuous.
*/
int
ppl_Polyhedron_remove_dimensions __P((ppl_Polyhedron_t ph,
				      unsigned int ds[],
				      unsigned int n));

/*!
  Removes the higher dimensions from \p ph and its enclosing space so
  that, upon successful return, the new space dimension is \p d.
*/
int
ppl_Polyhedron_remove_higher_dimensions __P((ppl_Polyhedron_t ph,
					     unsigned int d));

/*!
  First increases the space dimension of \p ph by adding as many
  dimensions as is the space dimension of \p cs; then adds to the
  system of constraints of \p ph a renamed-apart version of the
  constraints in \p cs.

  \warning
  This function modifies the constraint system referenced by \p cs:
  upon return, no assumption can be made on its value.
*/
int
ppl_Polyhedron_add_dimensions_and_constraints __P((ppl_Polyhedron_t ph,
						   ppl_ConSys_t cs));

/*!
  Transforms the polyhedron \p ph, assigning an affine expression
  to the specified variable.
  \param ph  The polyhedron that is transformed.
  \param var The variable to which the affine expression is assigned.
  \param le  The numerator of the affine expression.
  \param d   The denominator of the affine expression.
*/
int
ppl_Polyhedron_affine_image __P((ppl_Polyhedron_t ph,
				 unsigned int var,
				 ppl_const_LinExpression_t le,
				 ppl_const_Coefficient_t d));

/*!
  Transforms the polyhedron \p ph, substituting an affine expression
  to the specified variable.
  \param ph  The polyhedron that is transformed.
  \param var The variable to which the affine expression is substituted.
  \param le  The numerator of the affine expression.
  \param d   The denominator of the affine expression.
*/
int
ppl_Polyhedron_affine_preimage __P((ppl_Polyhedron_t ph,
				    unsigned int var,
				    ppl_const_LinExpression_t le,
				    ppl_const_Coefficient_t d));

/*!
  Use \p ph to shrink a generic, interval-based bounding box.
  The bounding box is abstractly provided by means of the parameters,

  \param ph
  The polyhedron that is used to shrink the bounding box.

  \param raise_lower_bound
  a pointer to a void function with arguments
  <CODE>(unsigned int k, int closed,
         ppl_const_Coefficient_t n, ppl_const_Coefficient_t d)</CODE>
  that intersects the interval corresponding to the <CODE>k</CODE>-th
  dimension with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is non-zero,
  with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is zero.

  \param lower_upper_bound
  a pointer to a void function with argument
  <CODE>(unsigned int k, int closed,
         ppl_const_Coefficient_t n, ppl_const_Coefficient_t d)</CODE>
  that intersects the interval corresponding to the <CODE>k</CODE>-th
  dimension with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is non-zero,
  with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE> is zero.

  \param set_empty
  a pointer to a void function with arguments <CODE>(unsigned int k)</CODE>
  that intersects the interval corresponding to the <CODE>k</CODE>-th
  dimension with \f$\emptyset\f$.
*/
int
ppl_Polyhedron_shrink_bounding_box
__P((ppl_const_Polyhedron_t ph,
     void (*raise_lower_bound)(unsigned int k, int closed,
			       ppl_const_Coefficient_t n,
			       ppl_const_Coefficient_t d),
     void (*lower_upper_bound)(unsigned int k, int closed,
			       ppl_const_Coefficient_t n,
			       ppl_const_Coefficient_t d),
     void (*set_empty)(unsigned int k)));

/*!
  Individual bit saying that the polyhedron and the set of points
  satisfying the constraint are disjoint.
*/
extern unsigned int PPL_POLY_CON_RELATION_IS_DISJOINT;

/*!
  Individual bit saying that the polyhedron intersects the set of
  points satisfying the constraint, but it is not included in it.
*/
extern unsigned int PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS;

/*!
  Individual bit saying that the polyhedron is included in the set of
  points satisfying the constraint.
*/
extern unsigned int PPL_POLY_CON_RELATION_IS_INCLUDED;

/*!
  Individual bit saying that the polyhedron is included in the set of
  points saturating the constraint.
*/
extern unsigned int PPL_POLY_CON_RELATION_SATURATES;

/*!
  Checks the relation between the polyhedron \p ph with the constraint
  \p c.  If successful, returns a non-negative integer that is
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

/*!
  Individual bit saying that adding the generator would not change the
  polyhedron.
*/
extern unsigned int PPL_POLY_GEN_RELATION_SUBSUMES;

/*!
  Checks the relation between the polyhedron \p ph with the generator
  \p g.  If successful, returns a non-negative integer that is
  obtained as the bitwise or of the bits (only
  PPL_POLY_GEN_RELATION_SUBSUMES, at present) that describe the
  relation between \p ph and \p g.
*/
int
ppl_Polyhedron_relation_with_Generator __P((ppl_const_Polyhedron_t ph,
					    ppl_const_Generator_t g));

/*!
  Returns a positive integer if \p ph is empty; returns 0 if \p ph is
  not empty.
*/
int
ppl_Polyhedron_check_empty __P((ppl_const_Polyhedron_t ph));

/*!
  Returns a positive integer if \p ph is a universe polyhedron;
  returns 0 if it is not.
*/
int
ppl_Polyhedron_check_universe __P((ppl_const_Polyhedron_t ph));

/*!
  Returns a positive integer if \p ph is bounded; returns 0 if \p ph is
  unbounded.
*/
int
ppl_Polyhedron_is_bounded __P((ppl_const_Polyhedron_t ph));

/*!
  Returns a positive integer if \p x contains or is equal to \p y;
  returns 0 if it does not.
*/
int
ppl_Polyhedron_contains_Polyhedron __P((ppl_const_Polyhedron_t x,
					ppl_const_Polyhedron_t y));

/*!
  Returns a positive integer if \p x strictly contains \p y; returns 0
  if it does not.
*/
int
ppl_Polyhedron_strictly_contains_Polyhedron __P((ppl_const_Polyhedron_t x,
						 ppl_const_Polyhedron_t y));

/*!
  Returns a positive integer if \p ph is well formed, i.e., if it
  satisfies all its implementation variant; returns 0 and make some
  noise if \p ph is broken.  Useful for debugging purposes.
*/
int
ppl_Polyhedron_OK __P((ppl_const_Polyhedron_t ph));


__END_DECLS
#undef __P

#endif /* !_ppl_c_h */
