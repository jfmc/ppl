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

#define PPL_TYPE_DECLARATION(Type) \
/*! Opaque pointer to Type. */ \
typedef struct ppl_ ## Type ## _tag* ppl_ ## Type ## _t; \
/*! Opaque pointer to const Type. */ \
typedef struct ppl_ ## Type ## _tag const* ppl_const_ ## Type ## _t


PPL_TYPE_DECLARATION(Coefficient);

/*!
  Creates a new coefficent with the value given by the GMP integer
  \p z and writes an handle for the newly created coefficient
  at address \p pc.
*/
int
ppl_new_Coefficient_from_mpz_t __P((ppl_Coefficient_t* pc, mpz_t z));

/*!
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Coefficient __P((ppl_const_Coefficient_t c));


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
  Invalidates the handle \p le: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_LinExpression __P((ppl_const_LinExpression_t le));

/*!
  Assigns an exact copy of the linear expression \p src to \p dst.
*/
int
ppl_assign_LinExpresson_from_LinExpression
__P((ppl_LinExpression_t dst,
     ppl_const_LinExpression_t src));

/*!
  Swaps the linear expressions \p a and \p b.
*/
int
ppl_swap_LinExpresson __P((ppl_LinExpression_t a, ppl_LinExpression_t b));

/*!
  Adds \p value to the coefficient of variable \p var in the linear
  expression \p le.  The space dimension is set to be the maximum
  between \p var + 1 and the old space dimension.
*/
int
ppl_LinExpression_add_to_coefficient __P((ppl_LinExpression_t le,
					  unsigned int var,
					  ppl_const_Coefficient_t value));

/*!
  Adds \p value to the inhomogeneous term of the linear expression
  \p le.
*/
int
ppl_LinExpression_add_to_inhomogeneous __P((ppl_LinExpression_t le,
					    ppl_const_Coefficient_t value));

/*!
  Returns the space dimension of \p le.
*/
int
ppl_LinExpression_space_dimension __P((ppl_const_LinExpression_t le));


PPL_TYPE_DECLARATION(Constraint);

/*!
  Describes the relations represented by a constraint.
*/
enum ppl_enum_Constraint_Type {
  /*! \hideinitializer
    The constraint is of the form \f$e = 0\f$. */
  PPL_CONSTRAINT_TYPE_EQUAL,
  /*! \hideinitializer
    The constraint is of the form \f$e \geq 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL,
  /*! \hideinitializer
    The constraint is of the form \f$e < 0\f$. */
  PPL_CONSTRAINT_TYPE_GREATER_THAN,
  /*! \hideinitializer
    The constraint is of the form \f$e \leq 0\f$. */
  PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL,
  /*! \hideinitializer
    The constraint is of the form \f$e < 0\f$. */
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
  Invalidates the handle \p c: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Constraint __P((ppl_const_Constraint_t c));

/*!
  Assigns an exact copy of the constraint \p src to \p dst.
*/
int
ppl_assign_Constraint_from_Constraint __P((ppl_Constraint_t dst,
					   ppl_const_Constraint_t src));

/*!
  Swaps the constraints \p a and \p b.
*/
int
ppl_swap_Constraint __P((ppl_Constraint_t a, ppl_Constraint_t b));

/*!
  Returns the space dimension of \p c.
*/
int
ppl_Constraint_space_dimension __P((ppl_const_Constraint_t c));

/*!
  Copies into \p value the coefficient of variable \p var in
  constraint \p c.
*/
int
ppl_Constraint_coefficient __P((ppl_const_Constraint_t c,
				int var,
				ppl_Coefficient_t value));

/*!
  Copies into \p value the inhomogeneous term of constraint \p c.
*/
int
ppl_Constraint_inhomogeneous_term __P((ppl_const_Constraint_t c,
				       ppl_Coefficient_t value));


PPL_TYPE_DECLARATION(ConSys);

/*!
  Builds an empty system of constraints and writes an handle to it at
  address \p pcs.
*/
int
ppl_new_ConSys __P((ppl_ConSys_t* pcs));

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
  Assigns an exact copy of the constraint system \p src to \p dst.
*/
int
ppl_assign_ConSys_from_ConSys __P((ppl_ConSys_t dst, ppl_const_ConSys_t src));

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


PPL_TYPE_DECLARATION(ConSys_const_iterator);

/*!
  Builds a new `const iterator' and writes an handle to it at address
  \p pcit.
*/
int
ppl_new_ConSys_const_iterator __P((ppl_ConSys_const_iterator_t* pcit));

/*!
  Builds a const iterator system that is a copy of \p cit; writes an
  handle for the newly created const iterator at address \p pcit.
*/
int
ppl_new_ConSys_const_iterator_from_ConSys_const_iterator
__P((ppl_ConSys_const_iterator_t* pcit,
     ppl_const_ConSys_const_iterator_t cit));

/*!
  Invalidates the handle \p cit: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_ConSys_const_iterator __P((ppl_const_ConSys_const_iterator_t cit));

/*!
  Assigns an exact copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_ConSys_const_iterator_from_ConSys_const_iterator
__P((ppl_ConSys_const_iterator_t dst, ppl_const_ConSys_const_iterator_t src));

/*!
  Assigns to \p cit a const iterator "pointing" to the beginning of
  the constraint system \p cs.
*/
int
ppl_ConSys_begin __P((ppl_ConSys_t cs, ppl_ConSys_const_iterator_t cit));

/*!
  Assigns to \p cit a const iterator "pointing" past the end of the
  constraint system \p cs.
*/
int
ppl_ConSys_end __P((ppl_ConSys_t cs, ppl_ConSys_const_iterator_t* pcit));

/*!
  Dereference \p cit writing a const handle to the resulting
  constraint at address \p pc.
*/
int
ppl_ConSys_const_iterator_dereference
__P((ppl_const_ConSys_const_iterator_t cit,
     ppl_const_Constraint_t* pc));

/*!
  Increment \p cit so that it "points" to the next constraint.
*/
int
ppl_ConSys_const_iterator_increment __P((ppl_ConSys_const_iterator_t cit));

/*!
  Return a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_ConSys_const_iterator_equal_test
__P((ppl_const_ConSys_const_iterator_t x,
     ppl_const_ConSys_const_iterator_t y));


PPL_TYPE_DECLARATION(Generator);

/*!
  Describes the relations represented by a generator.
*/
enum ppl_enum_Generator_Type {
  /*! \hideinitializer
    The generator is of the form \f$e = 0\f$. */
  PPL_GENERATOR_TYPE_POINT,
  /*! \hideinitializer
    The generator is of the form \f$e \geq 0\f$. */
  PPL_GENERATOR_TYPE_CLOSURE_POINT,
  /*! \hideinitializer
    The generator is of the form \f$e < 0\f$. */
  PPL_GENERATOR_TYPE_RAY,
  /*! \hideinitializer
    The generator is of the form \f$e \leq 0\f$. */
  PPL_GENERATOR_TYPE_LINE
};


/*!
  Creates the new generator `\p le \p rel 0' and writes an handle for
  it at address \p pg.  The space dimension of the new generator is
  equal to the space dimension of \p le.
*/
int
ppl_new_Generator __P((ppl_Generator_t* pg,
		       ppl_const_LinExpression_t le,
		       enum ppl_enum_Generator_Type));

/*!
  Invalidates the handle \p g: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_Generator __P((ppl_const_Generator_t g));

/*!
  Assigns an exact copy of the generator \p src to \p dst.
*/
int
ppl_assign_Generator_from_Generator __P((ppl_Generator_t dst,
					 ppl_const_Generator_t src));

/*!
  Swaps the generators \p a and \p b.
*/
int
ppl_swap_Generator __P((ppl_Generator_t a, ppl_Generator_t b));

/*!
  Returns the space dimension of \p g.
*/
int
ppl_Generator_space_dimension __P((ppl_const_Generator_t g));

/*!
  Copies into \p value the coefficient of variable \p var in
  generator \p g.
*/
int
ppl_Generator_coefficient __P((ppl_const_Generator_t g,
			       int var,
			       ppl_Coefficient_t value));

/*!
  If \p g is a point or a closure point assigns its divisor to \p value.
*/
int
ppl_Generator_divisor __P((ppl_const_Generator_t g,
			   ppl_Coefficient_t value));


PPL_TYPE_DECLARATION(GenSys);

/*!
  Builds an empty system of generators and writes an handle to it at
  address \p pgs.
*/
int
ppl_new_GenSys __P((ppl_GenSys_t* pgs));

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
  Assigns an exact copy of the generator system \p src to \p dst.
*/
int
ppl_assign_GenSys_from_GenSys __P((ppl_GenSys_t dst, ppl_const_GenSys_t src));

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


PPL_TYPE_DECLARATION(GenSys_const_iterator);

/*!
  Builds a new `const iterator' and writes an handle to it at address
  \p pgit.
*/
int
ppl_new_GenSys_const_iterator __P((ppl_GenSys_const_iterator_t* pgit));

/*!
  Builds a const iterator system that is a copy of \p git; writes an
  handle for the newly created const iterator at address \p pgit.
*/
int
ppl_new_GenSys_const_iterator_from_GenSys_const_iterator
__P((ppl_GenSys_const_iterator_t* pgit,
     ppl_const_GenSys_const_iterator_t git));

/*!
  Invalidates the handle \p git: this makes sure the corresponding
  resources will eventually be released.
*/
int
ppl_delete_GenSys_const_iterator __P((ppl_const_GenSys_const_iterator_t git));

/*!
  Assigns an exact copy of the const iterator \p src to \p dst.
*/
int
ppl_assign_GenSys_const_iterator_from_GenSys_const_iterator
__P((ppl_GenSys_const_iterator_t dst, ppl_const_GenSys_const_iterator_t src));

/*!
  Assigns to \p git a const iterator "pointing" to the beginning of
  the generator system \p gs.
*/
int
ppl_GenSys_begin __P((ppl_GenSys_t gs, ppl_GenSys_const_iterator_t git));

/*!
  Assigns to \p git a const iterator "pointing" past the end of the
  generator system \p gs.
*/
int
ppl_GenSys_end __P((ppl_GenSys_t gs, ppl_GenSys_const_iterator_t* pgit));

/*!
  Dereference \p git writing a const handle to the resulting
  generator at address \p pg.
*/
int
ppl_GenSys_const_iterator_dereference
__P((ppl_const_GenSys_const_iterator_t git,
     ppl_const_Generator_t* pg));

/*!
  Increment \p git so that it "points" to the next generator.
*/
int
ppl_GenSys_const_iterator_increment __P((ppl_GenSys_const_iterator_t git));

/*!
  Return a positive integer if the iterators corresponding to \p x and
  \p y are equal; return 0 if they are different.
*/
int
ppl_GenSys_const_iterator_equal_test
__P((ppl_const_GenSys_const_iterator_t x,
     ppl_const_GenSys_const_iterator_t y));


#if 0
/*! Returns the singleton system containing only
    Constraint::zero_dim_false().  */
static const ConSys& zero_dim_empty();
#endif

typedef struct ppl_GenSys_tag *ppl_GenSys_t;

typedef struct ppl_Polyhedron_tag *ppl_Polyhedron_t;

int
ppl_new_Polyhedron_from_dimension __P((ppl_Polyhedron_t* a, unsigned int d));

int
ppl_new_Polyhedron_empty_from_dimension __P((ppl_Polyhedron_t* a,
					     unsigned int d));

int
ppl_new_Polyhedron_from_Polyhedron __P((ppl_Polyhedron_t* a,
					ppl_Polyhedron_t ph));

int
ppl_new_Polyhedron_from_ConSys __P((ppl_Polyhedron_t* a, ppl_ConSys_t cs));

int
ppl_new_Polyhedron_from_GenSys __P((ppl_Polyhedron_t* a, ppl_GenSys_t gs));

int
ppl_delete_Polyhedron __P((ppl_Polyhedron_t p));

int
ppl_Polyhedron_space_dimension __P((ppl_Polyhedron_t p));

__END_DECLS
#undef __P

#endif /* !_ppl_c_h */
