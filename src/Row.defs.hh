/* Row class declaration.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Row_defs_hh
#define PPL_Row_defs_hh 1

#include "Row.types.hh"
#include "globals.hh"
#include "Topology.hh"
#include "Integer.types.hh"
#include "LinExpression.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include <cstddef>

#ifndef EXTRA_ROW_DEBUG
// When EXTRA_ROW_DEBUG evaluates to <CODE>true</CODE>, each row
// carries its own capacity; this enables extra consistency checks to
// be performed.
#define EXTRA_ROW_DEBUG 0
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
The class Row allows us to build objects like these:

    - \f$[b, a_0, \ldots, a_{d-1}]_=\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_=\f$
      represents the line of direction
      \f$\sum_{i=0}^{d-1} a_i x_i\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq\f$
      represents the inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_\geq\f$
      represents the ray of direction
      \f$\sum_{i=0}^{d-1} a_i x_i\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq\f$, with \f$b \neq 0\f$,
      represents the point
      \f$\sum_{i=0}^{d-1} \frac{a_i}{b} x_i\f$.

  So, a row can be both a constraint and a generator: it can be an
  equality, an inequality, a line, a ray or a point.

  A point must have the inhomogeneous term positive, a line
  and a ray must have the inhomogeneous term equal to zero.
  If needed, the coefficients of a point are negated at creation
  time so that it has a positive inhomogeneous term. 
  This invariant is maintained because, when combining a point
  with another generator, we only consider positive combinations.
  
  The inhomogeneous term of a constraint can be zero or different from zero.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Row {
public:
  //! Returns the inhomogeneous term.
  const Integer& inhomogeneous_term() const;

  //! Returns the coefficient \f$a_n\f$.
  const Integer& coefficient(dimension_type n) const;

  enum Kind {
    LINE_OR_EQUALITY = 0,
    RAY_OR_POINT_OR_INEQUALITY = 1
  };

  //! The type of the object to which the coefficients refer to.
  class Type;

  //! Tight constructor: resizing will require reallocation.
  Row(Type t, dimension_type sz);

  //! Sizing constructor with type.
  Row(Type t, dimension_type sz, dimension_type capacity);

  //! @name Post-constructors.
  //@{
  //! Constructs properly a default-constructed element.
  void construct(Type t, dimension_type sz);
  //! Constructs properly a default-constructed element.
  void construct(Type t, dimension_type sz, dimension_type capacity);
  //@}

  //! Pre-constructs a row: construction must be completed by construct().
  Row();

  //! Ordinary copy constructor.
  Row(const Row& y);

  //! Copy constructor with specified capacity.
  Row(const Row& y, dimension_type capacity);

  //! Copy constructor with specified size and capacity.
  Row(const Row& y, dimension_type sz, dimension_type capacity);

  //! Destructor.
  ~Row();

  //! Assignment operator.
  Row& operator=(const Row& y);

  //! Swaps \p *this with \p y.
  void swap(Row& y);

  //! Assigns the implementation of \p y to \p *this.
  void assign(Row& y);

  //! Resizes the row without copying the old contents.
  void resize_no_copy(dimension_type new_size);

  //! Grows the row without copying the old contents.
  /*!
    Adds new positions to the implementation of the row
    obtaining a new row with size \p new_size.
  */
  void grow_no_copy(dimension_type new_size);

  //! Shrinks the row by erasing elements at the end.
  /*!
    Destroys elements of the row implementation
    from position \p new_size to the end.
  */
  void shrink(dimension_type new_size);

  //! @name Subscript operators.
  //@{
  Integer& operator[](dimension_type k);
  const Integer& operator[](dimension_type k) const;
  //@}

  //! @name Type inspection methods.
  //@{
  Type type() const;
  Topology topology() const;
  bool is_line_or_equality() const;
  bool is_ray_or_point_or_inequality() const;
  bool is_necessarily_closed() const;
  //@}

  //! @name Type coercion methods.
  //@{
  void set_is_line_or_equality();
  void set_is_ray_or_point_or_inequality();
  void set_necessarily_closed();
  void set_not_necessarily_closed();
  //@}

  //! Gives the number of coefficients currently in use.
  dimension_type size() const;

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Normalizes all the coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! \brief
  //! Strong normalization: ensures that different rows represent
  //! different hyperplanes or hyperspaces.
  /*!
    In addition to the normalization performed by Row::normalize(), this
    method ensures that the first non-zero coefficient of lines and
    equalities is negative.
  */
  void strong_normalize();

  //! Linearly combines \p *this with \p y so that \p *this[k] is 0.
  /*!
    \param y   The row that will be combined with \p *this object.
    \param k   The position of \p *this that have to be \f$0\f$.

    Computes a linear combination between \p *this and \p y such
    that the k-th element of \p *this become \f$0\f$. Then it assigns the
    resulting row to \p *this and normalizes it.
  */
  void linear_combine(const Row& y, dimension_type k);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if all the homogeneous
  //! terms of \p *this are zero.
  bool all_homogeneous_terms_are_zero() const;

  //! Checks if all the invariants are satisfied.
  bool OK(dimension_type row_size, dimension_type row_capacity) const;

private:
  friend class Parma_Polyhedra_Library::LinExpression;
  friend class Parma_Polyhedra_Library::Constraint;
  friend class Parma_Polyhedra_Library::Generator;

  class Impl;

  //! The real implementation, as far as memory allocation is concerned.
  Impl* impl;

#if EXTRA_ROW_DEBUG

  //! The capacity of the row (only available during debugging).
  dimension_type capacity_;

  //! Returns the capacity of the row (only available during debugging).
  dimension_type capacity() const;

#endif // defined(EXTRA_ROW_DEBUG)
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Computes the scalar product between \p x and \p y.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
const Integer& operator*(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e reduced scalar product between \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
const Integer& reduced_scalar_product(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The basic comparison function.
/*! \relates Row

  \param x    A row of coefficients.
  \param y    Another row.

  \return     The returned absolute value can be \f$0, 1\f$ or \f$2\f$.

  Compares \p x and \p y, where \p x and \p y may be of different size,
  in which case the "missing" coefficients are assumed to be zero.
  The comparison is such that:
  -# equalities are smaller than inequalities;
  -# lines are smaller than points and rays;
  -# the ordering is lexicographic;
  -# the positions compared are, in decreasing order of significance,
     1, 2, ..., \p size(), 0;
  -# the result is negative, zero, or positive if x is smaller than,
     equal to, or greater than y, respectively;
  -# when \p x and \p y are different, the absolute value of the
     result is 1 if the difference is due to the coefficient in
     position 0; it is 2 otherwise.

  When \p x and \p y represent the hyper-planes associated
  to two equality or inequality constraints, the coefficient
  at 0 is the known term.
  In this case, the return value can be characterized as follows:
  - -2, if \p x is smaller than \p y and they are \e not parallel;
  - -1, if \p x is smaller than \p y and they \e are parallel;
  -  0, if \p x and y are equal;
  - +1, if \p y is smaller than \p x and they \e are parallel;
  - +2, if \p y is smaller than \p x and they are \e not parallel.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int compare(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! @name Classical comparison operators.
//@{
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \relates Row */
bool operator==(const Row& x, const Row& y);
/*! \relates Row */
bool operator!=(const Row& x, const Row& y);
/*! \relates Row */
bool operator<=(const Row& x, const Row& y);
/*! \relates Row */
bool operator <(const Row& x, const Row& y);
/*! \relates Row */
bool operator>=(const Row& x, const Row& y);
/*! \relates Row */
bool operator >(const Row& x, const Row& y);
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//@}
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The type of a Row object.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Row::Type {
public:
  //! The default Row::Type is a line/equality of a closed polyhedron.
  Type();

  //! \brief
  //! Builds the Row type by providing the two needed
  //! pieces of information.
  Type(Topology topol, Kind kind);

  //! @name Testing and setting the type.
  //@{
  Topology topology() const;
  bool is_line_or_equality() const;
  bool is_ray_or_point_or_inequality() const;
  bool is_necessarily_closed() const;

  void set_is_line_or_equality();
  void set_is_ray_or_point_or_inequality();
  void set_necessarily_closed();
  void set_not_necessarily_closed();
  //@}

private:
  //! Type is implemented by means of a finite bitset.
  typedef unsigned int flags_t;

  //! Builds the type from a bitmask.
  Type(flags_t mask);

  //! This holds the current bitset.
  flags_t flags;

  //! @name The bits that are currently in use.
  //@{
  static const flags_t NNC = NOT_NECESSARILY_CLOSED << 0;
  static const flags_t RPI = RAY_OR_POINT_OR_INEQUALITY << 1;
  //@}

  //! Check whether <EM>all</EM> bits in \p mask are set.
  bool test_all(flags_t mask) const;

  //! Set the bits in \p mask.
  void set(flags_t mask);

  //! Reset the bits in \p mask.
  void reset(flags_t mask);
};


class Parma_Polyhedra_Library::Row::Impl {
public:
  //! @name Custom allocator and deallocator.
  //@{
  void* operator new(size_t fixed_size, dimension_type capacity);
  void operator delete(void* p, dimension_type capacity);
  void operator delete(void* p);
  //@}

  //! Sizing constructor.
  Impl(Type t, dimension_type sz);

  //! Copy constructor.
  Impl(const Impl& y);

  //! Copy constructor with specified size.
  Impl(const Impl& y, dimension_type sz);

  //! Destructor.
  ~Impl();

  //! Resizes without copying the old contents.
  void resize_no_copy(dimension_type new_size);

  //! Grows without copying the old contents.
  void grow_no_copy(dimension_type new_size);

  //! Shrinks by erasing elements at the end.
  void shrink(dimension_type new_size);

  //! @name Subscript operators.
  //@{
  Integer& operator[](dimension_type k);
  const Integer& operator[](dimension_type k) const;
  //@}

  //! @name Size accessors.
  //@{
  dimension_type size() const;
  void set_size(dimension_type new_sz);
  void bump_size();
  //@}

private:
  //! The number of coefficients in the row.
  dimension_type size_;

public:
  // FIXME: this should become private.
  //! The type of this row.
  Type type;

private:
  //! The vector of coefficients.
  Integer vec_[
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       1
#endif
  ];

private:
  //! Private and unimplemented: default construction is not allowed.
  Impl();

  //! Private and unimplemented: assignment is not allowed.
  Impl& operator=(const Impl&);

  //! Exception-safe copy construction mechanism.
  void copy_construct(const Impl& y);
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Row& x,
	  Parma_Polyhedra_Library::Row& y);

} // namespace std

#ifndef EXTRA_NORMALIZATION
// If non-zero, lines and equalities are ALWAYS normalized so that the
// first non-zero coefficient is negative.
#define EXTRA_NORMALIZATION 0
#endif

#include "Row.inlines.hh"

#endif // !defined(PPL_Row_defs_hh)
