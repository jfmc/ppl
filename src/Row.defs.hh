/* Row class declaration.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Row_defs_hh
#define _Row_defs_hh 1

#include "Row.types.hh"
#if 0
#include "Integer.defs.hh"
#else
#include "Coefficient.types.hh"
#endif
#include <cstddef>
#include <iosfwd>

namespace Parma_Polyhedra_Library {
  // Put it in the namespace here to declare it friend later.
  std::ostream& operator <<(std::ostream& s, const Row& row);
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
The class Row allows us to build objects like these:

    - \f$[b, a_0, \ldots, a_{d-1}]_=\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_=\f$.
      represents the line of direction
      \f$\sum_{i=0}^{d-1} a_i \vec{x}_i\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq\f$
      represents the inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_\geq\f$
      represents the ray of direction
      \f$\sum_{i=0}^{d-1} a_i \vec{x}_i\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq\f$, with \f$b \neq 0\f$,
      represents the vertex
      \f$\sum_{i=0}^{d-1} \frac{a_i}{b} \vec{x}_i\f$.

  So, a row can be both a constraint and a generator: it can be an
  equality, an inequality, a line, a ray or a vertex.

  A vertex must have the inhomogeneous term different from zero, a line
  and a ray must have the inhomogeneous term equal to zero.
  The inhomogeneous term of a constraint can be zero or different from zero.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Row {

public:
  int first() const;
  int last() const;
  int next(int prev) const;
  int prev(int next) const;
  const Integer& coefficient() const;
  const Integer& coefficient(size_t n) const;

public:
  //! The type of the object to which the coefficients refer to.
  enum Type {
    LINE_OR_EQUALITY = 0,
    RAY_OR_VERTEX_OR_INEQUALITY = 1
  };

  //! Tight constructor: resizing will require reallocation.
  Row(Type type, size_t size);
  //! Sizing constructor with type.
  Row(Type type, size_t size, size_t capacity);

  //! Post-constructors: to construct properly default-constructed elements.
  //@{
  void construct(Type type, size_t size);
  void construct(Type type, size_t size, size_t capacity);
  //@}

  //! Pre-constructs a row: construction must be completed by construct().
  Row();

  //! Ordinary copy constructor.
  Row(const Row& y);

  //! Copy constructor with specified capacity.
  Row(const Row& y, size_t capacity);
  //! Copy constructor with specified size and capacity.
  Row(const Row& y, size_t size, size_t capacity);

  //! Destructor.
  ~Row();

  //! Assignment operator.
  Row& operator =(const Row& row);

  //! Swaps \p *this with \p y.
  void swap(Row& y);

  //! Assigns the implementation of \p y to \p *this.
  void assign(Row& y);

  //! Resizes the row without copying the old contents.
  void resize_no_copy(size_t new_size);
  //! Grows the row without copying the old contents.
  void grow_no_copy(size_t new_size);
  //! Shrinks the row by erasing elements at the end.
  void shrink(size_t new_size);

  //! @name Subscript operators.
  //@{
  Integer& operator [](size_t k);
  const Integer& operator [](size_t k) const;
  //@}

  //! @name Type inspection methods.
  //@{
  bool is_line_or_equality() const;
  bool is_ray_or_vertex_or_inequality() const;
  Type type() const;
  //@}

  //! @name Type coercion methods.
  //@{
  void set_is_line_or_equality();
  void set_is_ray_or_vertex_or_inequality();
  //@}

public:
  //! Gives the number of coefficients currently in use.
  size_t size() const;

  //! Normalizes all the coefficients so that they are mutually prime.
  void normalize();
  //! Strong normalization: ensures that different rows represent
  //! different hyperplanes or hyperspaces.
  void strong_normalize();

  //! Linearly combines \p *this with \p y such that \p *this[k] is 0.
  void linear_combine(const Row& y, size_t k);

  //! Returns <CODE>true</CODE> if and only if all the homogeneous
  //! terms of \p *this are zero.
  bool all_homogeneous_terms_are_zero() const;

  //! Output operator.
  friend std::ostream&
  Parma_Polyhedra_Library::operator <<(std::ostream& s, const Row& row);

  //! Checks if all the invariants are satisfied.
  bool OK(size_t row_size, size_t row_capacity) const;

private:
  class Impl;

  //! The real implementation, as far as memory allocation is concerned.
  Impl* impl;

#ifndef NDEBUG
  // For debugging only: the capacity of the row.
  size_t capacity_;

PPL_INTERNAL:
  // Its accessor.
  size_t capacity() const;
#endif
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Computes the scalar product between \p x and \p y.
  /*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  Integer operator *(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! The basic comparison function.
  /*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  int compare(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! @name Classical comparison operators.
  /*! \relates Row */
  //@{
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool operator ==(const Row& x, const Row& y);
  bool operator !=(const Row& x, const Row& y);
  bool operator <=(const Row& x, const Row& y);
  bool operator  <(const Row& x, const Row& y);
  bool operator >=(const Row& x, const Row& y);
  bool operator  >(const Row& x, const Row& y);
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //@}
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

} // namespace Parma_Polyhedra_Library


class Parma_Polyhedra_Library::Row::Impl {
public:
  //! @name Custom allocator and deallocator.
  //@{
  void* operator new(size_t fixed_size, size_t capacity);
  void operator delete(void* p, size_t capacity);
  void operator delete(void* p);
  //@}
  //! Sizing constructor.
  Impl(Type type, size_t size);
  //! Copy constructor.
  Impl(const Impl& y);
  //! Copy constructor with specified size.
  Impl(const Impl& y, size_t size);
  //! Destructor.
  ~Impl();

  //! Resizes without copying the old contents.
  void resize_no_copy(size_t new_size);
  //! Grows without copying the old contents.
  void grow_no_copy(size_t new_size);
  //! Shrinks by erasing elements at the end.
  void shrink(size_t new_size);

  //! @name Subscript operators.
  //@{
  Integer& operator [](size_t k);
  const Integer& operator [](size_t k) const;
  //@}

  //! @name Type and size accessors.
  //@{
  Type type() const;
  void set_type(Type t);
  size_t size() const;
  void set_size(size_t new_size);
  void bump_size();
  //@}

private:
  //! The number of coefficients in the row.
  size_t size_;
  //! The type of this row.
  Type type_;
  //! The vector of coefficients.
  Integer vec_[PPL_FLEXIBLE_ARRAY];

private:
  //! Private and unimplemented: default construction is not allowed.
  Impl();
  //! Private and unimplemented: assignment is not allowed.
  Impl& operator =(const Impl&);
  //! Exception-safe copy construction mechanism.
  void copy_construct(const Impl& y);
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specialize <CODE>std::swap</CODE>.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Row& x,
	  Parma_Polyhedra_Library::Row& y);

} // namespace std

// If non-zero, lines and equalities are ALWAYS normalized so that the
// first non-zero coefficient is negative.
#define EXTRA_NORMALIZATION 0

#include "Row.inlines.hh"

#endif
