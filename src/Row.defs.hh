/* Row class declaration.
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

#ifndef _Row_defs_hh
#define _Row_defs_hh 1

#include "Topology.hh"
#include "Row.types.hh"
#include "Integer.types.hh"
#include <cstddef>
#include <iosfwd>

namespace Parma_Polyhedra_Library {
  // Put it in the namespace here to declare it friend later.
  std::ostream& operator<<(std::ostream& s, const Row& row);
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
      represents the point
      \f$\sum_{i=0}^{d-1} \frac{a_i}{b} \vec{x}_i\f$.

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
  const Integer& coefficient(size_t n) const;

public:

  enum Kind {
    LINE_OR_EQUALITY = 0,
    RAY_OR_POINT_OR_INEQUALITY = 1
  };

  //! The type of the object to which the coefficients refer to.
  class Type;

  //! Tight constructor: resizing will require reallocation.
  Row(Type t, size_t sz);

  //! Sizing constructor with type.
  Row(Type t, size_t sz, size_t capacity);

  //! Post-constructors: to construct properly default-constructed elements.
  //@{
  void construct(Type t, size_t sz);
  void construct(Type t, size_t sz, size_t capacity);
  //@}

  //! Pre-constructs a row: construction must be completed by construct().
  Row();

  //! Ordinary copy constructor.
  Row(const Row& y);

  //! Copy constructor with specified capacity.
  Row(const Row& y, size_t capacity);

  //! Copy constructor with specified size and capacity.
  Row(const Row& y, size_t sz, size_t capacity);

  //! Destructor.
  ~Row();

  //! Assignment operator.
  Row& operator=(const Row& y);

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
  Integer& operator[](size_t k);
  const Integer& operator[](size_t k) const;
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

public:
  //! Gives the number of coefficients currently in use.
  size_t size() const;

  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

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
  Parma_Polyhedra_Library::operator<<(std::ostream& s, const Row& row);

  //! Checks if all the invariants are satisfied.
  bool OK(size_t row_size, size_t row_capacity) const;

private:
  class Impl;

  //! The real implementation, as far as memory allocation is concerned.
  Impl* impl;

#ifndef NDEBUG
  //! The capacity of the row (only available during debugging).
  size_t capacity_;

PPL_INTERNAL:
  //! Returns the capacity of the row (only available during debugging).
  size_t capacity() const;
#endif

};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Computes the scalar product between \p x and \p y.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  const Integer& operator*(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Computes the \e reduced scalar product between \p x and \p y,
  //! where the \f$\epsilon\f$ coefficient of \p x is ignored.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  const Integer& reduced_scalar_product(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! The basic comparison function.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  int compare(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! @name Classical comparison operators.
  /*! \relates Row */
  //@{
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool operator==(const Row& x, const Row& y);
  bool operator!=(const Row& x, const Row& y);
  bool operator<=(const Row& x, const Row& y);
  bool operator <(const Row& x, const Row& y);
  bool operator>=(const Row& x, const Row& y);
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
  //!@}

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
  void* operator new(size_t fixed_size, size_t capacity);
  void operator delete(void* p, size_t capacity);
  void operator delete(void* p);
  //@}

  //! Sizing constructor.
  Impl(Type t, size_t sz);

  //! Copy constructor.
  Impl(const Impl& y);

  //! Copy constructor with specified size.
  Impl(const Impl& y, size_t sz);

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
  Integer& operator[](size_t k);
  const Integer& operator[](size_t k) const;
  //@}

  //! @name Size accessors.
  //@{
  size_t size() const;
  void set_size(size_t new_sz);
  void bump_size();
  //@}

private:
  //! The number of coefficients in the row.
  size_t size_;

public:
  // FIXME: this should become private.
  //! The type of this row.
  Type type;

private:
  //! The vector of coefficients.
  Integer vec_[PPL_FLEXIBLE_ARRAY];

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
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Row& x,
	  Parma_Polyhedra_Library::Row& y);

} // namespace std

// If non-zero, lines and equalities are ALWAYS normalized so that the
// first non-zero coefficient is negative.
#define EXTRA_NORMALIZATION 0

#include "Row.inlines.hh"

#endif
