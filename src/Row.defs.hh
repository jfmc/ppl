/* Row class declaration.
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

#ifndef PPL_Row_defs_hh
#define PPL_Row_defs_hh 1

#include "Row.types.hh"
#include "globals.defs.hh"
#include "Topology.hh"
#include "LinExpression.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Integer.defs.hh"
#include <cstddef>
#include <vector>

#ifndef EXTRA_ROW_DEBUG
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_defines
  \brief
  Enables extra debugging information for class Row.

  When <CODE>EXTRA_ROW_DEBUG</CODE> evaluates to <CODE>true</CODE>,
  each instance of the class Row carries its own capacity; this enables
  extra consistency checks to be performed.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define EXTRA_ROW_DEBUG 0
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for linear expressions, constraints and generators.
/*!
  The class Row allows us to build objects of the form
  \f$ [b, a_0, \ldots, a_{d-1}]_k^t \f$,
  where the superscript \f$t \in \{ \mathrm{c}, \mathrm{nnc} \}\f$ represents
  the <EM>topology</EM> and
  the subscript \f$k \in \{\mathord{=}, \mathord{\geq} \}\f$ represents
  the <EM>kind</EM> of the row object.
  The row's type Row::Type is obtained by combining the information about
  the topology \f$t\f$ and the kind \f$k\f$.
  Note that, even though all the four possible combinations of topology
  and kind values will result in a legal Row::Type object, some of these
  types pose additional constraints on the values of the row's coefficients.

  When \f$t = c\f$, we have the following cases
  (\f$d\f$ is the dimension of the vector space):
    - \f$[b, a_0, \ldots, a_{d-1}]_=^c\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq^c\f$
      represents the non-strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_=^c\f$
      represents the line of direction
      \f$\vect{l} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_\geq^c\f$
      represents the ray of direction
      \f$\vect{r} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_\geq^c\f$, with \f$b > 0\f$,
      represents the point
      \f$\vect{p} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.

  When \f$t = \mathrm{nnc}\f$, the last coefficient of the row is
  associated to the slack variable \f$\epsilon\f$, so that we have the
  following cases (\f$d\f$ is again the dimension of the vector space,
  but this time we have \f$d+2\f$ coefficients):
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_=^\mathrm{nnc}\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_\geq^\mathrm{nnc}\f$
      represents the non-strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, e]_\geq^\mathrm{nnc}\f$,
      with \f$e < 0\f$, represents the strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b > 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}, 0]_=^\mathrm{nnc}\f$
      represents the line of direction
      \f$\vect{l} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[0, a_0, \ldots, a_{d-1}, 0]_\geq^\mathrm{nnc}\f$
      represents the ray of direction
      \f$\vect{r} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, e]_\geq^\mathrm{nnc}\f$,
      with \f$b > 0\f$ and \f$e > 0\f$, represents the point
      \f$\vect{p} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_\geq^\mathrm{nnc}\f$,
      with \f$b > 0\f$, represents the closure point
      \f$\vect{c} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.

  So, a row can be both a constraint and a generator: it can be an
  equality, a strict or non-strict inequality, a line, a ray, a point
  or a closure point.

  The inhomogeneous term of a constraint can be zero or different from zero.

  Points and closure points must have a positive inhomogeneous term
  (which is used as a common divisor for the coefficients),
  lines and rays must have the inhomogeneous term equal to zero.
  If needed, the coefficients of points and closure points are negated
  at creation time so that they satisfy this invariant.
  The invariant is maintained because, when combining a point or closure
  point with another generator, we only consider positive combinations.

  The \f$\epsilon\f$ coefficient, when present, is negative for strict
  inequality constraints, positive for points and equal to zero in all
  the other cases.
  Note that the above description corresponds to the end-user, high-level
  view of a Row object. In the implementation, to allow for code reuse,
  it is sometimes useful to regard an \f$\mathrm{nnc}\f$-object on
  the vector space \f$\Rset^d\f$ as if it was a \f$\mathrm{c}\f$-object on the
  vector space \f$\Rset^{d+1}\f$, therefore interpreting the slack
  variable \f$\epsilon\f$ as an ordinary dimension of the vector space.

  A Row object implementing a LinExpression is always of the form
  \f$ [0, a_0, \ldots, a_{d-1}]_=^c \f$, which represents the
  linear expression \f$\sum_{i=0}^{d-1} a_i x_i\f$.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Row {
public:
  //! \brief
  //! The type of the object to which the coefficients refer to,
  //! encoding both the row topology and the row kind.
  class Type;

  //! The possible kinds of Row objects.
  enum Kind {
    LINE_OR_EQUALITY = 0,
    RAY_OR_POINT_OR_INEQUALITY = 1
  };

  //! Pre-constructs a row: construction must be completed by construct().
  Row();

  //! \name Post-constructors
  //@{
  //! Constructs properly a default-constructed element.
  /*!
    Builds a row with type \p t, size \p sz and minimum capacity.
  */
  void construct(Type t, dimension_type sz);

  //! Constructs properly a default-constructed element.
  /*!
    \param t
    The type of the row that will be constructed;

    \param sz
    The size of the row that will be constructed;

    \param capacity
    The minimum capacity of the row that will be constructed.

    The row that we are constructing has a minimum capacity, i.e., it
    can contain at least \p capacity elements, \p sz of which will be
    constructed now.
  */
  void construct(Type t, dimension_type sz, dimension_type capacity);
  //@} // Post-constructors

  //! Tight constructor: resizing will require reallocation.
  Row(Type t, dimension_type sz);

  //! Sizing constructor with capacity.
  Row(Type t, dimension_type sz, dimension_type capacity);

  //! Ordinary copy constructor.
  Row(const Row& y);

  //! Copy constructor with specified capacity.
  /*!
    It is assumed that \p capacity is greater than or equal to \p y size.
  */
  Row(const Row& y, dimension_type capacity);

  //! Copy constructor with specified size and capacity.
  /*!
    It is assumed that \p sz is greater than or equal to the size of \p y
    and, of course, that \p sz is less than or equal to \p capacity.
  */
  Row(const Row& y, dimension_type sz, dimension_type capacity);

  //! Destructor.
  ~Row();

  //! Assignment operator.
  Row& operator=(const Row& y);

  //! Swaps \p *this with \p y.
  void swap(Row& y);

  //! Assigns the implementation of \p y to \p *this.
  /*!
    To be used with extra care,
    since it may easily cause a memory leak.
  */
  void assign(Row& y);

  //! Resizes the row without copying the old contents.
  /*!
    Shrinks the row if \p new_sz is less than <CODE>size()</CODE>;
    otherwise grows the row without copying the old contents.
  */
  void resize_no_copy(dimension_type new_sz);

  //! Grows the row without copying the old contents.
  /*!
    Adds new positions to the implementation of the row
    obtaining a new row with size \p new_sz.
  */
  void grow_no_copy(dimension_type new_sz);

  //! Shrinks the row by erasing elements at the end.
  /*!
    Destroys elements of the row implementation
    from position \p new_sz to the end.
  */
  void shrink(dimension_type new_sz);

  //! \name Type inspection methods
  //@{
  //! Returns the type (topological and row kind) of \p *this.
  Type type() const;

  //! Returns the topological kind of \p *this.
  Topology topology() const;

  //! \brief Returns <CODE>true</CODE> if and only if the topology
  //! of \p *this row is necessarily closed.
  bool is_necessarily_closed() const;

  //! \brief Returns <CODE>true</CODE> if and only if \p *this row
  //! represents a line or an equality.
  bool is_line_or_equality() const;

  //! \brief Returns <CODE>true</CODE> if and only if \p *this row
  //! represents a ray, a point or an inequality.
  bool is_ray_or_point_or_inequality() const;
  //@} // Type inspection methods

  //! \name Type coercion methods
  //@{
  //! Sets to \p NECESSARILY_CLOSED the topological kind of \p *this row.
  void set_necessarily_closed();

  //! Sets to \p NOT_NECESSARILY_CLOSED the topological kind of \p *this row.
  void set_not_necessarily_closed();

  //! Sets to \p LINE_OR_EQUALITY the kind of \p *this row.
  void set_is_line_or_equality();

  //! Sets to \p RAY_OR_POINT_OR_INEQUALITY the kind of \p *this row.
  void set_is_ray_or_point_or_inequality();
  //@} // Type coercion methods

  //! Returns the size() of the largest possible Row. 
  static dimension_type max_size();

  //! Gives the number of coefficients currently in use.
  dimension_type size() const;

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns the inhomogeneous term.
  const Integer& inhomogeneous_term() const;

  //! Returns the coefficient \f$a_n\f$.
  const Integer& coefficient(dimension_type n) const;

  //! \name Subscript operators
  //@{
  //! Returns a reference to the element of the row indexed by \p k.
  Integer& operator[](dimension_type k);

  //! Returns a constant reference to the element of the row indexed by \p k.
  const Integer& operator[](dimension_type k) const;
  //@} // Subscript operators

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! \brief
  //! Normalizes the sign of the coefficients so that the first non-zero
  //! (homogeneous) coefficient of a line-or-equality is positive.
  void sign_normalize();

  //! \brief
  //! Strong normalization: ensures that different rows represent
  //! different hyperplanes or hyperspaces.
  /*!
    Applies both Row::normalize() and Row::sign_normalize().
  */
  void strong_normalize();

  //! \brief
  //! Returns <CODE>true</CODE> if and only if the row is strongly normalized.
  bool check_strong_normalized() const;

  //! Linearly combines \p *this with \p y so that <CODE>*this[k]</CODE> is 0.
  /*!
    \param y
    The row that will be combined with \p *this object;

    \param k
    The position of \p *this that have to be \f$0\f$.

    Computes a linear combination of \p *this and \p y having
    the element of index \p k equal to \f$0\f$. Then it assigns
    the resulting row to \p *this and normalizes it.
  */
  void linear_combine(const Row& y, dimension_type k);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if all the homogeneous
  //! terms of \p *this are \f$0\f$.
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

#endif // EXTRA_ROW_DEBUG
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Computes the scalar product between \p x and \p y and assigns it to \p z.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void scalar_product_assign(Integer& z, const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns the sign of the scalar product between \p x and \p y.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int scalar_product_sign(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e reduced scalar product between \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored,
//! and assigns the result to \p z.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void reduced_scalar_product_assign(Integer& z, const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the \e reduced scalar product between \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored.
/*! \relates Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int reduced_scalar_product_sign(const Row& x, const Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The basic comparison function.
/*! \relates Row
  \return
  The returned absolute value can be \f$0\f$, \f$1\f$ or \f$2\f$.

  \param x
  A row of coefficients;

  \param y
  Another row.

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
//! \name Classical comparison operators
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
//@} // Classical comparison operators
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The type of a Row object.
/*!
  This combines the information about the topology (necessarily closed
  or not) and the kind (line/equality or ray/point/inequality)
  of a Row object.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Row::Type {
public:
  //! The default Row::Type is a line/equality of a closed polyhedron.
  Type();

  //! \brief
  //! Builds the Row type by providing the two needed
  //! pieces of information.
  Type(Topology topol, Kind kind);

  //! \name Testing and setting the type
  //@{
  Topology topology() const;
  bool is_necessarily_closed() const;
  bool is_line_or_equality() const;
  bool is_ray_or_point_or_inequality() const;

  void set_necessarily_closed();
  void set_not_necessarily_closed();
  void set_is_line_or_equality();
  void set_is_ray_or_point_or_inequality();
  //@} // Testing and setting the type

private:
  //! Type is implemented by means of a finite bitset.
  typedef unsigned int flags_t;

  //! Builds the type from a bitmask.
  Type(flags_t mask);

  //! This holds the current bitset.
  flags_t flags;

  //! \name The bits that are currently in use
  //@{
  static const flags_t NNC = NOT_NECESSARILY_CLOSED << 0;
  static const flags_t RPI = RAY_OR_POINT_OR_INEQUALITY << 1;
  //@} // The bits that are currently in use

  //! Check whether <EM>all</EM> bits in \p mask are set.
  bool test_all(flags_t mask) const;

  //! Set the bits in \p mask.
  void set(flags_t mask);

  //! Reset the bits in \p mask.
  void reset(flags_t mask);
};


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The real implementation of a Row object.
/*!
  The class Row::Impl provides the implementation of Row objects and,
  in particular, of the corresponding memory allocation functions.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Row::Impl {
public:
  //! \name Custom allocator and deallocator
  //@{

  /*! \brief
    Allocates a chunk of memory able to contain \p capacity Integer objects
    beyond the specified \p fixed_size and returns a pointer to the new
    allocated memory.
  */
  void* operator new(size_t fixed_size, dimension_type capacity);

  //! Uses the standard delete operator to free the memory \p p points to.
  void operator delete(void* p);

  //! \brief Placement version:
  //! uses the standard operator delete to free the memory \p p points to.
  void operator delete(void* p, dimension_type capacity);
  //@} // Custom allocator and deallocator

  //! Sizing constructor.
  Impl(Type t, dimension_type sz);

  //! Copy constructor.
  Impl(const Impl& y);

  //! Copy constructor with specified size.
  Impl(const Impl& y, dimension_type sz);

  //! Destructor.
  /*!
    Uses <CODE>shrink()</CODE> method with argument \f$0\f$
    to delete all the row elements.
  */
  ~Impl();

  //! Resizes without copying the old contents.
  /*!
    If \p new_sz is less than <CODE>size()</CODE>, shrinks the implementation
    of the row; otherwise the real implementation is grown without copying
    the old contents.
  */
  void resize_no_copy(dimension_type new_sz);

  //! Grows without copying the old contents.
  void grow_no_copy(dimension_type new_sz);

  //! Shrinks by erasing elements at the end.
  void shrink(dimension_type new_sz);

  //! Returns the size() of the largest possible Impl. 
  static dimension_type max_size();

  //! \name Size accessors
  //@{
  //! Returns the actual size of \p this.
  dimension_type size() const;

  //! Sets to \p new_sz the actual size of \p *this.
  void set_size(dimension_type new_sz);

  //! Increment the size of \p *this by 1.
  void bump_size();
  //@} // Size accessors

  //! \name Subscript operators
  //@{
  //! Returns a reference to the element of \p *this indexed by \p k.
  Integer& operator[](dimension_type k);

  //! Returns a constant reference to the element of \p *this indexed by \p k.
  const Integer& operator[](dimension_type k) const;
  //@} // Subscript operators

private:
  friend class Parma_Polyhedra_Library::Row;

  //! The number of coefficients in the row.
  dimension_type size_;

  //! The type of this row.
  Type type;

  //! The vector of coefficients.
  Integer vec_[
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       1
#endif
  ];

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

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::iter_swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void iter_swap(std::vector<Parma_Polyhedra_Library::Row>::iterator x,
	       std::vector<Parma_Polyhedra_Library::Row>::iterator y);

} // namespace std

#include "Row.inlines.hh"

#endif // !defined(PPL_Row_defs_hh)
