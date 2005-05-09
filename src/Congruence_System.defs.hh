/* Congruence_System class declaration.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Congruence_System_defs_hh
#define PPL_Congruence_System_defs_hh 1

#include "Congruence_System.types.hh"
#include "Linear_Expression.types.hh"
#include "Constraint.types.hh"
#include "Congruence.types.hh"
#include "Generator.types.hh"
#include "Matrix.defs.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Congruence_System
  Writes <CODE>true</CODE> if \p cgs is empty.  Otherwise, writes on
  \p s the congruences of \p cgs, all in one row and separated by ", ".
*/
std::ostream&
operator<<(std::ostream& s, const Congruence_System& cgs);

} // namespace IO_Operators

// Put this in the namespace here to declare it a friend later.

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Congruence_System */
bool
operator==(const Congruence_System& x, const Congruence_System& y);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Congruence_System */
void
swap(Parma_Polyhedra_Library::Congruence_System& x,
     Parma_Polyhedra_Library::Congruence_System& y);

} // namespace std

//! A system of congruences.
/*!
    An object of the class Congruence_System is a system of congruences,
    i.e., a multiset of objects of the class Congruence.
    When inserting congruences in a system, space dimensions are
    automatically adjusted so that all the congruences in the system
    are defined on the same vector space.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a system of congruences corresponding to
    an integer grid in \f$\Rset^2\f$:
    \code
  Congruence_System cgs;
  cgs.insert(x %= 0);
  cgs.insert(y %= 0);
    \endcode
    Note that:
    the congruence system is created with space dimension zero;
    the first and second congruence insertions increase the space
    dimension to \f$1\f$ and \f$2\f$, respectively.

    \par Example 2
    By adding to the congruence system of the previous example,
    the congruence \f$x + y = 1 \pmod{2}\f$:
    \code
  cgs.insert((x + y %= 1) / 2);
    \endcode
    we obtain the grid containing just those integral
    points where the sum of the \p x and \p y values is odd.

    \par Example 3
    The following code builds a system of congruences corresponding to
    the grid in \f$\Zset^2\f$ containing just the integral points on
    the \p x axis:
    \code
  Congruence_System cgs;
  cgs.insert(x %= 0);
  cgs.insert((y %= 0) / 0);
    \endcode

    \note
    After inserting a multiset of congruences in a congruence system,
    there are no guarantees that an <EM>exact</EM> copy of them
    can be retrieved:
    in general, only an <EM>equivalent</EM> congruence system
    will be available, where original congruences may have been
    reordered, removed (if they are trivial, duplicate or
    implied by other congruences), linearly combined, etc.
*/
class Parma_Polyhedra_Library::Congruence_System : private Matrix {
public:
  //! Default constructor: builds an empty system of congruences.
  Congruence_System();

#if 0 // FIX was this just a typo?
  //! Builds the singleton system containing only congruence \p cg.
  explicit Congruence_System(const Congruence& cg);
#endif

  //! \brief
  //! If \p cg represents the congruence \f$ e_1 = e_2 \f$, builds the
  //! singleton system containing only congruence \f$ e_1 = e_2 \pmod{0}\f$.
  /*!
    \exception std::invalid_argument
    Thrown if \p cg is not an equality congruence.
  */
  explicit Congruence_System(const Congruence& cg);

  //! Ordinary copy-constructor.
  Congruence_System(const Congruence_System& cs);

  //! Destructor.
  ~Congruence_System();

  //! Assignment operator.
  Congruence_System& operator=(const Congruence_System& y);

  //! Returns the maximum space dimension a Congruence_System can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Removes all the congruences from the congruence system
  //! and sets its space dimension to 0.
  void clear();

  //! \brief
  //! Inserts in \p *this a copy of the congruence \p cg,
  //! increasing the number of space dimensions if needed.
  void insert(const Congruence& cg);

  //! Add the rows in \p y to the end of the system.
  void add_rows(const Congruence_System& y);

  //! Adjusts all expressions to have the same moduli.
  void normalize_moduli();

  //! \brief
  //! Returns the singleton system containing only
  //! Congruence::zero_dim_false().
  // FIXME: do we want this?
  static const Congruence_System& zero_dim_empty();

  //! An iterator over a system of congruences.
  /*!
    A const_iterator is used to provide read-only access
    to each congruence contained in an object of Congruence_System.

    \par Example
    The following code prints the system of congruences
    defining the grid <CODE>gr</CODE>:
    \code
  const Congruence_System& cgs = gr.congruences();
  for (Congruence_System::const_iterator i = cgs.begin(),
         cgs_end = cgs.end(); i != cgs_end; ++i)
    cout << *i << endl;
    \endcode
  */
  class const_iterator
    : public std::iterator<std::forward_iterator_tag,
			   Congruence,
			   void,
			   const Congruence*,
			   const Congruence&> {
  public:
    //! Default constructor.
    const_iterator();

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Destructor.
    ~const_iterator();

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    const Congruence& operator*() const;

    //! Indirect member selector.
    const Congruence* operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator==(const const_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator!=(const const_iterator& y) const;

  private:
    friend class Congruence_System;

    //! The const iterator over the matrix of congruences.
    Matrix::const_iterator i;

    //! A const pointer to the matrix of congruences.
    const Matrix* csp;

    //! Constructor.
    const_iterator(const Matrix::const_iterator& iter,
		   const Congruence_System& csys);

    //! \p *this skips to the next non-trivial congruence.
    void skip_forward();
  };

  //! \brief
  //! Returns the const_iterator pointing to the first congruence,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  //! Checks if all the invariants are satisfied.
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*!

    Returns <CODE>true</CODE> if and only if \p *this is a valid
    Matrix, each row in the system is a valid Congruence and the
    number of columns is consistent with the number of congruences.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool OK() const;

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by
  //! \ref ascii_dump) and sets \p *this accordingly.
  //! Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Returns the number of equality congruences.
  dimension_type num_equalities() const;

  //! Returns the number of non-equality congruences.
  dimension_type num_non_equalities() const;

  //! Adjust \p *this to have \p new_space_dim dimensions.
  /*!
    \p new_space_dim must be greater than or equal to the current
    space dimension.
  */
  bool adjust_space_dimension(const dimension_type new_space_dim);

private:
  friend class const_iterator;
  friend class Grid;

  friend void std::swap(Parma_Polyhedra_Library::Congruence_System& x,
			Parma_Polyhedra_Library::Congruence_System& y);

  friend bool
  Parma_Polyhedra_Library::operator==(const Congruence_System& x,
				      const Congruence_System& y);

  // FIX where is this used?
  //! \brief
  //! Builds a system of \p n_rows congruences on a \p n_columns - 1
  //! dimensional space.
  Congruence_System(dimension_type n_rows, dimension_type n_columns);

  //! Swaps \p *this with \p y.
  void swap(Congruence_System& y);

  // FIX where will this be used?
  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! contains one or more linear equalities.
  bool has_linear_equalities() const;

  //! Returns the \p k- th congruence of the system.
  Congruence& operator[](dimension_type k);

  //! Returns a constant reference to the \p k- th congruence of the system.
  const Congruence& operator[](dimension_type k) const;

public:  // FIX for testing
  // FIX details?
  //! Returns <CODE>true</CODE> if \p g saturates all the congruences.
  bool saturates_all_congruences(const Generator& g) const;

  // FIX details?
  //! Returns <CODE>true</CODE> if \p g satisfies all the congruences.
  bool satisfies_all_congruences(const Generator& g) const;

  //! Returns <CODE>true</CODE> if \p g satisfies all the congruences.
  // FIX details
  bool satisfies_all_congruences(const Generator& g,
				 const Generator& ref) const;

private:

  //! \brief
  //! Substitutes a given column of coefficients by a given
  //! affine expression.
  /*!
    \param v
    Index of the column to which the affine transformation is substituted.

    \param expr
    The numerator of the affine transformation:
    \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$;

    \param denominator
    The denominator of the affine transformation.

    We want to allow affine transformations (see the Section \ref
    operations) having any rational coefficients. Since the coefficients
    of the congruences are integers we must also provide an integer
    \p denominator that will be used as denominator of the affine
    transformation.
    The denominator is required to be a positive integer.

    The affine transformation substitutes the matrix of congruences
    by a new matrix whose elements \f${a'}_{ij}\f$ are built from
    the old one \f$a_{ij}\f$ as follows:
    \f[
      {a'}_{ij} =
        \begin{cases}
          a_{ij} * \mathrm{denominator} + a_{iv} * \mathrm{expr}[j]
            \quad \text{for } j \neq v; \\
          \mathrm{expr}[v] * a_{iv}
            \quad \text{for } j = v.
        \end{cases}
    \f]

    \p expr is a constant parameter and unaltered by this computation.
  */
  void affine_preimage(dimension_type v,
		       const Linear_Expression& expr,
		       Coefficient_traits::const_reference denominator);

  //! Resizes the system without worrying about the old contents.
  /*!
    \param new_n_rows
    The number of rows of the resized system;

    \param new_n_columns
    The number of columns of the resized system.

    The system is expanded to the specified dimensions avoiding
    reallocation whenever possible.
    The contents of the original system is lost.
  */
  void resize_no_copy(dimension_type new_n_rows, dimension_type new_n_columns);

#if 0 // FIX
  //! \brief
  //! Inserts in \p *this a copy of the congruence \p cg,
  //! increasing the number of space dimensions if needed.
  //! It is a pending congruence.
  void insert_pending(const Congruence& cg);
#endif
};

// Congruence_System.inlines.hh is not included here on purpose.

#endif // !defined(PPL_Congruence_System_defs_hh)
