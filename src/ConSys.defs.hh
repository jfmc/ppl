/* ConSys class declaration.
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

#ifndef PPL_ConSys_defs_hh
#define PPL_ConSys_defs_hh 1

#include "LinExpression.types.hh"
#include "ConSys.types.hh"
#include "Matrix.defs.hh"
#include "Generator.types.hh"
#include "Polyhedron.types.hh"
#include "Constraint.types.hh"
#include <cstddef>
#include <vector>
#include <iterator>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

//! Output operator.
/*!
  \relates ConSys
  Writes <CODE>true</CODE> if \p cs is empty.  Otherwise, writes on
  \p s the constraints of \p cs, all in one row and separated by ", ".
*/
std::ostream& operator<<(std::ostream& s, const ConSys& cs);

// Put it in the namespace here to declare it friend later.
bool operator<=(const Polyhedron& x, const Polyhedron& y);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::ConSys */
void swap(Parma_Polyhedra_Library::ConSys& x,
	  Parma_Polyhedra_Library::ConSys& y);

} // namespace std

//! A system of constraints.
/*!
    An object of the class ConSys is a system of constraints,
    i.e., a multiset of objects of the class Constraint.
    When inserting constraints in a system, dimensions are automatically
    adjusted so that all the constraints in the system are defined
    on the same vector space.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a system of constraints corresponding to
    a square in \f$\Rset^2\f$:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
    \endcode
    Note that:
    the constraint system is created with space dimension zero;
    the first and third constraint insertions increases the space
    dimension to \f$1\f$ and \f$2\f$, respectively.

    \par Example 2
    By adding four strict inequalities to the constraint system
    of the previous example, we can remove just the four
    vertices from the square defined above.
    \code
  cs.insert(x + y > 0);
  cs.insert(x + y < 6);
  cs.insert(x - y < 3);
  cs.insert(y - x < 3);
    \endcode

    \par Example 3
    The following code builds a system of constraints corresponding to
    a half-strip in \f$\Rset^2\f$:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
    \endcode

    \note
    After inserting a multiset of constraints in a constraint system,
    there are no guarantees that an <EM>exact</EM> copy of them
    can be retrieved:
    in general, only an <EM>equivalent</EM> constraint system
    will be available, where original constraints may have been
    reordered, removed (if they are trivial, duplicate or
    implied by other constraints), linearly combined, etc.
*/
class Parma_Polyhedra_Library::ConSys : private Matrix {
public:
  //! Default constructor: builds an empty system of constraints.
  ConSys();

  //! Builds the singleton system containing only constraint \p c.
  ConSys(const Constraint& c);

  //! Ordinary copy-constructor.
  ConSys(const ConSys& cs);

  //! Destructor.
  virtual ~ConSys();

  //! Assignment operator.
  ConSys& operator=(const ConSys& y);

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Removes all the constraints from the constraint system
  //! and sets its space dimension to 0.
  void clear();

  //! \brief
  //! Inserts a copy of the constraint \p c into \p *this
  //! increasing the number of dimensions if needed.
  void insert(const Constraint& c);

  //! \brief
  //! Returns the singleton system containing only
  //! Constraint::zero_dim_false().
  static const ConSys& zero_dim_empty();

  //! An iterator over a system of constraints.
  /*!
    A const_iterator is used to provide read-only access
    to each constraint contained in an object of ConSys.

    \par Example
    The following code prints the system of constraints
    defining the polyhedron <CODE>ph</CODE>:
    \code
  const ConSys cs = ph.constraints();
  ConSys::const_iterator iend = cs.end();
  for (ConSys::const_iterator i = cs.begin(); i != iend; ++i)
    cout << *i << endl;
    \endcode
  */
  class const_iterator
    : public std::iterator<std::forward_iterator_tag,
			   Constraint,
			   void,
			   const Constraint*,
			   const Constraint&> {
  public:
    //! Default constructor.
    const_iterator();

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Destructor.
    virtual ~const_iterator();

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    const Constraint& operator*() const;

    //! Indirect member selector.
    const Constraint* operator->() const;

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
    friend class ConSys;

    //! The const iterator over the matrix of constraints.
    Matrix::const_iterator i;

    //! A const pointer to the matrix of constraints.
    const Matrix* csp;

    //! Constructor.
    const_iterator(const Matrix::const_iterator& iter, const ConSys& csys);

    //! \p *this skips to the next non-trivial constraint.
    void skip_forward();
  };

  //! \brief
  //! Returns the const_iterator pointing to the first constraint,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  //! Checks if all the invariants are satisfied.
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*!
    Returns <CODE>true</CODE> if and only if \p *this is a valid Matrix
    and every row in the matrix must be a valid Constraint.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool OK() const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  /*!
    After invoking the <CODE>Matrix::ascii_dump()</CODE> method,
    prints the contents of each row, specifying the type
    of the encoded constraint.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Loads from \p s an ASCII representation (as produced by
  //! \ref ascii_dump) and sets \p *this accordingly.
  //! Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  /*!
    Resizes the matrix of constraints using the numbers of rows and columns
    read from \p s, then initializes the coefficients of each constraint
    and its type (equality or inequality) reading the contents from \p s.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

private:
  friend class Parma_Polyhedra_Library::Polyhedron;

  friend bool
  Parma_Polyhedra_Library::operator<=(const Polyhedron& x,
				      const Polyhedron& y);

  friend void std::swap(Parma_Polyhedra_Library::ConSys& x,
			Parma_Polyhedra_Library::ConSys& y);

  //! Builds an empty system of constraints having the specified topology.
  ConSys(Topology topol);

  //! \brief
  //! Builds a system of \p n_rows constraints on a \p n_columns - 1
  //! dimensional space (including the \f$\epsilon\f$ dimension, if
  //! \p topol is <CODE>NOT_NECESSARILY_CLOSED</CODE>).
  ConSys(Topology topol, dimension_type n_rows, dimension_type n_columns);

  //! Swaps \p *this with \p y.
  void swap(ConSys& y);

  //! \brief
  //! Adjusts \p *this so that it matches the topology and
  //! the number of dimensions given as parameters
  //! (adding or removing columns if needed).
  //! Returns <CODE>false</CODE> if and only if \p topol is
  //! equal to <CODE>NECESSARILY_CLOSED</CODE> and \p *this
  //! contains strict inequalities.
  bool adjust_topology_and_dimension(Topology topol,
				     dimension_type num_dimensions);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! contains one or more strict inequality constraints.
  bool has_strict_inequalities() const;

  //! Returns the \p k- th constraint of the system.
  Constraint& operator[](dimension_type k);

  //! Returns a constant reference to the \p k- th constraint of the system.
  const Constraint& operator[](dimension_type k) const;

  //! Returns <CODE>true</CODE> if \p g satisfies all the constraints.
  bool satisfies_all_constraints(const Generator& g) const;

  //! \brief
  //! Substitutes a given column of coefficients by a given
  //! affine expression.
  /*!
    \param v            Index of the column to which the
                        affine transformation is substituted.
    \param expr         The numerator of the affine transformation:
                        \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$
    \param denominator  The denominator of the affine transformation.

    We want to allow affine transformations (see the Section \ref
    operations) having any rational coefficients. Since the coefficients
    of the constraints are integers we must also provide an integer
    \p denominator that will be used as denominator of the affine
    transformation.
    The denominator is required to be a positive integer.

    The affine transformation substitutes the matrix of constraints
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
		       const LinExpression& expr,
		       const Integer& denominator);

  //! Returns the number of the equality constraints.
  dimension_type num_equalities() const;

  //! Returns the number of the inequality constraints.
  dimension_type num_inequalities() const;
};

// ConSys.inlines.hh is not included here on purpose.

#endif // !defined(PPL_ConSys_defs_hh)
