/* ConSys class declaration.
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

#ifndef _ConSys_defs_hh
#define _ConSys_defs_hh 1

#include "LinExpression.defs.hh"
#include "ConSys.types.hh"
#include "Matrix.defs.hh"
#include "Generator.types.hh"
#include "Constraint.defs.hh"
#include <cstddef>
#include <vector>


//! A system of constraints.
/*!
    An object of the class ConSys is a system of constraints,
    i.e. a multiset of objects of the class Constraint.
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

    \par Example 2
    The following code builds a system of constraints corresponding to
    a half-strip in \f$\Rset^2\f$:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
    \endcode
*/
class Parma_Polyhedra_Library::ConSys : PPL_INTERNAL Matrix {
public:
  //! Default constructor: builds an empty system of constraints.
  ConSys();
  //! Ordinary copy-constructor.
  ConSys(const ConSys& cs);
  //! Destructor.
  virtual ~ConSys();

  //! Inserts a copy of the constraint \p c into \p *this,
  //! increasing the number of dimensions if needed.
  void insert(const Constraint& c);

  //! Swaps \p *this with the system of constraints \p y.
  void swap(ConSys& y);

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

  private:
    Matrix::const_iterator i;
    const Matrix* csp;

    //! Constructor.
    const_iterator(const Matrix::const_iterator& iter, const ConSys& csys);
    //! \p *this skips to the last non-trivial constraint.
    void skip_forward();

    friend class ConSys;

  public:
    //! Default constructor.
    const_iterator();
    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);
    //! Destructor.
    virtual ~const_iterator();
    //! Assignment operator.
    const_iterator& operator =(const const_iterator& y);
    //! Dereference operator.
    const Constraint& operator *() const;
    //! Indirect member selector.
    const Constraint* operator ->() const;
    //! Prefix increment operator.
    const_iterator& operator ++();
    //! Postfix increment operator.
    const_iterator operator ++(int);
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator ==(const const_iterator& y) const;
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator !=(const const_iterator& y) const;
  };

  //! Returns the const_iterator pointing to the first constraint,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;
  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

PPL_INTERNAL:
  //! Constructor: it builds a system of \p num_rows constraints
  //! on a \p num_columns - 1 dimensional space.
  ConSys(size_t num_rows, size_t num_columns);

  //! Returns the \p k- th constraint of the system.
  Constraint& operator [](size_t k);
  //! Returns a constant reference to the \p k- th constraint of the system.
  const Constraint& operator [](size_t k) const;

  //! Returns <CODE>true</CODE> if \p g satisfies all the constraints.
  bool satisfies_all_constraints(const Generator& g) const;

  //! Substitutes a given column of coefficients by a given affine expression.
  void substitute_variable(size_t v,
			   const LinExpression& expr,
			   const Integer& denominator);

  //! Returns the number of the equality constraints.
  size_t num_equalities() const;
  //! Returns the number of the inequality constraints.
  size_t num_inequalities() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

 private:
  //! Input operator.
  void get(std::istream& s);
  //! Output operator.
  void print(std::ostream& s) const;
};

namespace std {
  //! Specialize std::swap.
  void swap(Parma_Polyhedra_Library::ConSys& x,
	    Parma_Polyhedra_Library::ConSys& y);
}

#include "ConSys.inlines.hh"

#endif
