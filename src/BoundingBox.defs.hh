/* BoundingBox class declaration.
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

#ifndef PPL_BoundingBox_defs_hh
#define PPL_BoundingBox_defs_hh 1

#include "Integer.types.hh"
#include "Interval.defs.hh"
#include <vector>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

class BoundingBox {
public:
  //! Constructs a universe bounding box of dimension \p num_dimensions.
  BoundingBox(unsigned int num_dimensions);

  //! Returns the dimension of the vector space enclosing \p *this.
  unsigned int space_dimension() const;

  //! Returns a reference the interval that bounds
  //! the box on the <CODE>k</CODE>-th dimension.
  const Interval& operator[](size_t k) const;

  //! Returns <CODE>true</CODE> if and only if \p *this is empty.
  bool is_empty() const;

  //! If the <CODE>k</CODE>-th dimension is unbounded below,
  //! returns <CODE>false</CODE>.
  //! Otherwise returns <CODE>true</CODE> and set \p closed,
  //! \p n and \p d accordingly.
  /*!
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th dimension.
    If \f$I\f$ is not bounded from below, simply return <CODE>false</CODE>.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and <CODE>d</CODE>
    as follows: <CODE>closed</CODE> is set to <CODE>true</CODE> if the 
    the lower boundary of \f$I\f$ is closed and is set to <CODE>false</CODE>
    otherwise; <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.
    The fraction \f$n/d\f$ is in canonical form if and only if \f$n\f$
    and \f$d\f$ have no common factors and \f$d\f$ is positive, \f$0/1\f$
    being the unique representation for zero.
  */
  bool get_lower_bound(unsigned int k, bool& closed,
		       Integer& n, Integer& d) const;

  //! If the <CODE>k</CODE>-th dimension is unbounded above,
  //! returns <CODE>false</CODE>.
  //! Otherwise returns <CODE>true</CODE> and set \p closed,
  //! \p n and \p d accordingly.
  /*!
    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th dimension.
    If \f$I\f$ is not bounded from above, simply return <CODE>false</CODE>.
    Otherwise, set <CODE>closed</CODE>, <CODE>n</CODE> and <CODE>d</CODE>
    as follows: <CODE>closed</CODE> is set to <CODE>true</CODE> if the 
    the upper boundary of \f$I\f$ is closed and is set to <CODE>false</CODE>
    otherwise; <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.
  */
  bool get_upper_bound(unsigned int k, bool& closed,
		       Integer& n, Integer& d) const;

  //! Causes the box to become empty, i.e., to represent the empty set.
  void set_empty();

  //! Raises the lower bound of the interval corresponding
  //! to the <CODE>k</CODE>-th dimension.
  /*!
    Intersects the interval corresponding to the <CODE>k</CODE>-th dimension
    with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>false</CODE>.
    The fraction \f$n/d\f$ must be in canonical form;
  */
  void raise_lower_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d);

  //! Lowers the upper bound of the interval corresponding
  //! to the <CODE>k</CODE>-th dimension.
  /*!
    Intersects the interval corresponding to the <CODE>k</CODE>-th dimension
    with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE>
    is <CODE>false</CODE>.
    The fraction \f$n/d\f$ must be in canonical form.
  */
  void lower_upper_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d);

private:
  std::vector<Interval> vec;
  mutable bool empty;
  mutable bool empty_up_to_date;
};

std::ostream& operator<<(std::ostream& s, const BoundingBox& bbox);

} // namespace Parma_Polyhedra_Library

#include "BoundingBox.inlines.hh"

#endif // !defined(PPL_BoundingBox_defs_hh)
