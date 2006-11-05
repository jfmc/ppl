/* Box class declaration.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Box_defs_hh
#define PPL_Box_defs_hh 1

#include "Box.types.hh"
#include "globals.types.hh"
#include "Interval.defs.hh"
#include "Coefficient.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include <vector>
#include <iosfwd>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A not necessarily closed, iso-oriented hyperrectangle.
/*! \ingroup PPL_CXX_interface
  A Box object represents the Cartesian product of \f$n\f$
  not necessarily closed and possibly unbounded intervals
  represented by objects of class \p Interval,
  where \f$n\f$ is the space dimension of the box.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename Interval>
class Parma_Polyhedra_Library::Box {
public:
  //! Constructs a universe bounding box of dimension \p num_dimensions.
  Box(dimension_type num_dimensions);

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is an empty box.
  bool is_empty() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a universe box.
  bool is_universe() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    is a topologically closed subset of the vector space.
  */
  bool is_topologically_closed() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is discrete.
  bool is_discrete() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a bounded box.
  bool is_bounded() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    contains at least one integer point.
  */
  bool contains_integer_point() const;

  /*! \brief
    Returns a reference the interval that bounds
    the box on the <CODE>k</CODE>-th space dimension.
  */
  const Interval& operator[](dimension_type k) const;

  /*! \brief
    If the <CODE>k</CODE>-th space dimension is unbounded below, returns
    <CODE>false</CODE>. Otherwise returns <CODE>true</CODE> and set
    \p closed, \p n and \p d accordingly.

    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from below, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the the lower boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the greatest lower bound of \f$I\f$.  The fraction
    \f$n/d\f$ is in canonical form if and only if \f$n\f$ and \f$d\f$
    have no common factors and \f$d\f$ is positive, \f$0/1\f$ being
    the unique representation for zero.

    An undefined behavior is obtained if \p k is greater than
    or equal to the space dimension of \p *this.
  */
  bool get_lower_bound(dimension_type k, bool& closed,
		       Coefficient& n, Coefficient& d) const;

  /*! \brief
    If the <CODE>k</CODE>-th space dimension is unbounded above, returns
    <CODE>false</CODE>. Otherwise returns <CODE>true</CODE> and set
    \p closed, \p n and \p d accordingly.

    Let \f$I\f$ the interval corresponding to the <CODE>k</CODE>-th
    space dimension.  If \f$I\f$ is not bounded from above, simply return
    <CODE>false</CODE>.  Otherwise, set <CODE>closed</CODE>,
    <CODE>n</CODE> and <CODE>d</CODE> as follows: <CODE>closed</CODE>
    is set to <CODE>true</CODE> if the the upper boundary of \f$I\f$
    is closed and is set to <CODE>false</CODE> otherwise;
    <CODE>n</CODE> and <CODE>d</CODE> are assigned the integers
    \f$n\f$ and \f$d\f$ such that the canonical fraction \f$n/d\f$
    corresponds to the least upper bound of \f$I\f$.

    An undefined behavior is obtained if \p k is greater than
    or equal to the space dimension of \p *this.
  */
  bool get_upper_bound(dimension_type k, bool& closed,
		       Coefficient& n, Coefficient& d) const;

  //! Causes the box to become empty, i.e., to represent the empty set.
  void set_empty();

  /*! \brief
    Raises the lower bound of the interval corresponding
    to the <CODE>k</CODE>-th space dimension.

    Intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension
    with \f$[n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(n/d, +\infty)\f$ if <CODE>closed</CODE> is <CODE>false</CODE>.
    An undefined behavior is obtained if \p k is greater than or equal to
    the space dimension of \p *this or if \p d is equal to zero.
  */
  void raise_lower_bound(dimension_type k, bool closed,
			 Coefficient_traits::const_reference n,
			 Coefficient_traits::const_reference d);

  /*! \brief
    Lowers the upper bound of the interval corresponding
    to the <CODE>k</CODE>-th space dimension.

    Intersects the interval corresponding to the <CODE>k</CODE>-th
    space dimension
    with \f$(-\infty, n/d]\f$ if <CODE>closed</CODE> is <CODE>true</CODE>,
    with \f$(-\infty, n/d)\f$ if <CODE>closed</CODE>
    is <CODE>false</CODE>.
    An undefined behavior is obtained if \p k is greater than or equal to
    the space dimension of \p *this or if \p d is equal to zero.
  */
  void lower_upper_bound(dimension_type k, bool closed,
			 Coefficient_traits::const_reference n,
			 Coefficient_traits::const_reference d);

  //! Returns a system of constraints corresponding to \p *this.
  Constraint_System constraints() const;

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref CC76_extrapolation "CC76-widening" between \p *this and \p y.

    \param y
    A bounding box that <EM>must</EM> be contained in \p *this.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void CC76_widening_assign(const Box& y);

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref CC76_extrapolation "CC76-widening" between \p *this and \p y.

    \param y
    A bounding box that <EM>must</EM> be contained in \p *this.

    \param first
    An iterator that points to the first stop-point.

    \param last
    An iterator that points one past the last stop-point.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  template <typename Iterator>
  void CC76_widening_assign(const Box& y,
			    Iterator first, Iterator last);

private:
  //! A vector of intervals, one for each dimension of the vector space.
  std::vector<Interval> vec;

  /*! \brief
    A Boolean flag indicating emptiness of the box.
    Only meaningful when \p empty_up_to_date is <CODE>true</CODE>.
  */
  mutable bool empty;

  //! Tells whether or not the flag \p empty is meaningful.
  mutable bool empty_up_to_date;

  /*! \brief
    Checks the hard way whether \p *this is an empty box:
    returns <CODE>true</CODE> if and only if it is so.
  */
  bool check_empty() const;
};

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Output operator.
/*! \relates Parma_Polyhedra_Library::Box */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename Interval>
std::ostream& operator<<(std::ostream& s, const Box<Interval>& box);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "Box.inlines.hh"
#include "Box.templates.hh"

#endif // !defined(PPL_Box_defs_hh)
