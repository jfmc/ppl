/* Bounding_Box class implementation (non-inline functions).
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

#include <config.h>
#include "Bounding_Box.defs.hh"
#include "Variable.defs.hh"
#include "Constraint.defs.hh"
#include "Constraint_System.inlines.hh"
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

  //! \brief
  //! Assigns to \p *this the result of computing the 
  //! \ref CC76_widening "CC76-widening" between \p *this and \p y.
  /*!
    \param y                 A bounding box that <EM>must</EM>
                             be contained in \p *this.
    \param first             An iterator that points to the first
                             stop-point.
    \param last		     An iterator that points one past the last
                             stop-point.
    \exception std::invalid_argument thrown if \p *this and \p y
                                            are dimension-incompatible.
  */
template <typename Iterator>
void
PPL::Bounding_Box::CC76_widening_assign(const Bounding_Box& y,
					Iterator first, Iterator last) {
  for (dimension_type i = vec.size(); i-- > 0; ) {
    Interval& x_vec_i = vec[i];
    const Interval& y_vec_i = y.vec[i];

    // Upper bound.
    UBoundary& x_ub = x_vec_i.upper_bound();
    ERational& x_ubb = x_ub.bound();
    const ERational& y_ubb = y_vec_i.upper_bound().bound();
    assert(y_ubb <= x_ubb);
    if (y_ubb < x_ubb) {
      Iterator k = std::lower_bound(first, last, x_ubb);
      if (k != last) {
	if (x_ubb < *k)
	  x_ubb = *k;
      }
      else
	x_ub = UBoundary(ERational('+'), UBoundary::OPEN);
    }

    // Lower bound.
    LBoundary& x_lb = x_vec_i.lower_bound();
    ERational& x_lbb = x_lb.bound();
    const ERational& y_lbb = y_vec_i.lower_bound().bound();
    assert(y_lbb >= x_lbb);
    if (y_lbb > x_lbb) {
      Iterator k = std::lower_bound(first, last, x_lbb);
      if (k != last) {
	if (x_lbb < *k)
	  if (k != first)
	    x_lbb = *--k;
	  else
	    x_lb = LBoundary(ERational('-'), LBoundary::OPEN);
      }
      else
	x_lbb = *--k;
    }
  }
}

void
PPL::Bounding_Box::CC76_widening_assign(const Bounding_Box& y) {
  static ERational stop_points[] = {
    ERational(-2, 1),
    ERational(-1, 1),
    ERational(0, 1),
    ERational(1, 1),
    ERational(2, 1)
  };
  CC76_widening_assign(y,
		       stop_points,
		       stop_points
		       + sizeof(stop_points)/sizeof(stop_points[0]));
}

PPL::Constraint_System
PPL::Bounding_Box::constraints() const {
  Constraint_System cs;
  dimension_type space_dim = space_dimension();
  if (space_dim == 0) {
    if (is_empty())
      cs = Constraint_System::zero_dim_empty();
  }
  else if (is_empty())
    cs.insert(0*Variable(space_dim-1) <= -1);
  else {
    // KLUDGE: in the future `cs' will be constructed of the right dimension.
    // For the time being, we force the dimension with the following line.
    cs.insert(0*Variable(space_dim-1) <= 0);

    for (dimension_type k = 0; k < space_dim; ++k) {
      bool closed = false;
      PPL::Coefficient n;
      PPL::Coefficient d;
      if (get_lower_bound(k, closed, n, d)) {
	if (closed)
	  cs.insert(d*Variable(k) >= n);
	else
	  cs.insert(d*Variable(k) > n);
      }
      if (get_upper_bound(k, closed, n, d)) {
	if (closed)
	  cs.insert(d*Variable(k) <= n);
	else
	  cs.insert(d*Variable(k) < n);
      }
    }
  }
  return cs;
}

/*! \relates Parma_Polyhedra_Library::Bounding_Box */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const PPL::Bounding_Box& bbox) {
  if (bbox.is_empty()) {
    s << "empty";
    return s;
  }
  const dimension_type dimension = bbox.space_dimension();
  for (dimension_type k = 0; k < dimension; ++k) {
    bool closed = false;
    PPL::Coefficient n;
    PPL::Coefficient d;
    if (bbox.get_lower_bound(k, closed, n, d)) {
      s << (closed ? "[" : "(")
	<< n;
      if (d != 1)
	s << "/" << d;
      s << ", ";
    }
    else
      s << "(-inf, ";
    if (bbox.get_upper_bound(k, closed, n, d)) {
      s << n;
      if (d != 1)
	s << "/" << d;
      s << (closed ? "]" : ")");
    }
    else
      s << "+inf)";
    s << std::endl;
  }
  return s;
}
