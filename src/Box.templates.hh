/* Box class implementation: non-inline template functions.
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

#ifndef PPL_Box_templates_hh
#define PPL_Box_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename Interval>
template <typename Iterator>
void
Box<Interval>::CC76_widening_assign(const Box& y,
				    Iterator first, Iterator last) {
  for (dimension_type i = vec.size(); i-- > 0; ) {
    Interval& x_vec_i = vec[i];
    const Interval& y_vec_i = y.vec[i];

    // Upper bound.
    typename Interval::boundary_type& x_ub = x_vec_i.upper();
    const typename Interval::boundary_type& y_ub = y_vec_i.upper();
    assert(y_ub <= x_ub);
    if (y_ub < x_ub) {
      Iterator k = std::lower_bound(first, last, x_ub);
      if (k != last) {
	if (x_ub < *k)
	  x_ub = *k;
      }
      else
	// FIXME: set x_ub to unbounded.
	;
    }

    // Lower bound.
    typename Interval::boundary_type& x_lb = x_vec_i.upper();
    const typename Interval::boundary_type& y_lb = y_vec_i.upper();
    assert(y_lb >= x_lb);
    if (y_lb > x_lb) {
      Iterator k = std::lower_bound(first, last, x_lb);
      if (k != last) {
	if (x_lb < *k)
	  if (k != first)
	    x_lb = *--k;
	  else
	    // FIXME: set x_lb to unbounded.
	    ;
      }
      else
	x_lb = *--k;
    }
  }
}

template <typename Interval>
void
Box<Interval>::CC76_widening_assign(const Box& y) {
  static typename Interval::boundary_type stop_points[] = {
    typename Interval::boundary_type(-2),
    typename Interval::boundary_type(-1),
    typename Interval::boundary_type(0),
    typename Interval::boundary_type(1),
    typename Interval::boundary_type(2)
  };
  CC76_widening_assign(y,
		       stop_points,
		       stop_points
		       + sizeof(stop_points)/sizeof(stop_points[0]));
}

template <typename Interval>
Constraint_System
Box<Interval>::constraints() const {
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
      Coefficient n;
      Coefficient d;
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

/*! \relates Parma_Polyhedra_Library::Box */
template <typename Interval>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Box<Interval>& box) {
  if (box.is_empty()) {
    s << "empty";
    return s;
  }
  const dimension_type dimension = box.space_dimension();
  for (dimension_type k = 0; k < dimension; ++k) {
    bool closed = false;
    Coefficient n;
    Coefficient d;
    if (box.get_lower_bound(k, closed, n, d)) {
      s << (closed ? "[" : "(")
	<< n;
      if (d != 1)
	s << "/" << d;
      s << ", ";
    }
    else
      s << "(-inf, ";
    if (box.get_upper_bound(k, closed, n, d)) {
      s << n;
      if (d != 1)
	s << "/" << d;
      s << (closed ? "]" : ")");
    }
    else
      s << "+inf)";
    s << "\n";
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_templates_hh)
