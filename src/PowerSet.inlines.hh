/* PowerSet class implementation: inline functions.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_PowerSet_inlines_hh
#define PPL_PowerSet_inlines_hh 1

#include "ConSys.defs.hh"
#include "ConSys.inlines.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

template <typename CS>
dimension_type
PowerSet<CS>::space_dimension() const {
  return space_dim;
}

template <typename CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::begin() {
  return sequence.begin();
}

template <typename CS>
typename PowerSet<CS>::const_iterator
PowerSet<CS>::begin() const {
  return sequence.begin();
}

template <typename CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::end() {
  return sequence.end();
}

template <typename CS>
typename PowerSet<CS>::const_iterator
PowerSet<CS>::end() const {
  return sequence.end();
}

template <typename CS>
typename PowerSet<CS>::reverse_iterator
PowerSet<CS>::rbegin() {
  return sequence.rbegin();
}

template <typename CS>
typename PowerSet<CS>::const_reverse_iterator
PowerSet<CS>::rbegin() const {
  return sequence.rbegin();
}

template <typename CS>
typename PowerSet<CS>::reverse_iterator
PowerSet<CS>::rend() {
  return sequence.rend();
}

template <typename CS>
typename PowerSet<CS>::const_reverse_iterator
PowerSet<CS>::rend() const {
  return sequence.rend();
}

template <typename CS>
typename PowerSet<CS>::size_type
PowerSet<CS>::size() const {
  return sequence.size();
}

template <typename CS>
void
PowerSet<CS>::pop_back() {
  sequence.pop_back();
}

template <typename CS>
PowerSet<CS>::PowerSet(dimension_type num_dimensions, bool universe)
  : space_dim(num_dimensions) {
  if (universe)
    sequence.push_back(CS(num_dimensions, true));
}

template <typename CS>
PowerSet<CS>::PowerSet(const PowerSet<CS>& y)
  : sequence(y.sequence), space_dim(y.space_dim) {
}

template <typename CS>
PowerSet<CS>&
PowerSet<CS>:: operator=(const PowerSet<CS>& y) {
  sequence = y.sequence;
  space_dim = y.space_dim;
  return *this;
}

template <typename CS>
PowerSet<CS>::PowerSet(const ConSys& cs)
  : space_dim(cs.space_dimension()) {
  sequence.push_back(CS(cs));
}

template <typename CS>
void
PowerSet<CS>::omega_reduction() {
  for (iterator xi = begin(), xin = xi; xi != end(); xi = xin) {
    ++xin;
    const CS& xv = *xi;
    for (iterator yi = xin, yin = yi; yi != end(); yi = yin) {
      ++yin;
      if (xi == yi)
	continue;
      const CS& yv = *yi;
      if (yv.definitely_entails(xv)) {
	if (yi == xin)
	  ++xin;
	sequence.erase(yi);
      }
      else if (xv.definitely_entails(yv)) {
	sequence.erase(xi);
	break;
      }
    }
  }
  assert(OK());
}

template <typename CS>
bool
PowerSet<CS>::definitely_contains(const CS& y) const {
  for (const_iterator xi = begin(), x_end = end(); xi != x_end; ++xi)
    if (xi->is_definitely_equivalent_to(y))
      return true;
  return false;
}

template <typename CS>
PowerSet<CS>&
PowerSet<CS>::add_disjunct(const CS& d) {
  if (!c.is_bottom()) {
    sequence.push_back(d);
    omega_reduction();
  }
  return *this;
}

template <typename CS>
bool
PowerSet<CS>::definitely_entails(const PowerSet<CS>& y) const {
  const PowerSet<CS>& x = *this;
  bool found = true;
  for (typename PowerSet<CS>::const_iterator xi = x.begin(),
	 x_end = x.end(); found && xi != x_end; ++xi) {
    found = false;
    for (typename PowerSet<CS>::const_iterator yi = y.begin(),
	   yend = y.end(); !found && yi != yend; ++yi)
      found = (*xi).definitely_entails(*yi);
  }
  return found;
}

/*! \relates PowerSet */
template <typename CS>
inline
bool operator==(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  return (x.size() == y.size() && equal(x.begin(), x.end(), y.begin()));
}

/*! \relates PowerSet */
template <typename CS>
inline
bool operator!=(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  return !(x == y);
}

// Simple tests

template <typename CS>
inline bool
PowerSet<CS>::is_top() const {
  // FIXME: this may fail for top powersets containing redundant elements.
  return size() == 1 && begin()->is_top();
}

template <typename CS>
inline bool
PowerSet<CS>::is_bottom() const {
  return sequence.empty();
}

// Projection

/*! \relates PowerSet */
template <typename CS>
CS
project(const PowerSet<CS>& x) {
  CS ret;
  typename PowerSet<CS>::const_iterator xi;

  if (x.size() == 0)
    ret.bottom();
  else {
    ret = *(x.begin());
    for (xi=x.begin(), ++xi; xi != x.end(); ++xi)
      ret.upper_bound_assign(project(*xi));
  }
  return ret;
}

// Meet operators

/*! \relates PowerSet */
template <typename CS>
PowerSet<CS>
operator*(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  PowerSet<CS> z;
  typename PowerSet<CS>::const_iterator xi, yi;
  for (xi = x.begin(); xi != x.end(); ++xi) {
    for (yi = y.begin(); yi != y.end(); ++yi) {
      CS zi = *xi * *yi;
      if (!zi.is_bottom())
	z.sequence.push_back(zi);
    }
  }
  z.omega_reduction();
  return z;
}

template <typename CS>
void
PowerSet<CS>::meet_assign(const PowerSet<CS>& y) {
  // We let others throw an exception if space_dim != y.space_dim.
  PowerSet<CS> z(space_dim);
  const PowerSet<CS>& x = *this;
  for (typename PowerSet<CS>::const_iterator xi = x.begin(),
	 x_end = x.end(); xi != x_end; ++xi) {
    for (typename PowerSet<CS>::const_iterator yi = y.begin(),
	   yend = y.end(); yi != yend; ++yi) {
      CS zi = *xi;
      zi.meet_assign(*yi);
      if (!zi.is_bottom())
	z.sequence.push_back(zi);
    }
  }
  z.omega_reduction();
  std::swap(*this, z);
}

template <typename CS>
void
PowerSet<CS>::concatenate_assign(const PowerSet<CS>& y) {
  space_dim += y.space_dim;
  Sequence new_sequence;
  const PowerSet<CS>& x = *this;
  for (typename PowerSet<CS>::const_iterator xi = x.begin(),
	 x_end = x.end(); xi != x_end; ++xi) {
    for (typename PowerSet<CS>::const_iterator yi = y.begin(),
	   yend = y.end(); yi != yend; ++yi) {
      CS zi = *xi;
      zi.concatenate_assign(*yi);
      assert(!zi.is_bottom());
      new_sequence.push_back(zi);
    }
  }
  std::swap(sequence, new_sequence);
  omega_reduction();
}

// Join operator

template <typename CS>
void
PowerSet<CS>::upper_bound_assign(const PowerSet<CS>& y) {
  std::copy(y.begin(), y.end(), back_inserter(sequence));
  omega_reduction();
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::PowerSet */
template <typename CS>
std::ostream&
operator<<(std::ostream& s, const PowerSet<CS>& x) {
  if (x.is_bottom())
    s << "false";
  else if ((x.size() == 1) && (*(x.begin())).is_top())
    s << "true";
  else {
    s << "{ ";
    typename PowerSet<CS>::const_iterator i = x.begin();
    typename PowerSet<CS>::const_iterator x_end = x.end();
    while (i != x_end) {
      s << *i++;
      if (i != x_end)
	s << ", ";
    }
    s << " }";
  }
  return s;
}

} // namespace IO_Operators

template <typename CS>
void
PowerSet<CS>::add_constraint(const Constraint& c) {
  for (typename PowerSet<CS>::iterator xi = begin(),
	 xin = xi, x_end = end(); xi != x_end; xi = xin) {
    ++xin;
    CS& xv = *xi;
    xv.add_constraint(c);
    if (xv.is_bottom()) {
      erase(xi);
      x_end = end();
    }
  }
  omega_reduction();
}

template <typename CS>
void
PowerSet<CS>::add_constraints(ConSys& cs) {
  typename PowerSet<CS>::iterator xi = begin();
  if (size() == 1) {
    CS& xv = *xi;
    xv.add_constraints(cs);
    if (xv.is_bottom())
      erase(xi);
  }
  else
    for (typename PowerSet<CS>::iterator xin = xi,
	   x_end = end(); xi != x_end; xi = xin) {
      ++xin;
      CS& xv = *xi;
      ConSys cs_copy = cs;
      xv.add_constraints(cs_copy);
      if (xv.is_bottom()) {
	erase(xi);
	x_end = end();
      }
    }
  omega_reduction();
}

template <typename CS>
void
PowerSet<CS>::add_dimensions_and_embed(dimension_type m) {
  space_dim += m;
  for (typename PowerSet<CS>::iterator i = begin(),
	 send = end(); i != send; ++i)
    i->add_dimensions_and_embed(m);
  omega_reduction();
}

template <typename CS>
void
PowerSet<CS>::add_dimensions_and_project(dimension_type m) {
  space_dim += m;
  for (typename PowerSet<CS>::iterator i = begin(),
	 send = end(); i != send; ++i)
    i->add_dimensions_and_project(m);
  omega_reduction();
}

template <typename CS>
void
PowerSet<CS>::remove_dimensions(const Variables_Set& to_be_removed) {
  // FIXME: set space_dim
  for (typename PowerSet<CS>::iterator i = begin(),
	 send = end(); i != send; ++i)
    i->remove_dimensions(to_be_removed);
  omega_reduction();
}

template <typename CS>
void
PowerSet<CS>::remove_higher_dimensions(dimension_type new_dimension) {
  space_dim = new_dimension;
  for (typename PowerSet<CS>::iterator i = begin(),
	 send = end(); i != send; ++i)
    i->remove_higher_dimensions(new_dimension);
  omega_reduction();
}

template <typename CS>
template <typename PartialFunction>
void
PowerSet<CS>::map_dimensions(const PartialFunction& pfunc) {
  if (is_bottom()) {
    dimension_type n = 0;
    for (dimension_type i = space_dim; i-- > 0; ) {
      dimension_type new_i;
      if (pfunc.maps(i, new_i))
	++n;
    }
    space_dim = n;
  }
  else {
    iterator sbegin = begin();
    for (iterator i = sbegin, send = end(); i != send; ++i)
      i->map_dimensions(pfunc);
    space_dim = sbegin->space_dimension();
    omega_reduction();
  }
  assert(OK());
}

template <typename CS>
void
PowerSet<CS>::H79_extrapolation_assign(const PowerSet& /* y */) {
}

template <typename CS>
void
PowerSet<CS>::limited_H79_extrapolation_assign(const PowerSet& y,
					       const ConSys& cs) {
  std::deque<iterator> possibly_new;
  for (iterator xi = begin(), xend = end(); xi != xend; ++xi)
    if (!y.definitely_contains(*xi))
      possibly_new.push_back(xi);

  if (possibly_new.empty())
    return;

  // Heuristics: less precise elements are later in the sequence.
  typename std::deque<iterator>::const_reverse_iterator ni
    = possibly_new.rbegin();
  typename std::deque<iterator>::const_reverse_iterator nend
    = possibly_new.rend();
  const iterator& zi = *ni;
  CS new_upper_bound(*zi);
  erase(zi);
  for (++ni; ni != nend; ++ni) {
    const iterator& xi = *ni;
    assert(xi->OK());
    new_upper_bound.upper_bound_assign(*xi);
    erase(xi);
  }

  if (sequence.empty()) {
    // Heuristics again: less precise elements are later in the sequence.
    const_reverse_iterator yi = y.rbegin();
    const_reverse_iterator yend = y.rend();
    CS old_upper_bound(*yi);
    for (++yi; yi != yend; ++yi)
      old_upper_bound.upper_bound_assign(*yi);

    assert(old_upper_bound.definitely_entails(new_upper_bound));

    new_upper_bound.limited_H79_extrapolation_assign(old_upper_bound, cs);
  }
  else{
    // Heuristics again: less precise elements are later in the sequence.
    const_reverse_iterator xi = rbegin();
    const_reverse_iterator xend = rend();
    CS old_upper_bound(*xi);
    for (++xi; xi != xend; ++xi)
      old_upper_bound.upper_bound_assign(*xi);

    new_upper_bound.upper_bound_assign(old_upper_bound);

    new_upper_bound.limited_H79_extrapolation_assign(old_upper_bound, cs);
  }

  sequence.push_back(new_upper_bound);

  omega_reduction();
}

template <typename CS>
bool
PowerSet<CS>::OK() const {
  for (typename PowerSet<CS>::const_iterator i = begin(),
	 send = end(); i != send; ++i) {
    if (!i->OK()
	|| i->space_dimension() != space_dim)
      return false;
    if (i->is_bottom())
      return false;
  }
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_PowerSet_inlines_hh)
