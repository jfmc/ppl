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

#include <algorithm>
#include <cassert>

namespace Parma_Polyhedra_Library {

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
PowerSet<CS>::push_back(const CS& y) {
  sequence.push_back(y);
}

template <typename CS>
void
PowerSet<CS>::pop_back() {
  sequence.pop_back();
}

template <typename CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::erase(iterator first, iterator last) {
  return sequence.erase(first, last);
}

template <typename CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::erase(iterator position) {
  return sequence.erase(position);
}

template <typename CS>
PowerSet<CS>::PowerSet(const PowerSet<CS>& y)
  : sequence(y.sequence) {
}

template <typename CS>
PowerSet<CS>&
PowerSet<CS>::operator=(const PowerSet<CS>& y) {
  sequence = y.sequence;
  return *this;
}

template <typename CS>
PowerSet<CS>::PowerSet() {
}

template <typename CS>
void
PowerSet<CS>::omega_reduce() {
  for (iterator xi = begin(), xin = xi; xi != end(); xi = xin) {
    ++xin;
    const CS& xv = *xi;
    for (iterator yi = begin(), yin = yi; yi != end(); yi = yin) {
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
PowerSet<CS>::is_omega_reduced() const {
  for (const_iterator sbegin = begin(), send = end(),
	 xi = sbegin; xi != send; ++xi) {
    const CS& xv = *xi;
    for (const_iterator yi = sbegin; yi != send; ++yi) {
      if (xi == yi)
	continue;
      const CS& yv = *yi;
      if (xv.definitely_entails(yv) || yv.definitely_entails(xv))
	return false;
    }
  }
  return true;
}

template <typename CS>
PowerSet<CS>&
PowerSet<CS>::add_disjunct(const CS& d) {
  if (!d.is_bottom()) {
    sequence.push_back(d);
    omega_reduce();
  }
  return *this;
}

template <typename CS>
bool
PowerSet<CS>::definitely_entails(const PowerSet<CS>& y) const {
  const PowerSet<CS>& x = *this;
  bool found = true;
  for (const_iterator xi = x.begin(),
	 x_end = x.end(); found && xi != x_end; ++xi) {
    found = false;
    for (const_iterator yi = y.begin(),
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
    for (xi = x.begin(), ++xi; xi != x.end(); ++xi)
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
  z.omega_reduce();
  return z;
}

template <typename CS>
void
PowerSet<CS>::meet_assign(const PowerSet<CS>& y) {
  const PowerSet<CS>& x = *this;
  Sequence new_sequence;
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi)
    for (const_iterator yi = y.begin(), yend = y.end(); yi != yend; ++yi) {
      CS zi = *xi;
      zi.meet_assign(*yi);
      if (!zi.is_bottom())
	new_sequence.push_back(zi);
    }
  std::swap(sequence, new_sequence);
  omega_reduce();
}

template <typename CS>
void
PowerSet<CS>::upper_bound_assign(const PowerSet<CS>& y) {
  std::copy(y.begin(), y.end(), back_inserter(sequence));
  omega_reduce();
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::PowerSet */
template <typename CS>
std::ostream&
operator<<(std::ostream& s, const PowerSet<CS>& x) {
  if (x.is_bottom())
    s << "false";
  else if (x.is_top())
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
bool
PowerSet<CS>::OK() const {
  for (const_iterator i = begin(), send = end(); i != send; ++i) {
    if (!i->OK())
      return false;
    if (i->is_bottom())
      return false;
  }
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_PowerSet_inlines_hh)
