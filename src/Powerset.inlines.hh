/* Powerset class implementation: inline functions.
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

#ifndef PPL_Powerset_inlines_hh
#define PPL_Powerset_inlines_hh 1

#include <algorithm>
#include <cassert>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename CS>
typename Powerset<CS>::iterator
Powerset<CS>::begin() {
  return sequence.begin();
}

template <typename CS>
typename Powerset<CS>::const_iterator
Powerset<CS>::begin() const {
  return sequence.begin();
}

template <typename CS>
typename Powerset<CS>::iterator
Powerset<CS>::end() {
  return sequence.end();
}

template <typename CS>
typename Powerset<CS>::const_iterator
Powerset<CS>::end() const {
  return sequence.end();
}

template <typename CS>
typename Powerset<CS>::reverse_iterator
Powerset<CS>::rbegin() {
  return sequence.rbegin();
}

template <typename CS>
typename Powerset<CS>::const_reverse_iterator
Powerset<CS>::rbegin() const {
  return sequence.rbegin();
}

template <typename CS>
typename Powerset<CS>::reverse_iterator
Powerset<CS>::rend() {
  return sequence.rend();
}

template <typename CS>
typename Powerset<CS>::const_reverse_iterator
Powerset<CS>::rend() const {
  return sequence.rend();
}

template <typename CS>
typename Powerset<CS>::size_type
Powerset<CS>::size() const {
  return sequence.size();
}

template <typename CS>
bool
Powerset<CS>::empty() const {
  return sequence.empty();
}

template <typename CS>
void
Powerset<CS>::push_back(const CS& y) {
  sequence.push_back(y);
}

template <typename CS>
void
Powerset<CS>::pop_back() {
  sequence.pop_back();
}

template <typename CS>
typename Powerset<CS>::iterator
Powerset<CS>::erase(iterator first, iterator last) {
  return sequence.erase(first, last);
}

template <typename CS>
typename Powerset<CS>::iterator
Powerset<CS>::erase(iterator position) {
  return sequence.erase(position);
}

template <typename CS>
void
Powerset<CS>::clear() {
  sequence.clear();
}

template <typename CS>
Powerset<CS>::Powerset(const Powerset<CS>& y)
  : sequence(y.sequence), reduced(y.reduced) {
}

template <typename CS>
Powerset<CS>&
Powerset<CS>::operator=(const Powerset<CS>& y) {
  sequence = y.sequence;
  reduced = y.reduced;
  return *this;
}

template <typename CS>
inline void
Powerset<CS>::swap(Powerset& y) {
  std::swap(sequence, y.sequence);
  std::swap(reduced, y.reduced);
}

template <typename CS>
Powerset<CS>::Powerset()
  : sequence(), reduced(true) {
}

template <typename CS>
void
Powerset<CS>::collapse(const iterator sink) {
  assert(sink != end());
  // Collapse onto the disjunct pointed to by `sink' all the disjuncts
  // that follow.
  CS& d = *sink;
  iterator j = sink;
  iterator send = end();
  for (++j; j != send; ++j)
    d.upper_bound_assign(*j);

  // Erase the surplus disjuncts.
  j = sink;
  erase(++j, send);

  // Ensure omega-reduction.
  for (iterator k = begin(); k != sink; ) {
    if (k->definitely_entails(d))
      k = erase(k);
    else
      ++k;
  }
  assert(OK());
}

template <typename CS>
void
Powerset<CS>::omega_reduce() const {
  if (reduced)
    return;

  Powerset& ps = const_cast<Powerset&>(*this);
  // First remove all bottom elements.
  for (iterator xi = ps.begin(); xi != ps.end(); )
    if (xi->is_bottom())
      xi = ps.erase(xi);
    else
      ++xi;
  // Then remove non-maximal elements.
  for (iterator xi = ps.begin(); xi != ps.end(); ) {
    const CS& xv = *xi;
    bool erasing_xi = false;
    for (iterator yi = ps.begin(); yi != ps.end(); )
      if (xi == yi)
	++yi;
      else {
	const CS& yv = *yi;
	if (yv.definitely_entails(xv))
	  yi = ps.erase(yi);
	else if (xv.definitely_entails(yv)) {
	  erasing_xi = true;
	  break;
	}
	else
	  ++yi;
      }
    if (erasing_xi)
      xi = ps.erase(xi);
    else
      ++xi;
    if (abandon_expensive_computations && xi != ps.end()) {
      // Hurry up!
      ps.collapse(xi);
      break;
    }
  }
  reduced = true;
  assert(OK());
}

template <typename CS>
void
Powerset<CS>::collapse(const unsigned max_disjuncts) {
  assert(max_disjuncts > 0);
  // Omega-reduce before counting the number of disjuncts.
  omega_reduce();
  size_type n = size();
  if (n > max_disjuncts) {
    iterator i = begin();
    // Move `i' to the last disjunct that will survive.
    for (unsigned m = max_disjuncts-1; m-- > 0; )
      ++i;

    // This disjunct will be assigned an upper-bound of itself and of
    // all the disjuncts that follow.
    collapse(i);
  }
  assert(OK());
  assert(is_omega_reduced());
}

template <typename CS>
bool
Powerset<CS>::check_omega_reduced() const {
  for (const_iterator sbegin = begin(), send = end(),
	 xi = sbegin; xi != send; ++xi) {
    const CS& xv = *xi;
    if (xv.is_bottom())
      return false;
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
bool
Powerset<CS>::is_omega_reduced() const {
  if (!reduced && check_omega_reduced())
    reduced = true;
  return reduced;
}

template <typename CS>
void
Powerset<CS>::add_non_bottom_disjunct(Sequence& s,
				      const CS& d,
				      iterator& first,
				      iterator last) {
  for (iterator xi = first; xi != last; ) {
    const CS& xv = *xi;
    if (d.definitely_entails(xv))
      return;
    else if (xv.definitely_entails(d)) {
      if (xi == first)
	++first;
      xi = s.erase(xi);
    }
    else
      ++xi;
  }
  s.push_back(d);
}

template <typename CS>
void
Powerset<CS>::add_non_bottom_disjunct(Sequence& s, const CS& d) {
  assert(!d.is_bottom());
  iterator s_begin = s.begin();
  iterator s_end = s.end();
  add_non_bottom_disjunct(s, d, s_begin, s_end);
}

template <typename CS>
void
Powerset<CS>::add_disjunct(const CS& d) {
  if (!d.is_bottom())
    add_non_bottom_disjunct(sequence, d);
}

template <typename CS>
bool
Powerset<CS>::definitely_entails(const Powerset<CS>& y) const {
  const Powerset<CS>& x = *this;
  bool found = true;
  for (const_iterator xi = x.begin(),
	 x_end = x.end(); found && xi != x_end; ++xi) {
    found = false;
    for (const_iterator yi = y.begin(),
	   y_end = y.end(); !found && yi != y_end; ++yi)
      found = (*xi).definitely_entails(*yi);
  }
  return found;
}

/*! \relates Powerset */
template <typename CS>
inline
bool operator==(const Powerset<CS>& x, const Powerset<CS>& y) {
  return (x.size() == y.size() && equal(x.begin(), x.end(), y.begin()));
}

/*! \relates Powerset */
template <typename CS>
inline
bool operator!=(const Powerset<CS>& x, const Powerset<CS>& y) {
  return !(x == y);
}

template <typename CS>
inline bool
Powerset<CS>::is_top() const {
  // Must perform omega-reduction for correctness.
  omega_reduce();
  const_iterator i = begin();
  const_iterator send = end();
  return i != send && i->is_top() && ++i == send;
}

template <typename CS>
inline bool
Powerset<CS>::is_bottom() const {
  // Must perform omega-reduction for correctness.
  omega_reduce();
  return sequence.empty();
}

template <typename CS>
inline void
Powerset<CS>::collapse() {
  if (!empty())
    collapse(begin());
}

template <typename CS>
void
Powerset<CS>::meet_assign(const Powerset<CS>& y) {
  const Powerset<CS>& x = *this;
  Sequence new_sequence;
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi)
    for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi) {
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
Powerset<CS>::upper_bound_assign(const Powerset<CS>& y) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  iterator sbegin = begin();
  iterator send = end();
  for (const_iterator i = y.begin(), y_end = y.end(); i != y_end; ++i)
    add_non_bottom_disjunct(sequence, *i, sbegin, send);
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename CS>
std::ostream&
operator<<(std::ostream& s, const Powerset<CS>& x) {
  if (x.is_bottom())
    s << "false";
  else if (x.is_top())
    s << "true";
  else {
    s << "{ ";
    typename Powerset<CS>::const_iterator i = x.begin();
    typename Powerset<CS>::const_iterator x_end = x.end();
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
Powerset<CS>::OK(const bool disallow_bottom) const {
  for (const_iterator i = begin(), send = end(); i != send; ++i) {
    if (!i->OK())
      return false;
    if (disallow_bottom && i->is_bottom()) {
#ifndef NDEBUG
      std::cerr << "Bottom element in powerset!"
		<< std::endl;
#endif
      return false;
    }
  }
  if (reduced && !check_omega_reduced()) {
#ifndef NDEBUG
    std::cerr << "Powerset claims to be reduced, but it is not!"
	      << std::endl;
#endif
    return false;
  }
  return true;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename CS>
inline void
swap(Parma_Polyhedra_Library::Powerset<CS>& x,
     Parma_Polyhedra_Library::Powerset<CS>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Powerset_inlines_hh)
