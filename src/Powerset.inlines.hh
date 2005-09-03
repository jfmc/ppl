/* Powerset class implementation: inline functions.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Powerset_inlines_hh
#define PPL_Powerset_inlines_hh 1

#include <algorithm>
#include <cassert>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename CS>
inline
Powerset<CS>::iterator::iterator()
  : base() {
}

template <typename CS>
inline 
Powerset<CS>::iterator::iterator(const iterator& y)
  : base(y.base) {
}

template <typename CS>
inline 
Powerset<CS>::iterator::iterator(const Base& b)
  : base(b) {
}

template <typename CS>
inline typename Powerset<CS>::iterator::reference
Powerset<CS>::iterator::operator*() const {
  return *base;
}

template <typename CS>
inline typename Powerset<CS>::iterator::pointer
Powerset<CS>::iterator::operator->() const {
  return &*base;
}

template <typename CS>
inline typename Powerset<CS>::iterator&
Powerset<CS>::iterator::operator++() {
  ++base;
  return *this;
}

template <typename CS>
inline typename Powerset<CS>::iterator
Powerset<CS>::iterator::operator++(int) {
  iterator tmp = *this;
  operator++();
  return tmp;
}

template <typename CS>
inline typename Powerset<CS>::iterator&
Powerset<CS>::iterator::operator--() {
  --base;
  return *this;
}

template <typename CS>
inline typename Powerset<CS>::iterator
Powerset<CS>::iterator::operator--(int) {
  iterator tmp = *this;
  operator--();
  return tmp;
}

template <typename CS>
inline bool
Powerset<CS>::iterator::operator==(const iterator& y) const {
  return base == y.base;
}

template <typename CS>
inline bool
Powerset<CS>::iterator::operator!=(const iterator& y) const {
  return !operator==(y);
}

template <typename CS>
inline
Powerset<CS>::const_iterator::const_iterator()
  : base() {
}

template <typename CS>
inline 
Powerset<CS>::const_iterator::const_iterator(const const_iterator& y)
  : base(y.base) {
}

template <typename CS>
inline 
Powerset<CS>::const_iterator::const_iterator(const Base& b)
  : base(b) {
}

template <typename CS>
inline typename Powerset<CS>::const_iterator::reference
Powerset<CS>::const_iterator::operator*() const {
  return *base;
}

template <typename CS>
inline typename Powerset<CS>::const_iterator::pointer
Powerset<CS>::const_iterator::operator->() const {
  return &*base;
}

template <typename CS>
inline typename Powerset<CS>::const_iterator&
Powerset<CS>::const_iterator::operator++() {
  ++base;
  return *this;
}

template <typename CS>
inline typename Powerset<CS>::const_iterator
Powerset<CS>::const_iterator::operator++(int) {
  const_iterator tmp = *this;
  operator++();
  return tmp;
}

template <typename CS>
inline typename Powerset<CS>::const_iterator&
Powerset<CS>::const_iterator::operator--() {
  --base;
  return *this;
}

template <typename CS>
inline typename Powerset<CS>::const_iterator
Powerset<CS>::const_iterator::operator--(int) {
  const_iterator tmp = *this;
  operator--();
  return tmp;
}

template <typename CS>
inline bool
Powerset<CS>::const_iterator::operator==(const const_iterator& y) const {
  return base == y.base;
}

template <typename CS>
inline bool
Powerset<CS>::const_iterator::operator!=(const const_iterator& y) const {
  return !operator==(y);
}

template <typename CS>
inline 
Powerset<CS>::const_iterator::const_iterator(const iterator& y)
  : base(y.base) {
}

/*! \relates Powerset::const_iterator */
template <typename CS>
inline bool
operator==(const typename Powerset<CS>::iterator& x,
	   const typename Powerset<CS>::const_iterator& y) {
  return Powerset<CS>::const_iterator(x).operator==(y);
}

/*! \relates Powerset::const_iterator */
template <typename CS>
inline bool
operator!=(const typename Powerset<CS>::iterator& x,
	   const typename Powerset<CS>::const_iterator& y) {
  return !(x == y);
}

template <typename CS>
inline typename Powerset<CS>::iterator
Powerset<CS>::begin() {
  return sequence.begin();
}

template <typename CS>
inline typename Powerset<CS>::iterator
Powerset<CS>::end() {
  return sequence.end();
}

template <typename CS>
inline typename Powerset<CS>::const_iterator
Powerset<CS>::begin() const {
  return sequence.begin();
}

template <typename CS>
inline typename Powerset<CS>::const_iterator
Powerset<CS>::end() const {
  return sequence.end();
}

template <typename CS>
inline typename Powerset<CS>::reverse_iterator
Powerset<CS>::rbegin() {
  return reverse_iterator(end());
}

template <typename CS>
inline typename Powerset<CS>::reverse_iterator
Powerset<CS>::rend() {
  return reverse_iterator(begin());
}

template <typename CS>
inline typename Powerset<CS>::const_reverse_iterator
Powerset<CS>::rbegin() const {
  return const_reverse_iterator(end());
}

template <typename CS>
inline typename Powerset<CS>::const_reverse_iterator
Powerset<CS>::rend() const {
  return const_reverse_iterator(begin());
}

template <typename CS>
inline typename Powerset<CS>::size_type
Powerset<CS>::size() const {
  return sequence.size();
}

template <typename CS>
inline bool
Powerset<CS>::empty() const {
  return sequence.empty();
}

template <typename CS>
inline typename Powerset<CS>::iterator
Powerset<CS>::drop_disjunct(iterator position) {
  return sequence.erase(position.base);
}

template <typename CS>
inline void
Powerset<CS>::drop_disjuncts(iterator first, iterator last) {
  sequence.erase(first.base, last.base);
}

template <typename CS>
inline void
Powerset<CS>::clear() {
  sequence.clear();
}

template <typename CS>
inline
Powerset<CS>::Powerset(const Powerset& y)
  : sequence(y.sequence), reduced(y.reduced) {
}

template <typename CS>
inline Powerset<CS>&
Powerset<CS>::operator=(const Powerset& y) {
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
inline
Powerset<CS>::Powerset()
  : sequence(), reduced(true) {
}

template <typename CS>
inline
Powerset<CS>::Powerset(const CS& d)
  : sequence(), reduced(true) {
  if (!d.is_bottom())
    sequence.push_back(d);
  assert(OK());
}

template <typename CS>
inline
Powerset<CS>::~Powerset() {
}

template <typename CS>
void
Powerset<CS>::collapse(const Sequence_iterator sink) {
  assert(sink != sequence.end());
  CS& d = *sink;
  iterator x_sink = sink;
  iterator next_x_sink = x_sink;
  ++next_x_sink;
  iterator x_end = end();
  for (const_iterator xi = next_x_sink; xi != x_end; ++xi)
    d.upper_bound_assign(*xi);
  // Drop the surplus disjuncts.
  drop_disjuncts(next_x_sink, x_end);

  // Ensure omega-reduction.
  for (iterator xi = begin(); xi != x_sink; )
    if (xi->definitely_entails(d))
      xi = drop_disjunct(xi);
    else
      ++xi;

  assert(OK());
}

template <typename CS>
void
Powerset<CS>::omega_reduce() const {
  if (reduced)
    return;

  Powerset& x = const_cast<Powerset&>(*this);
  // First remove all bottom elements.
  for (iterator xi = x.begin(), x_end = x.end(); xi != x_end; )
    if (xi->is_bottom())
      xi = x.drop_disjunct(xi);
    else
      ++xi;
  // Then remove non-maximal elements.
  for (iterator xi = x.begin(); xi != x.end(); ) {
    const CS& xv = *xi;
    bool dropping_xi = false;
    for (iterator yi = x.begin(); yi != x.end(); )
      if (xi == yi)
	++yi;
      else {
	const CS& yv = *yi;
	if (yv.definitely_entails(xv))
	  yi = x.drop_disjunct(yi);
	else if (xv.definitely_entails(yv)) {
	  dropping_xi = true;
	  break;
	}
	else
	  ++yi;
      }
    if (dropping_xi)
      xi = x.drop_disjunct(xi);
    else
      ++xi;
    if (abandon_expensive_computations && xi != x.end()) {
      // Hurry up!
      x.collapse(xi.base);
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
    // Let `i' point to the last disjunct that will survive.
    iterator i = begin();
    std::advance(i, max_disjuncts-1);
    // This disjunct will be assigned an upper-bound of itself and of
    // all the disjuncts that follow.
    collapse(i.base);
  }
  assert(OK());
  assert(is_omega_reduced());
}

template <typename CS>
bool
Powerset<CS>::check_omega_reduced() const {
  for (const_iterator x_begin = begin(), x_end = end(),
	 xi = x_begin; xi != x_end; ++xi) {
    const CS& xv = *xi;
    if (xv.is_bottom())
      return false;
    for (const_iterator yi = x_begin; yi != x_end; ++yi) {
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
typename Powerset<CS>::iterator
Powerset<CS>::add_non_bottom_disjunct(const CS& d,
				      iterator first,
				      iterator last) {
  for (iterator xi = first; xi != last; ) {
    const CS& xv = *xi;
    if (d.definitely_entails(xv))
      return first;
    else if (xv.definitely_entails(d)) {
      if (xi == first)
	++first;
      xi = drop_disjunct(xi);
    }
    else
      ++xi;
  }
  sequence.push_back(d);
  return first;
}

template <typename CS>
inline void
Powerset<CS>::add_non_bottom_disjunct(const CS& d) {
  assert(!d.is_bottom());
  add_non_bottom_disjunct(d, begin(), end());
}

template <typename CS>
inline void
Powerset<CS>::add_disjunct(const CS& d) {
  if (!d.is_bottom())
    add_non_bottom_disjunct(d);
}

template <typename CS>
bool
Powerset<CS>::definitely_entails(const Powerset& y) const {
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
  x.omega_reduce();
  y.omega_reduce();
  if (x.size() != y.size())
    return false;
  // Take a copy of `y' and work with it.
  Powerset<CS> yy = y;
  for (typename Powerset<CS>::const_iterator xi = x.begin(),
	 x_end = x.end(); xi != x_end; ++xi) {
    typename Powerset<CS>::iterator yyi = yy.begin();
    typename Powerset<CS>::iterator yy_end = yy.end();
    yyi = std::find(yyi, yy_end, *xi);
    if (yyi == yy_end)
      return false;
    else
      yy.drop_disjunct(yyi);
  }
  return true;
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
  const_iterator xi = begin();
  const_iterator x_end = end();
  return xi != x_end && xi->is_top() && ++xi == x_end;
}

template <typename CS>
inline bool
Powerset<CS>::is_bottom() const {
  // Must perform omega-reduction for correctness.
  omega_reduce();
  return empty();
}

template <typename CS>
inline void
Powerset<CS>::collapse() {
  if (!empty())
    collapse(sequence.begin());
}

template <typename CS>
template <typename Binary_Operator_Assign>
void
Powerset<CS>::pairwise_apply_assign(const Powerset& y,
				    Binary_Operator_Assign op_assign) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  Sequence new_sequence;
  for (const_iterator xi = begin(), x_end = end(),
	 y_begin = y.begin(), y_end = y.end(); xi != x_end; ++xi)
    for (const_iterator yi = y_begin; yi != y_end; ++yi) {
      CS zi = *xi;
      op_assign(zi, *yi);
      if (!zi.is_bottom())
	new_sequence.push_back(zi);
    }
  // Put the new sequence in place.
  std::swap(sequence, new_sequence);
  reduced = false;
}

template <typename CS>
inline void
Powerset<CS>::meet_assign(const Powerset& y) {
  pairwise_apply_assign(y, std::mem_fun_ref(&CS::meet_assign));
}

template <typename CS>
void
Powerset<CS>::least_upper_bound_assign(const Powerset& y) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  iterator old_begin = begin();
  iterator old_end = end();
  for (const_iterator i = y.begin(), y_end = y.end(); i != y_end; ++i)
    old_begin = add_non_bottom_disjunct(*i, old_begin, old_end);
}

template <typename CS>
inline void
Powerset<CS>::upper_bound_assign(const Powerset& y) {
  least_upper_bound_assign(y);
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
  else
    for (typename Powerset<CS>::const_iterator i = x.begin(),
	   x_end = x.end(); i != x_end; ++i) {
      s << "{ " << *i << " }";
      if (i != x_end)
	s << ", ";
    }
  return s;
}

} // namespace IO_Operators

template <typename CS>
memory_size_type
Powerset<CS>::external_memory_in_bytes() const {
  memory_size_type bytes = 0;
  for (const_iterator xi = begin(), x_end = end(); xi != x_end; ++xi) {
    bytes += xi->total_memory_in_bytes();
    // We assume there is at least a forward and a backward link, and
    // that the pointers implementing them are at least the size of
    // pointers to `CS'.
    bytes += 2*sizeof(CS*);
  }
  return bytes;
}

template <typename CS>
inline memory_size_type
Powerset<CS>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename CS>
bool
Powerset<CS>::OK(const bool disallow_bottom) const {
  for (const_iterator xi = begin(), x_end = end(); xi != x_end; ++xi) {
    if (!xi->OK())
      return false;
    if (disallow_bottom && xi->is_bottom()) {
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
