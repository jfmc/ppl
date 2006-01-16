/* Powerset class implementation: inline functions.
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

#ifndef PPL_Powerset_inlines_hh
#define PPL_Powerset_inlines_hh 1

#include <algorithm>
#include <cassert>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename D>
inline
Powerset<D>::omega_iterator::omega_iterator()
  : base() {
}

template <typename D>
inline
Powerset<D>::omega_iterator::omega_iterator(const omega_iterator& y)
  : base(y.base) {
}

template <typename D>
inline
Powerset<D>::omega_iterator::omega_iterator(const Base& b)
  : base(b) {
}

template <typename D>
inline typename Powerset<D>::omega_iterator::reference
Powerset<D>::omega_iterator::operator*() const {
  return *base;
}

template <typename D>
inline typename Powerset<D>::omega_iterator::pointer
Powerset<D>::omega_iterator::operator->() const {
  return &*base;
}

template <typename D>
inline typename Powerset<D>::omega_iterator&
Powerset<D>::omega_iterator::operator++() {
  ++base;
  return *this;
}

template <typename D>
inline typename Powerset<D>::omega_iterator
Powerset<D>::omega_iterator::operator++(int) {
  omega_iterator tmp = *this;
  operator++();
  return tmp;
}

template <typename D>
inline typename Powerset<D>::omega_iterator&
Powerset<D>::omega_iterator::operator--() {
  --base;
  return *this;
}

template <typename D>
inline typename Powerset<D>::omega_iterator
Powerset<D>::omega_iterator::operator--(int) {
  omega_iterator tmp = *this;
  operator--();
  return tmp;
}

template <typename D>
inline bool
Powerset<D>::omega_iterator::operator==(const omega_iterator& y) const {
  return base == y.base;
}

template <typename D>
inline bool
Powerset<D>::omega_iterator::operator!=(const omega_iterator& y) const {
  return !operator==(y);
}

template <typename D>
inline
Powerset<D>::omega_const_iterator::omega_const_iterator()
  : base() {
}

template <typename D>
inline
Powerset<D>
::omega_const_iterator::omega_const_iterator(const omega_const_iterator& y)
  : base(y.base) {
}

template <typename D>
inline
Powerset<D>::omega_const_iterator::omega_const_iterator(const Base& b)
  : base(b) {
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator::reference
Powerset<D>::omega_const_iterator::operator*() const {
  return *base;
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator::pointer
Powerset<D>::omega_const_iterator::operator->() const {
  return &*base;
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator&
Powerset<D>::omega_const_iterator::operator++() {
  ++base;
  return *this;
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator
Powerset<D>::omega_const_iterator::operator++(int) {
  omega_const_iterator tmp = *this;
  operator++();
  return tmp;
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator&
Powerset<D>::omega_const_iterator::operator--() {
  --base;
  return *this;
}

template <typename D>
inline typename Powerset<D>::omega_const_iterator
Powerset<D>::omega_const_iterator::operator--(int) {
  omega_const_iterator tmp = *this;
  operator--();
  return tmp;
}

template <typename D>
inline bool
Powerset<D>
::omega_const_iterator::operator==(const omega_const_iterator& y) const {
  return base == y.base;
}

template <typename D>
inline bool
Powerset<D>
::omega_const_iterator::operator!=(const omega_const_iterator& y) const {
  return !operator==(y);
}

template <typename D>
inline
Powerset<D>
::omega_const_iterator::omega_const_iterator(const omega_iterator& y)
  : base(y.base) {
}

/*! \relates Powerset::omega_const_iterator */
template <typename D>
inline bool
operator==(const typename Powerset<D>::omega_iterator& x,
	   const typename Powerset<D>::omega_const_iterator& y) {
  return Powerset<D>::omega_const_iterator(x).operator==(y);
}

/*! \relates Powerset::omega_const_iterator */
template <typename D>
inline bool
operator!=(const typename Powerset<D>::omega_iterator& x,
	   const typename Powerset<D>::omega_const_iterator& y) {
  return !(x == y);
}

template <typename D>
inline typename Powerset<D>::iterator
Powerset<D>::begin() {
  return sequence.begin();
}

template <typename D>
inline typename Powerset<D>::iterator
Powerset<D>::end() {
  return sequence.end();
}

template <typename D>
inline typename Powerset<D>::const_iterator
Powerset<D>::begin() const {
  return sequence.begin();
}

template <typename D>
inline typename Powerset<D>::const_iterator
Powerset<D>::end() const {
  return sequence.end();
}

template <typename D>
inline typename Powerset<D>::reverse_iterator
Powerset<D>::rbegin() {
  return reverse_iterator(end());
}

template <typename D>
inline typename Powerset<D>::reverse_iterator
Powerset<D>::rend() {
  return reverse_iterator(begin());
}

template <typename D>
inline typename Powerset<D>::const_reverse_iterator
Powerset<D>::rbegin() const {
  return const_reverse_iterator(end());
}

template <typename D>
inline typename Powerset<D>::const_reverse_iterator
Powerset<D>::rend() const {
  return const_reverse_iterator(begin());
}

template <typename D>
inline typename Powerset<D>::size_type
Powerset<D>::size() const {
  return sequence.size();
}

template <typename D>
inline bool
Powerset<D>::empty() const {
  return sequence.empty();
}

template <typename D>
inline typename Powerset<D>::iterator
Powerset<D>::drop_disjunct(iterator position) {
  return sequence.erase(position.base);
}

template <typename D>
inline void
Powerset<D>::drop_disjuncts(iterator first, iterator last) {
  sequence.erase(first.base, last.base);
}

template <typename D>
inline void
Powerset<D>::clear() {
  sequence.clear();
}

template <typename D>
inline
Powerset<D>::Powerset(const Powerset& y)
  : sequence(y.sequence), reduced(y.reduced) {
}

template <typename D>
inline Powerset<D>&
Powerset<D>::operator=(const Powerset& y) {
  sequence = y.sequence;
  reduced = y.reduced;
  return *this;
}

template <typename D>
inline void
Powerset<D>::swap(Powerset& y) {
  std::swap(sequence, y.sequence);
  std::swap(reduced, y.reduced);
}

template <typename D>
inline
Powerset<D>::Powerset()
  : sequence(), reduced(true) {
}

template <typename D>
inline
Powerset<D>::Powerset(const D& d)
  : sequence(), reduced(true) {
  if (!d.is_bottom())
    sequence.push_back(d);
  assert(OK());
}

template <typename D>
inline
Powerset<D>::~Powerset() {
}

template <typename D>
void
Powerset<D>::collapse(const Sequence_iterator sink) {
  assert(sink != sequence.end());
  D& d = *sink;
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

template <typename D>
void
Powerset<D>::omega_reduce() const {
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
    const D& xv = *xi;
    bool dropping_xi = false;
    for (iterator yi = x.begin(); yi != x.end(); )
      if (xi == yi)
	++yi;
      else {
	const D& yv = *yi;
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

template <typename D>
void
Powerset<D>::collapse(const unsigned max_disjuncts) {
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

template <typename D>
bool
Powerset<D>::check_omega_reduced() const {
  for (const_iterator x_begin = begin(), x_end = end(),
	 xi = x_begin; xi != x_end; ++xi) {
    const D& xv = *xi;
    if (xv.is_bottom())
      return false;
    for (const_iterator yi = x_begin; yi != x_end; ++yi) {
      if (xi == yi)
	continue;
      const D& yv = *yi;
      if (xv.definitely_entails(yv) || yv.definitely_entails(xv))
	return false;
    }
  }
  return true;
}

template <typename D>
bool
Powerset<D>::is_omega_reduced() const {
  if (!reduced && check_omega_reduced())
    reduced = true;
  return reduced;
}

template <typename D>
typename Powerset<D>::iterator
Powerset<D>::add_non_bottom_disjunct(const D& d,
				      iterator first,
				      iterator last) {
  for (iterator xi = first; xi != last; ) {
    const D& xv = *xi;
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

template <typename D>
inline void
Powerset<D>::add_non_bottom_disjunct(const D& d) {
  assert(!d.is_bottom());
  add_non_bottom_disjunct(d, begin(), end());
}

template <typename D>
inline void
Powerset<D>::add_disjunct(const D& d) {
  if (!d.is_bottom())
    add_non_bottom_disjunct(d);
}

template <typename D>
bool
Powerset<D>::definitely_entails(const Powerset& y) const {
  const Powerset<D>& x = *this;
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
template <typename D>
inline
bool operator==(const Powerset<D>& x, const Powerset<D>& y) {
  x.omega_reduce();
  y.omega_reduce();
  if (x.size() != y.size())
    return false;
  // Take a copy of `y' and work with it.
  Powerset<D> yy = y;
  for (typename Powerset<D>::const_iterator xi = x.begin(),
	 x_end = x.end(); xi != x_end; ++xi) {
    typename Powerset<D>::iterator yyi = yy.begin();
    typename Powerset<D>::iterator yy_end = yy.end();
    yyi = std::find(yyi, yy_end, *xi);
    if (yyi == yy_end)
      return false;
    else
      yy.drop_disjunct(yyi);
  }
  return true;
}

/*! \relates Powerset */
template <typename D>
inline
bool operator!=(const Powerset<D>& x, const Powerset<D>& y) {
  return !(x == y);
}

template <typename D>
inline bool
Powerset<D>::is_top() const {
  // Must perform omega-reduction for correctness.
  omega_reduce();
  const_iterator xi = begin();
  const_iterator x_end = end();
  return xi != x_end && xi->is_top() && ++xi == x_end;
}

template <typename D>
inline bool
Powerset<D>::is_bottom() const {
  // Must perform omega-reduction for correctness.
  omega_reduce();
  return empty();
}

template <typename D>
inline void
Powerset<D>::collapse() {
  if (!empty())
    collapse(sequence.begin());
}

template <typename D>
template <typename Binary_Operator_Assign>
void
Powerset<D>::pairwise_apply_assign(const Powerset& y,
				    Binary_Operator_Assign op_assign) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  Sequence new_sequence;
  for (const_iterator xi = begin(), x_end = end(),
	 y_begin = y.begin(), y_end = y.end(); xi != x_end; ++xi)
    for (const_iterator yi = y_begin; yi != y_end; ++yi) {
      D zi = *xi;
      op_assign(zi, *yi);
      if (!zi.is_bottom())
	new_sequence.push_back(zi);
    }
  // Put the new sequence in place.
  std::swap(sequence, new_sequence);
  reduced = false;
}

template <typename D>
inline void
Powerset<D>::meet_assign(const Powerset& y) {
  pairwise_apply_assign(y, std::mem_fun_ref(&D::meet_assign));
}

template <typename D>
void
Powerset<D>::least_upper_bound_assign(const Powerset& y) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  iterator old_begin = begin();
  iterator old_end = end();
  for (const_iterator i = y.begin(), y_end = y.end(); i != y_end; ++i)
    old_begin = add_non_bottom_disjunct(*i, old_begin, old_end);
}

template <typename D>
inline void
Powerset<D>::upper_bound_assign(const Powerset& y) {
  least_upper_bound_assign(y);
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Powerset */
template <typename D>
std::ostream&
operator<<(std::ostream& s, const Powerset<D>& x) {
  if (x.is_bottom())
    s << "false";
  else if (x.is_top())
    s << "true";
  else
    for (typename Powerset<D>::const_iterator i = x.begin(),
	   x_end = x.end(); i != x_end; ) {
      s << "{ " << *i++ << " }";
      if (i != x_end)
	s << ", ";
    }
  return s;
}

} // namespace IO_Operators

template <typename D>
memory_size_type
Powerset<D>::external_memory_in_bytes() const {
  memory_size_type bytes = 0;
  for (const_iterator xi = begin(), x_end = end(); xi != x_end; ++xi) {
    bytes += xi->total_memory_in_bytes();
    // We assume there is at least a forward and a backward link, and
    // that the pointers implementing them are at least the size of
    // pointers to `D'.
    bytes += 2*sizeof(D*);
  }
  return bytes;
}

template <typename D>
inline memory_size_type
Powerset<D>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename D>
bool
Powerset<D>::OK(const bool disallow_bottom) const {
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
template <typename D>
inline void
swap(Parma_Polyhedra_Library::Powerset<D>& x,
     Parma_Polyhedra_Library::Powerset<D>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Powerset_inlines_hh)
