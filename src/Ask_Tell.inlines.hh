/* Ask_Tell class implementation: inline functions.
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

#ifndef PPL_Ask_Tell_inlines_hh
#define PPL_Ask_Tell_inlines_hh 1

#include <algorithm>
#include <cassert>

// Temporary!!!
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename CS>
Ask_Tell_Pair<CS>::Ask_Tell_Pair(const CS& ask, const CS& tell)
  : a(ask), t(tell) {
}

template <typename CS>
const CS&
Ask_Tell_Pair<CS>::ask() const {
  return a;
}

template <typename CS>
CS&
Ask_Tell_Pair<CS>::ask() {
  return a;
}

template <typename CS>
const CS&
Ask_Tell_Pair<CS>::tell() const {
  return t;
}

template <typename CS>
CS&
Ask_Tell_Pair<CS>::tell() {
  return t;
}

template <typename CS>
bool
Ask_Tell_Pair<CS>::definitely_entails(const Ask_Tell_Pair& y) const {
  const CS& ax = ask();
  const CS& tx = tell();
  const CS& ay = y.ask();
  const CS& ty = y.tell();
   if(!ay.definitely_entails(ax))
    return false;
  else if (tx.definitely_entails(ty))
    return true;
  // The following test can be omitted.
  else if (tx.definitely_entails(ay))
    return false;
  else
    return (tx*ay).definitely_entails(ty);
}


template <typename CS>
Ask_Tell<CS>::Ask_Tell(dimension_type num_dimensions, bool universe)
  : space_dim(num_dimensions) {
  if (!universe)
    add_pair(CS(num_dimensions, true), CS(num_dimensions, false));
}

template <typename CS>
Ask_Tell<CS>::Ask_Tell(const Ask_Tell<CS>& y)
  : sequence(y.sequence), space_dim(y.space_dim) {
}

template <typename CS>
Ask_Tell<CS>&
Ask_Tell<CS>:: operator=(const Ask_Tell<CS>& y) {
  sequence = y.sequence;
  space_dim = y.space_dim;
  return *this;
}

template <typename CS>
inline void
Ask_Tell<CS>::swap(Ask_Tell& y) {
  std::swap(sequence, y.sequence);
  std::swap(space_dim, y.space_dim);
}

template <typename CS>
Ask_Tell<CS>::Ask_Tell(const Constraint_System& cs)
  : space_dim(cs.space_dimension()) {
  CS tell(cs);
  if (!tell.is_top()) {
    CS ask(space_dim);
    pair_insert_good(ask, tell);
  }
}

template <typename CS>
dimension_type
Ask_Tell<CS>::space_dimension() const {
  return space_dim;
}

template <typename CS>
typename Ask_Tell<CS>::iterator
Ask_Tell<CS>::begin() {
  return sequence.begin();
}

template <typename CS>
typename Ask_Tell<CS>::const_iterator
Ask_Tell<CS>::begin() const {
  return sequence.begin();
}

template <typename CS>
typename Ask_Tell<CS>::iterator
Ask_Tell<CS>::end() {
  return sequence.end();
}

template <typename CS>
typename Ask_Tell<CS>::const_iterator
Ask_Tell<CS>::end() const {
  return sequence.end();
}

template <typename CS>
typename Ask_Tell<CS>::reverse_iterator
Ask_Tell<CS>::rbegin() {
  return sequence.rbegin();
}

template <typename CS>
typename Ask_Tell<CS>::const_reverse_iterator
Ask_Tell<CS>::rbegin() const {
  return sequence.rbegin();
}

template <typename CS>
typename Ask_Tell<CS>::reverse_iterator
Ask_Tell<CS>::rend() {
  return sequence.rend();
}

template <typename CS>
typename Ask_Tell<CS>::const_reverse_iterator
Ask_Tell<CS>::rend() const {
  return sequence.rend();
}

template <typename CS>
typename Ask_Tell<CS>::size_type
Ask_Tell<CS>::size() const {
  return sequence.size();
}

template <typename CS>
void
Ask_Tell<CS>::pair_insert_good(const CS& a, const CS& t) {
  sequence.push_back(Ask_Tell_Pair<CS>(a, t));
}

template <typename CS>
void
Ask_Tell<CS>::pair_insert(const CS& a, const CS& t) {
  if (!t.definitely_entails(a)) {
    CS newt = t;
    newt.meet_assign(a);
    pair_insert_good(a, newt);
  }
  else
    pair_insert_good(a, t);
}

template <typename CS>
void
Ask_Tell<CS>::add_constraint(const Constraint& c) {
  CS tell(space_dim);
  tell.add_constraint(c);
  if (!tell.is_top()) {
    CS ask(space_dim);
    pair_insert_good(ask, tell);
  }
  engine();
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::add_constraints(const Constraint_System& cs) {
  CS tell(cs);
  if (!tell.is_top()) {
    CS ask(space_dim);
    pair_insert_good(ask, tell);
  }
  engine();
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::add_space_dimensions_and_embed(dimension_type m) {
  space_dim += m;
  for (typename Ask_Tell<CS>::iterator i = begin(),
	 send = end(); i != send; ++i) {
    Ask_Tell_Pair<CS>& p = *i;
    p.ask().add_space_dimensions_and_embed(m);
    p.tell().add_space_dimensions_and_embed(m);
  }
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::add_space_dimensions_and_project(dimension_type m) {
  space_dim += m;
  for (typename Ask_Tell<CS>::iterator i = begin(),
	 send = end(); i != send; ++i) {
    Ask_Tell_Pair<CS>& p = *i;
    p.ask().add_space_dimensions_and_project(m);
    p.tell().add_space_dimensions_and_project(m);
  }
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::remove_space_dimensions(const Variables_Set& to_be_removed) {
  space_dim -= to_be_removed.size();
  for (typename Ask_Tell<CS>::iterator i = begin(),
	 send = end(); i != send; ++i) {
    Ask_Tell_Pair<CS>& p = *i;
    p.ask().remove_space_dimensions(to_be_removed);
    p.tell().remove_space_dimensions(to_be_removed);
  }
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::remove_higher_space_dimensions(dimension_type new_dimension) {
  space_dim = new_dimension;
  for (typename Ask_Tell<CS>::iterator i = begin(),
	 send = end(); i != send; ++i) {
    Ask_Tell_Pair<CS>& p = *i;
    p.ask().remove_higher_space_dimensions(new_dimension);
    p.tell().remove_higher_space_dimensions(new_dimension);
  }
  assert(OK());
}

template <typename CS>
template <typename Partial_Function>
void
Ask_Tell<CS>::map_space_dimensions(const Partial_Function& pfunc) {
  if (is_top()) {
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
    for (iterator i = sbegin, send = end(); i != send; ++i) {
      Ask_Tell_Pair<CS>& p = *i;
      p.ask().map_space_dimensions(pfunc);
      p.tell().map_space_dimensions(pfunc);
    }
    space_dim = sbegin->ask().space_dimension();
  }
  assert(OK());
}

// Reduction
//
// Preconditions:
//
//     the map is well formed.
//
// Postconditions:
//
//     the map is well formed and there are no two pairs x and y such that
//     x.ASK.definitely_entails(y.ASK) && y.TELL.definitely_entails(x.TELL).

template <typename CS>
bool
Ask_Tell<CS>::reduce() {
  bool changed = false;
  for (iterator sbegin = begin(),
	 send = end(), xi = sbegin; xi != send; ++xi)
    for (iterator yi = sbegin, yin; yi != send; yi = yin) {
      yin = yi;
      ++yin;
      if (xi != yi
	  && yi->ask().definitely_entails(xi->ask())
	  && xi->tell().definitely_entails(yi->tell())) {
	erase(yi);
	sbegin = begin();
	send = end();
	changed = true;
      }
    }
  assert(OK());
  return changed;
}

// Deduction
//
// Preconditions:
//
//     the map is well formed and the postcondition of reduce() is satisfied.
//
// Postconditions:
//
//     the map is well formed, the postcondition of reduce() is satisfied,
//     and...
//

template <typename CS>
bool
Ask_Tell<CS>::deduce() {
  bool changed = false;
  for (iterator sbegin = begin(),
	 send = end(), xi = sbegin; xi != send; ++xi) {
    CS& xi_tell = xi->tell();
    bool tell_changed;
    do {
      tell_changed = false;
      for (iterator yi = sbegin; yi != send; ++yi) {
	if (xi != yi
	    && xi_tell.definitely_entails(yi->ask())
	    && !xi_tell.definitely_entails(yi->tell())) {
	  xi_tell.meet_assign(yi->tell());
	  changed = tell_changed = true;
	  }
	}
    } while (tell_changed);
  }
  if (changed)
    (void) reduce();
  assert(OK());
  return changed;
}

// Absorption

template <typename CS>
bool
Ask_Tell<CS>::absorb() {
  bool changed = false;
  for (iterator sbegin = begin(),
	 send = end(), xi = sbegin, xin; xi != send; xi = xin) {
    xin = xi;
    ++xin;
    CS& xi_ask = xi->ask();
    CS& xi_tell = xi->tell();
    // We are may strengthen the ask component of the pair referenced by xi.
    // If we do it the pair may become useless (i.e., with the ask component
    // entailing the tell component) and thus be discarded.
    bool must_check_xi_pair = false;
    bool ask_changed;
    do {
      ask_changed = false;
      for (iterator yi = sbegin; yi != send; ++yi) {
	if (xi != yi) {
	  CS& yi_ask = yi->ask();
	  CS& yi_tell = yi->tell();
	  if (xi_ask.definitely_entails(yi_ask)
	      && !xi_ask.definitely_entails(yi_tell)) {
	    xi_ask.meet_assign(yi_tell);
	    must_check_xi_pair = true;
	    ask_changed = true;
	  }
	}
      }
    } while (ask_changed);
    if (must_check_xi_pair) {
      changed = true;
      if (xi_ask.definitely_entails(xi_tell))
	erase(xi);
    }
  }
  if (changed)
    (void) reduce();
  assert(OK());
  return changed;
}

// Engine

template <typename CS>
void Ask_Tell<CS>::engine() {
  reduce();
  deduce();
  absorb();
}

// Bottom

template <typename CS>
Ask_Tell<CS>&
Ask_Tell<CS>::bottom() {
  CS top, bottom;
  erase(begin(), end());
  bottom.bottom();
  pair_insert_good(top, bottom);
  return *this;
}

// Entailment

template <typename CS>
bool
Ask_Tell<CS>::definitely_entails(const Ask_Tell<CS>& y) const {
  const Ask_Tell<CS>& x = *this;
  if (x.size() == 1 && y.size() == 1)
    return (*x.begin()).definitely_entails(*y.begin());
  else {
    const_iterator xi, yi;
    bool found;
    found = true;
    for (yi = y.begin(); found && yi != y.end(); ++yi) {
      found = false;
      for (xi = x.begin(); (!found) && xi != x.end(); ++xi)
	found = (*xi).definitely_entails(*yi);
    }
#if 0
    bool found1 = (x*y == x);
    if (found != found1)
      cerr << "Disagreement on " << x << ", " << y << " sim: " << found <<
      " diff: " << found1 << endl;
#endif
    return found;
  }
}

template <typename CS>
Ask_Tell<CS>&
Ask_Tell<CS>::add_pair(const CS& ask, const CS& tell) {
  if (!ask.definitely_entails(tell)) {
    pair_insert(ask, tell);
    engine();
  }
  assert(OK());
  return *this;
}

/*! \relates Ask_Tell */
template <typename CS>
inline
bool operator==(const Ask_Tell<CS>& x, const Ask_Tell<CS>& y) {
  return x.size() == y.size() && equal(x.begin(), x.end(), y.begin());
}

/*! \relates Ask_Tell */
template <typename CS>
inline
bool operator!=(const Ask_Tell<CS>& x, const Ask_Tell<CS>& y) {
  return !(x == y);
}

// Simple tests

template <typename CS>
bool
Ask_Tell<CS>::is_top() const {
  return sequence.empty();
}

template <typename CS>
bool
Ask_Tell<CS>::is_bottom() const {
  if (size() == 1)
    return begin()->ask().is_top() && begin()->tell().is_bottom();
  else
    return false;
}

// Projection

template <typename CS>
CS
project(const Ask_Tell<CS>& x) {
  CS ret;
  if (!x.empty() && (x.begin())->ask().is_top())
    ret = (x.begin())->tell();
  return ret;
}

// Meet operators

template <typename CS>
Ask_Tell<CS>
operator*(const Ask_Tell<CS>& x, const Ask_Tell<CS>& y) {
  typename Ask_Tell<CS>::const_iterator yi;
  Ask_Tell<CS> ret(x);
  for (yi = y.begin(); yi != y.end(); ++yi)
    ret.pair_insert_good(yi->ask(), yi->tell());
  ret.engine();
  return ret;
}

template <typename CS>
void
Ask_Tell<CS>::meet_assign(const Ask_Tell<CS>& y) {
  std::copy(y.begin(), y.end(), back_inserter(sequence));
  engine();
  assert(OK());
}

template <typename CS>
void
Ask_Tell<CS>::concatenate_assign(const Ask_Tell<CS>& y) {
  dimension_type old_space_dim = space_dim;
  add_space_dimensions_and_embed(y.space_dimension());
  for (typename Ask_Tell<CS>::const_iterator y_end = y.end(),
	 yi = y.begin(); yi != y_end; ++yi) {
    CS ask(old_space_dim);
    ask.concatenate_assign(yi->ask());
    CS tell(old_space_dim);
    tell.concatenate_assign(yi->tell());
    pair_insert_good(ask, tell);
  }
  assert(OK());
}

// Join operators

template <typename CS>
Ask_Tell<CS>
operator+(const Ask_Tell<CS>& x, const Ask_Tell<CS>& y) {
  Ask_Tell<CS> z(x.space_dimension());
  for (typename Ask_Tell<CS>::const_iterator xi = x.begin(),
	 x_end = x.end(); xi != x_end; ++xi)
    for (typename Ask_Tell<CS>::const_iterator yi = y.begin(),
	   y_end = y.end(); yi != y_end; ++yi) {
      CS tell = xi->tell();
      tell.upper_bound_assign(yi->tell());
      CS ask = xi->ask();
      ask.meet_assign(yi->ask());
      if (!ask.definitely_entails(tell))
	z.pair_insert(ask, tell);
    }
  z.engine();
  return z;
}

template <typename CS>
void
Ask_Tell<CS>::upper_bound_assign(const Ask_Tell& y) {
  *this = *this + y;
}

// Hiding

template <typename CS>
bool
Ask_Tell<CS>::probe(const CS& tell, const CS& ask) const {
  const_iterator yi;
  bool tell_changed;

  CS xtell(tell);
  tell_changed = true;
  while (tell_changed) {
    tell_changed = false;
    for (yi = begin(); yi != end(); ++yi) {
      if (xtell.definitely_entails(yi->ask())
	  && !xtell.definitely_entails(yi->tell())) {
	  xtell.meet_assign(yi->tell());
	  if (xtell.definitely_entails(ask))
	    return true;
	  tell_changed = true;
      }
    }
  }
  return false;
}

template <typename CS>
bool
Ask_Tell<CS>::OK() const {
  for (typename Ask_Tell<CS>::const_iterator i = begin(),
	 send = end(); i != send; ++i) {
    const Ask_Tell_Pair<CS>& p = *i;
    if (!p.ask().OK() || p.ask().space_dimension() != space_dim
	|| !p.tell().OK() || p.tell().space_dimension() != space_dim
	|| p.ask().definitely_entails(p.tell()))
      return false;
  }
  return true;
}

namespace IO_Operators {

template <typename CS>
std::ostream&
operator<<(std::ostream& s, const Ask_Tell<CS>& x) {
  if (x.is_top())
    s << "true";
  else if (x.is_bottom())
    s << "false";
  else
    for (typename Ask_Tell<CS>::const_iterator xi = x.begin(),
	   x_end = x.end(); xi != x_end; ++xi)
      s << "(" << xi->ask() << " -> " << xi->tell() << ")";
  return s;
}

} // namespace IO_Operators

template <typename CS>
void
Ask_Tell<CS>::H79_extrapolation_assign(const Ask_Tell& y) {
  using namespace IO_Operators;
  std::cout << *this << std::endl
	    << y << std::endl;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Ask_Tell */
template <typename CS>
inline void
swap(Parma_Polyhedra_Library::Ask_Tell<CS>& x,
     Parma_Polyhedra_Library::Ask_Tell<CS>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Ask_Tell_inlines_hh)
