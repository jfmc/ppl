/* AskTell class implementation: inline functions.
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

#ifndef _AskTell_inlines_hh
#define _AskTell_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename CS>
bool
entails(const CS& ax, const CS& tx, const CS& ay, const CS& ty) {
  if(!entails(ay, ax))
    return false;
  // The following test can be omitted.
  else if (ax == ay)
    return entails(tx, ty);
  else if (entails(tx, ty))
    return true;
  // The following test can be omitted.
  else if (entails(tx, ay))
    return false;
  else
    return entails(tx*ay, ty);
}

template <typename CS>
AskTell<CS>::iterator::iterator() {
}

template <typename CS>
AskTell<CS>::iterator::iterator(const Base& x)
  : Base(x) {
}

template <typename CS>
const CS&
AskTell<CS>::iterator::ask() const {
  return Base::operator*().first;
}

template <typename CS>
CS&
AskTell<CS>::iterator::tell() const {
  return Base::operator*().second;
}

template <typename CS>
AskTell<CS>::const_iterator::const_iterator() {
}

template <typename CS>
AskTell<CS>::const_iterator::const_iterator(const Base& x)
  : Base(x) {
}

template <typename CS>
const CS&
AskTell<CS>::const_iterator::ask() const {
  return Base::operator*().first;
}

template <typename CS>
const CS&
AskTell<CS>::const_iterator::tell() const {
  return Base::operator*().second;
}

template <typename CS>
typename AskTell<CS>::iterator
AskTell<CS>::begin() {
  return iterator(Base::begin());
}

template <typename CS>
typename AskTell<CS>::iterator
AskTell<CS>::end() {
  return iterator(Base::end());
}

template <typename CS>
typename AskTell<CS>::const_iterator
AskTell<CS>::begin() const {
  return const_iterator(Base::begin());
}

template <typename CS>
typename AskTell<CS>::const_iterator
AskTell<CS>::end() const {
  return const_iterator(Base::end());
}

// Map insertions

template <typename CS>
void AskTell<CS>::pair_insert_good(const CS& a, const CS& t) {
  std::pair<typename Base::iterator, bool> stat
    = Base::insert(Base::value_type(a, t));
  if (!stat.second)
    (*(stat.first)).second.meet_assign(t);
}

template <typename CS>
void
AskTell<CS>::pair_insert(const CS& a, const CS& t) {
  if (!entails(t, a)) {
    CS newt = t;
    newt.meet_assign(a);
    pair_insert_good(a, newt);
  }
  else
    pair_insert_good(a, t);
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
//     entails(x.ASK, y.ASK) && entails(y.TELL, x.TELL).

template <typename CS>
bool AskTell<CS>::reduce() {
  bool map_changed = false;
  iterator xi, yi, yin;
  for (xi = begin(); xi != end(); ++xi) {
    for (yi = xi, ++yi; yi != end(); yi = yin) {
      yin = yi;
      ++yin;
      if (entails((*yi).first, (*xi).first)
	  && entails((*xi).second, (*yi).second)) {
	erase(yi);
	map_changed = true;
      }
    }
  }
  return map_changed;
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
bool AskTell<CS>::deduce() {
  iterator xi, yi;
  bool tell_changed, map_changed;

  map_changed = false;
  for (xi = begin(); xi != end(); ++xi) {
    CS xtell((*xi).second);
    tell_changed = true;
    while (tell_changed) {
      tell_changed = false;
      for (yi = begin(); yi != end(); ++yi) {
	if (yi != xi) {
	  if (entails(xtell, (*yi).first) && !entails(xtell, (*yi).second)) {
	    xtell.meet_assign((*yi).second);
	    map_changed = tell_changed = true;
	  }
	}
      }
    }
    (*xi).second = xtell;
  }
  if (map_changed)
    (void) reduce();
  return map_changed;
}

// Absorption

template <typename CS>
bool AskTell<CS>::absorb() {
  iterator xi, xip, yi, yip;
  bool ask_changed, ask_did_change, map_changed;

  map_changed = false;
  for (xip = end(); xip != begin(); ) {
    xi = xip;
    --xi;
    CS xask((*xi).first);
    CS xtell((*xi).second);
    ask_did_change = false;
    ask_changed = true;
    yip = xi;
    while (ask_changed) {
      ask_changed = false;
      for (; yip != begin(); yip = yi) {
	yi = yip;
	--yi;
	if (yi != xi) {
	  if (entails(xask, (*yi).first) && (!entails(xask, (*yi).second))) {
	    xask.meet_assign((*yi).second);
	    ask_did_change = true;
	    ask_changed = true;
	  }
	}
      }
      if (ask_changed)
	yip = lower_bound(xask);
    }
    if (ask_did_change) {
      erase(xi);
      if (!entails(xask, xtell))
	pair_insert(xask, xtell);
      map_changed = true;
    }
    else
      xip = xi;
  }
  if (map_changed)
    (void) reduce();
  return map_changed;
}

// Engine

template <typename CS>
void AskTell<CS>::engine() {
  reduce();
  deduce();
  absorb();
}

// Bottom

template <typename CS>
AskTell<CS>&
AskTell<CS>::bottom() {
  CS top, bottom;
  erase(begin(), end());
  bottom.bottom();
  pair_insert_good(top, bottom);
  return *this;
}

// Entailment

template <typename CS>
bool
entails(const AskTell<CS>& x, const AskTell<CS>& y) {
  if (x.size() == 1 && y.size() == 1)
    return entails((*x.begin()).first, (*x.begin()).second,
		   (*y.begin()).first, (*y.begin()).second);
//    return entails(*(x.begin()), *(y.begin())) ;
  else {
    typename AskTell<CS>::const_iterator xi, yi;
    bool found;
    found = true;
    for (yi = y.begin(); found && yi != y.end(); ++yi) {
      found = false;
      for (xi = x.begin(); (!found) && xi != x.end(); ++xi)
	found = entails((*xi).first, (*xi).second, (*yi).first, (*yi).second);
//	found = entails(*xi, *yi);
    }
    bool found1 = (x*y == x);
    if (found != found1)
      cerr << "Disagreement on " << x << ", " << y << " sim: " << found <<
      " diff: " << found1 << endl;
    return found;
  }
}

// Equality

template <typename CS>
bool
operator==(const AskTell<CS>& x, const AskTell<CS>& y) {
  return (x.size() == y.size() && equal(x.begin(), x.end(), y.begin()));
}

template <typename CS>
AskTell<CS>&
AskTell<CS>::inject(const CS& askv, const CS& tellv) {
  if (!entails(askv, tellv)) {
    pair_insert(askv, tellv);
    engine();
  }
  return *this;
}

// Simple tests

template <typename CS>
bool
AskTell<CS>::is_top() const {
  return empty();
}

template <typename CS>
bool
AskTell<CS>::is_bottom() const {
  if (size() == 1)
    return ((*begin()).first.is_top() && (*begin()).second.is_bottom());
  else
    return false;
}

// Projection

template <typename CS>
CS
project(const AskTell<CS>& x) {
  CS ret;
  if (!empty() && (begin()).ask().is_top())
    ret = (begin()).tell();
  return ret;
}

// Meet operators

template <typename CS>
AskTell<CS> operator * (const AskTell<CS>& x, const AskTell<CS>& y) {
  typename AskTell<CS>::const_iterator yi;
  AskTell<CS> ret(x);
  for (yi = y.begin(); yi != y.end(); ++yi) 
    ret.pair_insert_good((*yi).first, (*yi).second);
  ret.engine();
  return ret;
}

template <typename CS>
void
AskTell<CS>::meet_assign(const AskTell<CS>& y) {
  AskTell<CS>::const_iterator yi;
  for (yi = y.begin(); yi != y.end(); ++yi) 
    pair_insert_good((*yi).first, (*yi).second);
  engine();
  return *this;
}

// Join operators

template <typename CS>
AskTell<CS>
operator+(const AskTell<CS>& x, const AskTell<CS>& y) {
  typename AskTell<CS>::const_iterator xi, yi;
  AskTell<CS> ret;
  for (xi = x.begin(); xi != x.end(); ++xi) {
    for (yi = y.begin(); yi != y.end(); ++yi) {
      CS tellv((*xi).second);
      tellv.upper_bound_assign((*yi).second);
      if (!tellv.is_top()) {
	CS askv((*xi).first);
	askv.meet_assign((*yi).first);
	if (!entails(askv, tellv))
	  ret.pair_insert(askv, tellv);
      }
    }
  }
  ret.engine();
  return ret;
}

template <typename CS>
void
AskTell<CS>::upper_bound_assign(const AskTell<CS>& y) {
  *this = *this + y;
  return *this;
}

// Hiding

template <typename CS>
bool
AskTell<CS>::probe(const CS& tellv, const CS& askv) const {
  const_iterator yi;
  bool tell_changed;

  CS xtell(tellv);
  tell_changed = true;
  while (tell_changed) {
    tell_changed = false;
    for (yi = begin(); yi != end(); ++yi) {
      if (entails(xtell, (*yi).first) && !entails(xtell, (*yi).second)) {
	  xtell.meet_assign((*yi).second);
	  if (entails(xtell, askv))
	    return true;
	  tell_changed = true;
      }
    }
  }
  return false;
}

// Lexicographic comparison

template <typename CS>
int
lcompare(const typename AskTell<CS>::value_type& x,
	 const typename AskTell<CS>::value_type& y) {
  if (int i = lcompare(x.first(), y.first()))
    return i;
  else
    return lcompare(x.second(), y.second());
}

template <typename CS>
int
lcompare(const AskTell<CS>& x, const AskTell<CS>& y) {
  return std::lexicographical_compare(x.begin(), x.end(),
				      y.begin(), y.end(),
				      AskTell<CS>::Less());
}

// Output

template <typename CS>
std::ostream&
operator<<(std::ostream& s, const AskTell<CS>& x) {
  if (x.is_top())
    s << "TOP";
  else if (x.is_bottom())
    s << "BOTTOM";
  else {
    typename AskTell<CS>::const_iterator xi;
    for (xi=x.begin(); xi != x.end(); ++xi)
      s << "(" << (*xi).first << " -> " << (*xi).second << ")";
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

#endif // _AskTell_inlines_hh
