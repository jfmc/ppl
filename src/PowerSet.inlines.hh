/* PowerSet class implementation: inline functions.
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

#ifndef _PowerSet_inlines_hh
#define _PowerSet_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <class CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::begin() {
  return sequence.begin();
}

template <class CS>
typename PowerSet<CS>::const_iterator
PowerSet<CS>::begin() const {
  return sequence.begin();
}

template <class CS>
typename PowerSet<CS>::iterator
PowerSet<CS>::end() {
  return sequence.end();
}

template <class CS>
typename PowerSet<CS>::const_iterator
PowerSet<CS>::end() const {
  return sequence.end();
}

template <class CS>
size_t
PowerSet<CS>::size() const {
  return sequence.size();
}

template <class CS>
PowerSet<CS>::PowerSet() {
}

template <class CS>
void PowerSet<CS>::omega_reduction() {
  iterator xi, xin, yi, yin;
  for (xi = xin = begin(); xi != end(); xi = xin) {
    ++xin;
    const CS& xv = *xi;
    for (yi = yin = begin(); yi != end(); yi = yin) {
      ++yin;
      if (xi == yi)
	continue;
      const CS& yv = *yi;
      if (entails(yv, xv))
	sequence.erase(yi);
      else if (entails(xv, yv)) {
	sequence.erase(xi);
	break;
      }
    }
  }
}

template <class CS>
PowerSet<CS>& PowerSet<CS>::inject(const CS& x) {
  if (!x.is_bottom()) {
    sequence.push_back(x);
    omega_reduction();
  }
  return *this;
}

template <class CS>
bool
entails(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  bool found = true;
  for (typename PowerSet<CS>::const_iterator xi = x.begin(),
	 xend = x.end(); found && xi != xend; ++xi) {
    found = false;
    for (typename PowerSet<CS>::const_iterator yi = y.begin(),
	   yend = y.end(); !found && yi != yend; ++yi)
      found = entails(*xi, *yi);
  }
  return found;
}

template <class CS>
inline
bool operator==(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  return (x.size() == y.size() && equal(x.begin(), x.end(), y.begin()));
}

template <class CS>
inline
bool operator!=(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  return !(x == y);
}

// Simple tests

template <class CS>
inline bool
PowerSet<CS>::is_top() const {
  if (size() == 1) {
    value_type tv = *(begin());
    return tv.is_top();
  }
  else
    return false;
}

template <class CS>
inline bool
PowerSet<CS>::is_bottom() const {
  return sequence.empty();
}

// Projection

template <class CS>
CS project(const PowerSet<CS>& x) {
  CS ret;
  typename PowerSet<CS>::const_iterator xi;

  if (x.size() == 0)
    ret.bottom();
  else {
    ret = *(x.begin());
    for (xi=x.begin(), ++xi; xi != x.end(); ++xi) {
      ret += project(*xi);
    }
  }
  return ret;
}

// Meet operators

template <class CS>
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

template <class CS>
PowerSet<CS>&
PowerSet<CS>::operator*=(const PowerSet<CS>& y) {
  if (this != &y) {
    *this = *this * y;
  }
  return *this;
}

// Join operator

template <class CS>
void
PowerSet<CS>::upper_bound_assign(const PowerSet<CS>& y) {
  std::copy(y.begin(), y.end(), back_inserter(sequence));
  omega_reduction();
}

// Hiding

template <class CS>
PowerSet<CS> hide(const PowerSet<CS>& x, Variable n) {
  PowerSet<CS> ret(true);
  typename PowerSet<CS>::const_iterator xi;
  for (xi=x.begin(); xi != x.end(); ++xi)
    ret.insert(hide(*xi, n));
  ret.omega();
  return ret;
}

template <class CS>
inline
PowerSet<CS>& PowerSet<CS>::hide_assign(Variable n) {
  *this = hide(*this, n);
  return *this;
}

// Renaming

template <class CS>
PowerSet<CS> operator << (const PowerSet<CS>& x, Variable n) {
  PowerSet<CS> ret(true);
  typename PowerSet<CS>::const_iterator xi, ii;
  for (ii = xi = x.begin(); xi != x.end(); ++xi)
    ii = ret.insert(ii, *xi<<n);
  return ret;
}

template <class CS>
inline
PowerSet<CS>& PowerSet<CS>::operator <<= (Variable n) {
  *this = *this << n;
  return *this;
}

// Lexicographic comparison

template <class CS>
int
lcompare(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  typename PowerSet<CS>::const_iterator xi = x.begin();
  typename PowerSet<CS>::const_iterator yi = y.begin();
  while (true) {
    if (yi == y.end())
      if (xi == x.end())
	return 0;
      else
	return 1;
    else if (xi == x.end())
      return -1;
    else {
      int i = lcompare(*xi, *yi);
      if (i != 0)
	return i;
      xi++;
      yi++;
    }
  }
}

// Output

template <class CS>
std::ostream& operator<< (std::ostream& s, const PowerSet<CS>& x) {
  if (x.is_bottom())
    s << "false";
  else if ((x.size() == 1) && (*(x.begin())).is_top())
    s << "true";
  else {
    s << "{ ";
    typename PowerSet<CS>::const_iterator i = x.begin();
    typename PowerSet<CS>::const_iterator xend = x.end();
    while (i != xend) {
      s << *i++;
      if (i != xend)
	s << ", ";
    }
    s << " }";
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

#endif // _PowerSet_inlines_hh
