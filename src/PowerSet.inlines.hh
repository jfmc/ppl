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

#if 0
// Construct a bottom element.
template <class CS>
PowerSet<CS>::PowerSet(bool) {
}
#endif

// Omega reduction.
template <class CS>
void PowerSet<CS>::omega() {
  const_iterator xi, yi, yin;
  for (xi = begin(); xi != end(); ++xi) {
    value_type xv = *xi;
    for (yi = xi, ++yi; yi != end(); yi = yin) {
      yin = yi;
      ++yin;
      if (entails(*yi, xv))
	erase(yi);
    }
  }
}

// Injection

template <class CS>
PowerSet<CS>& PowerSet<CS>::inject(const CS& x) {
  if (!x.is_bottom()) {
    insert(x);
    omega();
  }
  return *this;
}

// Bottom

template <class CS>
inline
PowerSet<CS>& PowerSet<CS>::bottom() {
  erase(begin(), end());
  return *this;
}

// Entailment

template <class CS>
bool entails(const PowerSet<CS>& x, const PowerSet<CS>& y) {
  if (x.size() == 1 && y.size() == 1)
    return entails(*(x.begin()), *(y.begin())) ;
  else {
    typename PowerSet<CS>::const_iterator xi, yi;
    bool found;
    found = true;
    for (xi = x.begin(); found && xi != x.end(); ++xi) {
      found = false;
      for (yi = y.begin(); (!found) && yi != y.end(); ++yi) {
	found = entails(*xi, *yi);
      }
    }
    return found;
  }
}

// Equality

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
  return empty();
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
	z.insert(zi);
    }
  }
  z.omega();
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
inline
PowerSet<CS>&
PowerSet<CS>::operator+=(const PowerSet<CS>& y) {
  if (this != &y) {
    const_iterator xi, yi;
    xi = begin();
    yi = y.begin();
    while (xi != end() && yi != y.end()) {
      if (lcompare(*xi, *yi))
	xi++;
      else if (lcompare(*yi, *xi))
	insert(xi, *yi++);
      else {
	xi++;
	yi++;
      }
    }
    while (yi != y.end())
      insert(end(), *yi++);
  }
  omega();
  return *this;
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
