/* Unlimited_Sparse_Row class implementation: non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Unlimited_Sparse_Row_templates_hh
#define PPL_Unlimited_Sparse_Row_templates_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row.defs.hh"


namespace Parma_Polyhedra_Library {

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row
::combine_needs_first(const Unlimited_Sparse_Row& y,
                      const Func1& f, const Func2& g) {
  if (this == &y) {
    for (iterator i = begin(), i_end = end(); i != i_end; ++i)
      g(i->second, i->second);
  } else {
    iterator i = begin();
    iterator i_end = end();
    const_iterator j = y.begin();
    const_iterator j_end = y.end();
    while (i != i_end && j != j_end)
      if (i->first == j->first) {
        g(i->second, j->second);
        if (i->second == 0) {
          i = reset(i);
          i_end = end();
        } else
          ++i;
        ++j;
      } else
        if (i->first < j->first) {
          f(i->second);
          if (i->second == 0) {
            i = reset(i);
            i_end = end();
          } else
            ++i;
        } else
          j = y.lower_bound(j, i->first);
    while (i != i_end) {
      f(i->second);
      if (i->second == 0) {
        i = reset(i);
        i_end = end();
      } else
        ++i;
    }
  }
}

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row
::combine_needs_second(const Unlimited_Sparse_Row& y,
                       const Func1& g, const Func2& /* h */) {
  iterator i;
  for (const_iterator j = y.begin(), j_end = y.end(); j != j_end; ++j) {
    i = find_create(j->first);
    g(i->second, j->second);
  }
}

template <typename Func1, typename Func2, typename Func3>
void
Unlimited_Sparse_Row
::combine(const Unlimited_Sparse_Row& y, const Func1& f,
          const Func2& g, const Func3& h) {
  if (this == &y) {
    for (iterator i = begin(), i_end = end(); i != i_end; ++i)
      g(i->second, i->second);
  } else {
    iterator i = begin();
    iterator i_end = end();
    const_iterator j = y.begin();
    const_iterator j_end = y.end();
    while (i != i_end && j != j_end) {
      if (i->first == j->first) {
        g(i->second, j->second);
        if (i->second == 0) {
          i = reset(i);
          i_end = end();
        } else
          ++i;
        ++j;
      } else
        if (i->first < j->first) {
          f(i->second);
          if (i->second == 0) {
            i = reset(i);
            i_end = end();
          } else
            ++i;
        } else {
          PPL_ASSERT(i->first > j->first);
          i = find_create(i, j->first);
          i_end = end();
          h(i->second, j->second);
          if (i->second == 0)
            i = reset(i);
          else
            ++i;
          ++j;
        }
    }
    PPL_ASSERT(i == i_end || j == j_end);
    while (i != i_end) {
      f(i->second);
      if (i->second == 0) {
        i = reset(i);
        i_end = end();
      } else
        ++i;
    }
    while (j != j_end) {
      if (tree.empty()) {
        i = find_create(j->first);
      } else {
        --i;
        i = find_create(i, j->first);
      }
      h(i->second, j->second);
      ++j;
      if (i->second == 0)
        i = reset(i);
      else
        break;
    }
    while (j != j_end) {
      i = find_create(i, j->first);
      h(i->second, j->second);
      if (i->second == 0)
        i = reset(i);
      ++j;
    }
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_templates_hh)
