/* Unlimited_Sparse_Row_Over_CO_Tree class implementation: non-inline template functions.
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

#ifndef PPL_Unlimited_Sparse_Row_Over_CO_Tree_templates_hh
#define PPL_Unlimited_Sparse_Row_Over_CO_Tree_templates_hh 1

// TODO: Remove this.
// Added to please KDevelop4.
#include "Unlimited_Sparse_Row_Over_CO_Tree.defs.hh"


namespace Parma_Polyhedra_Library {

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_CO_Tree
::combine_needs_first(const Unlimited_Sparse_Row_Over_CO_Tree& y,
                      const Func1& f, const Func2& g) {
  iterator i = begin();
  const_iterator j = y.begin();
  while (!i.itr.is_at_end() && !j.itr.is_at_end())
    if (i->first == j->first) {
      g(i->second, j->second);
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        f(i->second);
        ++i;
      } else
        y.lower_bound_hint_assign(i->first, j);
  while (!i.itr.is_at_end()) {
    f(i->second);
    ++i;
  }
}

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_CO_Tree
::combine_needs_second(const Unlimited_Sparse_Row_Over_CO_Tree& y,
                       const Func1& g, const Func2& /* h */) {
  iterator i;
  unordered_const_iterator j = y.unordered_begin();
  unordered_const_iterator j_end = y.unordered_end();
  for ( ; j != j_end; ++j) {
    find_create_assign(j->first, i);
    g(i->second, j->second);
  }
}

template <typename Func1, typename Func2, typename Func3>
void
Unlimited_Sparse_Row_Over_CO_Tree
::combine(const Unlimited_Sparse_Row_Over_CO_Tree& y, const Func1& f,
          const Func2& g, const Func3& h) {
  iterator i = begin();
  const_iterator j = y.begin();
  while (!i.itr.is_at_end() && !j.itr.is_at_end()) {
    if (i->first == j->first) {
      g(i->second, j->second);
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        f(i->second);
        ++i;
      } else {
        PPL_ASSERT(i->first > j->first);
        find_create_hint_assign(j->first, i);
        if (this == &y)
          j = i;
        h(i->second, j->second);
        ++i;
        ++j;
      }
  }
  PPL_ASSERT(i.itr.is_at_end() || j.itr.is_at_end());
  while (!i.itr.is_at_end()) {
    f(i->second);
    ++i;
  }
  if (!j.itr.is_at_end()) {
    PPL_ASSERT(this != &y);
    if (tree.empty())
      find_create_assign(j->first, i);
    else {
      --i;
      find_create_hint_assign(j->first, i);
    }
    h(i->second, j->second);
    ++j;

    while (!j.itr.is_at_end()) {
      find_create_hint_assign(j->first, i);
      h(i->second, j->second);
      ++j;
    }
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Over_CO_Tree_templates_hh)
