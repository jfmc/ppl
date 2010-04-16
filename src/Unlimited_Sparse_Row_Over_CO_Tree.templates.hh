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
  iterator last_i = begin();
  const_iterator j = y.begin();
  if (i.itr.is_at_end())
    return;
  if (!j.itr.is_at_end()) {
    if (i->first == j->first) {
      g(i->second, j->second);
      last_i = i;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        f(i->second);
        last_i = i;
        ++i;
      } else {
        ++j;
      }
  } else {
    f(i->second);
    last_i = i;
    ++i;
  }
  PPL_ASSERT(!last_i.itr.is_at_end());
  while (!i.itr.is_at_end() && !j.itr.is_at_end())
    if (i->first == j->first) {
      g(i->second, j->second);
      last_i = i;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        f(i->second);
        last_i = i;
        ++i;
      } else
        j = y.lower_bound(i->first, j);
  while (!i.itr.is_at_end()) {
    f(i->second);
    ++i;
  }
}

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_CO_Tree
::combine_needs_second(const Unlimited_Sparse_Row_Over_CO_Tree& y,
                       const Func1& g, const Func2& h) {
  iterator i = begin();
  iterator last_i = begin();
  const_iterator j = y.begin();
  if (!i.itr.is_at_end()) {
    if (!j.itr.is_at_end()) {
      if (i->first == j->first) {
        g(i->second, j->second);
        last_i = i;
        ++i;
        ++j;
      } else
        if (i->first < j->first) {
          last_i = i;
          ++i;
        } else {
          last_i = find_create(j->first);
          h(last_i->second, j->second);
          i = last_i;
          ++i;
          if (this == &y)
            j = last_i;
          ++j;
        }
    } else {
      last_i = i;
      ++i;
    }
  } else {
    if (!j.itr.is_at_end()) {
      last_i = find_create(j->first);
      h(last_i->second, j->second);
      i = last_i;
      ++i;
      if (this == &y)
        j = last_i;
      ++j;
    } else {
      PPL_ASSERT(i.itr.is_at_end());
      PPL_ASSERT(j.itr.is_at_end());

      return;
    }
  }
  PPL_ASSERT(!last_i.itr.is_at_end());
  while (!i.itr.is_at_end() && !j.itr.is_at_end())
    if (i->first == j->first) {
      g(i->second, j->second);
      last_i = i;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        last_i = i;
        i = lower_bound(j->first, i);
      } else {
        last_i = find_create(j->first, last_i);
        h(last_i->second, j->second);
        i = last_i;
        ++i;
        if (this == &y)
          j = last_i;
        ++j;
      }
  while (!j.itr.is_at_end()) {
    last_i = find_create(j->first, last_i);
    h(last_i->second, j->second);
    ++j;
  }
}

template <typename Func1, typename Func2, typename Func3>
void
Unlimited_Sparse_Row_Over_CO_Tree
::combine(const Unlimited_Sparse_Row_Over_CO_Tree& y, const Func1& f,
          const Func2& g, const Func3& h) {
  iterator i = begin();
  iterator last_i = begin();
  const_iterator j = y.begin();
  if (!i.itr.is_at_end()) {
    if (!j.itr.is_at_end()) {
      if (i->first == j->first) {
        g(i->second, j->second);
        last_i = i;
        ++i;
        ++j;
      } else
        if (i->first < j->first) {
          f(i->second);
          last_i = i;
          ++i;
        } else {
          last_i = find_create(j->first);
          h(last_i->second, j->second);
          i = last_i;
          ++i;
          if (this == &y)
            j = last_i;
          ++j;
        }
    } else {
      f(i->second);
      last_i = i;
      ++i;
    }
  } else {
    if (!j.itr.is_at_end()) {
      last_i = find_create(j->first);
      h(last_i->second, j->second);
      i = last_i;
      ++i;
      if (this == &y)
        j = last_i;
      ++j;
    } else {
      PPL_ASSERT(i.itr.is_at_end());
      PPL_ASSERT(j.itr.is_at_end());

      return;
    }
  }
  PPL_ASSERT(!last_i.itr.is_at_end());
  while (!i.itr.is_at_end() && !j.itr.is_at_end())
    if (i->first == j->first) {
      g(i->second, j->second);
      last_i = i;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        f(i->second);
        last_i = i;
        ++i;
      } else {
        last_i = find_create(j->first, last_i);
        h(last_i->second, j->second);
        i = last_i;
        ++i;
        if (this == &y)
          j = last_i;
        ++j;
      }
  while (!i.itr.is_at_end()) {
    f(i->second);
    ++i;
  }
  while (!j.itr.is_at_end()) {
    last_i = find_create(j->first, last_i);
    h(last_i->second, j->second);
    ++j;
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Over_CO_Tree_templates_hh)
