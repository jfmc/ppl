/* Unlimited_Sparse_Row_Over_Linear_Sequence class implementation: non-inline template functions.
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

#ifndef PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_templates_hh
#define PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_templates_hh 1


namespace Parma_Polyhedra_Library {

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_Linear_Sequence
::combine_needs_first(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
                      const Func1& f, const Func2& g) {
  iterator i = begin();
  iterator last_i = begin();
  iterator i_end = end();
  const_iterator j = y.begin();
  const_iterator j_end = y.end();
  if (i == i_end)
    return;
  if (j != j_end) {
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
  PPL_ASSERT(last_i != i_end);
  while (i != i_end && j != j_end)
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
  while (i != i_end) {
    f(i->second);
    ++i;
  }
}

#ifndef PPL_SPARSE_BACKEND_SLOW_INSERTIONS

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_Linear_Sequence
::combine_needs_second(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
                       const Func1& g, const Func2& h) {
  iterator i = begin();
  iterator last_i = begin();
  iterator i_end = end();
  const_iterator j = y.begin();
  const_iterator j_end = y.end();
  if (i != i_end) {
    if (j != j_end) {
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
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
          i = last_i;
          ++i;
          i_end = end();
          if (this == &y) {
            j = last_i;
            j_end = y.end();
          }
#endif
          ++j;
        }
    } else {
      last_i = i;
      ++i;
    }
  } else {
    if (j != j_end) {
      last_i = find_create(j->first);
      h(last_i->second, j->second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
      i = last_i;
      ++i;
      i_end = end();
      if (this == &y) {
        j = last_i;
        j_end = y.end();
      }
#endif
      ++j;
    } else {
      PPL_ASSERT(i == i_end);
      PPL_ASSERT(j == j_end);

      return;
    }
  }
  PPL_ASSERT(last_i != i_end);
  while (i != i_end && j != j_end)
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
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = end();
        if (this == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  while (j != j_end) {
    last_i = find_create(j->first, last_i);
    h(last_i->second, j->second);
    ++j;
  }
}

#else

template <typename Func1, typename Func2>
void
Unlimited_Sparse_Row_Over_Linear_Sequence
::combine_needs_second(const Unlimited_Sparse_Row_Over_Linear_Sequence& y,
                       const Func1& g, const Func2& h) {
  Unlimited_Sparse_Row_Over_Linear_Sequence row;
  iterator itr = row.end();
  iterator i = begin();
  iterator i_end = end();
  const_iterator j = y.begin();
  const_iterator j_end = y.end();
  if (i == i_end && j == j_end)
    return;
  if (j == j_end
      || (i != i_end && i->first < j->first)) {
    itr = row.find_create(i->first);
    // We need to do itr->second = i->second, but this is faster.
    std::swap(itr->second, i->second);
    ++i;
  } else
      if (i == i_end
          || (j != j_end && i->first > j->first)) {
        itr = row.find_create(j->first);
        h(itr->second, j->second);
        ++j;
      } else {
        PPL_ASSERT(i != i_end);
        PPL_ASSERT(j != j_end);
        PPL_ASSERT(i->first == j->first);
        itr = row.find_create(i->first);
        // We need to do itr->second = i->second, but this is faster.
        std::swap(itr->second, i->second);
        g(itr->second, j->second);
        ++i;
        ++j;
      }
  PPL_ASSERT(itr != row.end());
  while (i != i_end && j != j_end) {
    if (i->first < j->first) {
      itr = row.find_create(i->first, itr);
      // We need to do itr->second = i->second, but this is faster.
      std::swap(itr->second, i->second);
      ++i;
    } else {
      if (i->first > j->first) {
        itr = row.find_create(j->first, itr);
        h(itr->second, j->second);
        ++j;
      } else {
        PPL_ASSERT(i->first == j->first);
        itr = row.find_create(i->first, itr);
        // We need to do itr->second = i->second, but this is faster.
        std::swap(itr->second, i->second);
        g(itr->second, j->second);
        ++i;
        ++j;
      }
    }
  }
  while (j != j_end) {
    itr = row.find_create(j->first, itr);
    h(itr->second, j->second);
    ++j;
  }
  PPL_ASSERT(j == j_end);
  std::swap(row,*this);
}

#endif // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

#ifndef PPL_SPARSE_BACKEND_SLOW_INSERTIONS

template <typename Func1, typename Func2, typename Func3>
void
Unlimited_Sparse_Row_Over_Linear_Sequence
::combine(const Unlimited_Sparse_Row_Over_Linear_Sequence& y, const Func1& f,
          const Func2& g, const Func3& h) {
  iterator i = begin();
  iterator last_i = begin();
  iterator i_end = end();
  const_iterator j = y.begin();
  const_iterator j_end = y.end();
  if (i != i_end) {
    if (j != j_end) {
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
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
          i = last_i;
          ++i;
          i_end = end();
          if (this == &y) {
            j = last_i;
            j_end = y.end();
          }
#endif
          ++j;
        }
    } else {
      f(i->second);
      last_i = i;
      ++i;
    }
  } else {
    if (j != j_end) {
      last_i = find_create(j->first);
      h(last_i->second, j->second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
      i = last_i;
      ++i;
      i_end = end();
      if (this == &y) {
        j = last_i;
        j_end = y.end();
      }
#endif
      ++j;
    } else {
      PPL_ASSERT(i == i_end);
      PPL_ASSERT(j == j_end);

      return;
    }
  }
  PPL_ASSERT(last_i != i_end);
  while (i != i_end && j != j_end)
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
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = end();
        if (this == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  while (i != i_end) {
    f(i->second);
    ++i;
  }
  while (j != j_end) {
    last_i = find_create(j->first, last_i);
    h(last_i->second, j->second);
    ++j;
  }
}

#else

template <typename Func1, typename Func2, typename Func3>
void
Unlimited_Sparse_Row_Over_Linear_Sequence
::combine(const Unlimited_Sparse_Row_Over_Linear_Sequence& y, const Func1& f,
          const Func2& g, const Func3& h) {
  Unlimited_Sparse_Row_Over_Linear_Sequence row;
  iterator itr = row.end();
  iterator i = begin();
  iterator i_end = end();
  const_iterator j = y.begin();
  const_iterator j_end = y.end();
  if (i == i_end && j == j_end)
    return;
  if (j == j_end
      || (i != i_end && i->first < j->first)) {
    itr = row.find_create(i->first);
    // We need to do itr->second = i->second, but this is faster.
    std::swap(itr->second, i->second);
    f(itr->second);
    ++i;
  } else
      if (i == i_end
          || (j != j_end && i->first > j->first)) {
        itr = row.find_create(j->first);
        h(itr->second, j->second);
        ++j;
      } else {
        PPL_ASSERT(i != i_end);
        PPL_ASSERT(j != j_end);
        PPL_ASSERT(i->first == j->first);
        itr = row.find_create(i->first);
        // We need to do itr->second = i->second, but this is faster.
        std::swap(itr->second, i->second);
        g(itr->second, j->second);
        ++i;
        ++j;
      }
  PPL_ASSERT(itr != row.end());
  while (i != i_end && j != j_end) {
    if (i->first < j->first) {
      itr = row.find_create(i->first, itr);
      // We need to do itr->second = i->second, but this is faster.
      std::swap(itr->second, i->second);
      f(itr->second);
      ++i;
    } else {
      if (i->first > j->first) {
        itr = row.find_create(j->first, itr);
        h(itr->second, j->second);
        ++j;
      } else {
        PPL_ASSERT(i->first == j->first);
        itr = row.find_create(i->first, itr);
        // We need to do itr->second = i->second, but this is faster.
        std::swap(itr->second, i->second);
        g(itr->second, j->second);
        ++i;
        ++j;
      }
    }
  }
  while (i != i_end) {
    itr = row.find_create(i->first, itr);
    // We need to do itr->second = i->second, but this is faster.
    std::swap(itr->second, i->second);
    f(itr->second);
    ++i;
  }
  PPL_ASSERT(i == i_end);
  while (j != j_end) {
    itr = row.find_create(j->first, itr);
    h(itr->second, j->second);
    ++j;
  }
  PPL_ASSERT(j == j_end);
  std::swap(row,*this);
}

#endif // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_Over_Linear_Sequence_templates_hh)
