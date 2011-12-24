/* Sorting objects for which copies cost more than swaps.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#ifndef PPL_swapping_sort_templates_hh
#define PPL_swapping_sort_templates_hh 1

#include <iterator>
#include <algorithm>

namespace Parma_Polyhedra_Library {

namespace Implementation {

/*
  This sorting algorithm avoids (almost all) copies of objects
  by performing more iter_swap's: it is meant to be used when
  object copying costs much more than object swapping.

  Moreover, the algorithm always uses iter_swap() instead of
  swap() so as to behave as expected when instantiated on
  Linear_System::With_Bit_Matrix_iterator. Namely, using a sorting
  routine that either copies objects or directly swaps them
  (i.e., without calling iter_swap) would not be correct
  when using Linear_System::With_Bit_Matrix_iterator.
*/

template <typename Value_Type, typename Compare>
inline const Value_Type&
median(const Value_Type& x, const Value_Type& y, const Value_Type& z,
       Compare comp) {
  return comp(x, y)
    ? (comp(y, z) ? y : (comp(x, z) ? z : x))
    : (comp(x, z) ? x : (comp(y, z) ? z : y));
}

template <typename Iter, typename Value_Type, typename Compare>
Iter
swapping_partition(Iter first, Iter last, const Value_Type& pivot,
		   Compare comp) {
  for ( ; ; ) {
    while (comp(*first, pivot))
      ++first;
    --last;
    while (comp(pivot, *last))
      --last;
    if (first < last) {
      using std::iter_swap;
      iter_swap(first, last);
      ++first;
    }
    else
      return first;
  }
}

template <typename Iter, typename Compare>
void
swapping_insertion_sort(Iter first, Iter last, Compare comp) {
  if (first == last)
    return;
  for (Iter i = first + 1; i != last; ++i) {
    using std::iter_swap;
    Iter current = i;
    if (comp(*current, *first)) {
      Iter next = current + 1;
      while (current != first)
	iter_swap(--current, --next);
    }
    else {
      Iter previous = current - 1;
      while (comp(*current, *previous))
	iter_swap(current--, previous--);
    }
  }
}

template <typename Iter, typename Compare>
void
swapping_quicksort_loop(Iter first, Iter last, Compare comp) {
  const typename std::iterator_traits<Iter>::difference_type threshold = 16;
  while (last - first > threshold) {
    // The construction of this temporary object is
    // required for the correctness of the algorithm.
    Iter middle = first + (last - first) / 2;
    typename std::iterator_traits<Iter>::value_type
      pivot = median(*first, *middle, *(last - 1), comp);
    Iter part_point = swapping_partition(first, last, pivot, comp);
    swapping_quicksort_loop(part_point, last, comp);
    last = part_point;
  }
}

template <typename Iter, typename Compare>
inline void
swapping_sort(Iter first, Iter last, Compare comp) {
  if (first == last)
    return;
  swapping_quicksort_loop(first, last, comp);
  swapping_insertion_sort(first, last, comp);
}

template <typename Iter>
Iter
swapping_unique(Iter first, Iter last) {
  if (first == last)
    return last;
  Iter current = first;
  Iter next = current;
  ++next;
  while(next != last && *current != *next) {
    current = next;
    ++next;
  }
  if (next == last)
    return last;
  ++next;
  while (next != last) {
    if (*current != *next) {
      using std::iter_swap;
      iter_swap(++current, next);
    }
    ++next;
  }
  return ++current;
}

} // namespace Implementation

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_swapping_sort_templates_hh)
