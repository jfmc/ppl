/* Unlimited_Sparse_Row class implementation (non-inline functions).
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

#include <ppl-config.h>

#include "Unlimited_Sparse_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::Unlimited_Sparse_Row
::Unlimited_Sparse_Row(const std::vector<Coefficient>& v)
  : data() {
  typedef std::vector<Coefficient>::size_type vec_size_type;

  for (vec_size_type i = 0, n = v.size(); i < n; ++i)
    if (v[i] != 0)
      // We can not store data.end(), because it is invalidated by this
      // operation
      data.insert(data.end_dangerous(), std::make_pair(i, v[i]));
  PPL_ASSERT(OK());
}

inline void
PPL::Unlimited_Sparse_Row
::reset(const dimension_type first, const dimension_type last) {
  PPL_ASSERT(first <= last);
  dangerous_iterator itr = lower_bound_dangerous(first);
  dangerous_iterator itr_end = lower_bound_dangerous(last);
  if (itr == itr_end)
    return;
  if (itr_end != end_dangerous() && itr_end->first == last)
    ++itr_end;
  reset(itr, itr_end);
  PPL_ASSERT(OK());
}

void
PPL::Unlimited_Sparse_Row::delete_element_and_shift(dimension_type i) {
  dangerous_iterator itr = lower_bound_dangerous(i);
  if (itr != end_dangerous() && itr->first == i)
    itr = reset(itr);
  // We can't declare j_end before and keep using it because reset() may have
  // invalidated end() iterators.
  for (iterator j = itr, j_end = end(); j != j_end; ++j)
    --(j->first);
}

void
PPL::Unlimited_Sparse_Row
::add_zeroes_and_shift(dimension_type n,dimension_type i) {
  dangerous_iterator j = lower_bound_dangerous(i);
  dangerous_iterator j_end = end_dangerous();
  for ( ; j != j_end; ++j)
    (*j).first += n;
  PPL_ASSERT(OK());
}

bool
PPL::Unlimited_Sparse_Row::operator==(const Unlimited_Sparse_Row &x) const {
  const_iterator i = begin();
  const_iterator j = x.begin();
  const_iterator i_end = end();
  const_iterator j_end = x.end();

  while (i != i_end && j != j_end) {
    if (i->first == j->first) {
      if (i->second != j->second)
        return false;
      ++i;
      ++j;
    } else
      if (i->first < j->first) {
        if (i->second != 0)
          return false;
        ++i;
      } else {
        // i->first > j->first
        if (j->second != 0)
          return false;
        ++j;
      }
  }
  while (i != i_end) {
    if (i->second != 0)
      return false;
    ++i;
  }
  while (j != j_end) {
    if (j->second != 0)
      return false;
    ++j;
  }
  return true;
}

void
PPL::Unlimited_Sparse_Row::normalize() {
  // Compute the GCD of all the coefficients.
  const_iterator i = begin();
  const const_iterator i_end = end();
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  for ( ; i != i_end; ++i) {
    const Coefficient& x_i = i->second;
    if (const int x_i_sign = sgn(x_i)) {
      // FIXME: can this be optimized further?
      gcd = x_i;
      if (x_i_sign < 0)
        neg_assign(gcd);
      goto compute_gcd;
    }
  }
  // We reach this point only if all the coefficients were zero.
  return;

 compute_gcd:
  if (gcd == 1)
    return;
  for (++i; i != i_end; ++i) {
    const Coefficient& x_i = i->second;
    if (x_i != 0) {
      // Note: we use the ternary version instead of a more concise
      // gcd_assign(gcd, x_i) to take advantage of the fact that
      // `gcd' will decrease very rapidly (see D. Knuth, The Art of
      // Computer Programming, second edition, Section 4.5.2,
      // Algorithm C, and the discussion following it).  Our
      // implementation of gcd_assign(x, y, z) for checked numbers is
      // optimized for the case where `z' is smaller than `y', so that
      // on checked numbers we gain.  On the other hand, for the
      // implementation of gcd_assign(x, y, z) on GMP's unbounded
      // integers we cannot make any assumption, so here we draw.
      // Overall, we win.
      gcd_assign(gcd, x_i, gcd);
      if (gcd == 1)
        return;
    }
  }
  // Divide the coefficients by the GCD.
  for (iterator j = begin(), j_end = end(); j != j_end; ++j) {
    Coefficient& x_j = j->second;
    exact_div_assign(x_j, x_j, gcd);
  }
  PPL_ASSERT(OK());
}

void
PPL::Unlimited_Sparse_Row::ascii_dump(std::ostream& s) const {
  dimension_type n_elements=0;
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    ++n_elements;
  s << "elements " << n_elements << ' ';
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    s << "[ " << i->first << " ]= " << i->second << ' ';
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Unlimited_Sparse_Row)

bool
PPL::Unlimited_Sparse_Row::ascii_load(std::istream& s) {
  reset_after(0);
  std::string str;
  dimension_type n_elements;
  dimension_type current_key;
  Coefficient current_data;

  if (!(s >> str) || str!="elements")
    return false;

  if (!(s >> n_elements))
    return false;

  for (dimension_type i = 0; i < n_elements; ++i) {
    if (!(s >> str) || str!="[")
      return false;
    if (!(s >> current_key))
      return false;
    if (!(s >> str) || str!="]=")
      return false;
    if (!(s >> current_data))
      return false;
    data.push_back(std::make_pair(current_key, current_data));
  }
  PPL_ASSERT(OK());
  return true;
}

bool
PPL::Unlimited_Sparse_Row::OK() const {
  if (!data.OK())
    return false;
  if (begin() == end())
    return true;
  const_iterator previous = begin();
  const_iterator i = begin();
  ++i;
  const_iterator i_end = end();
  for ( ; i != i_end; ++i, ++previous)
    if (previous->first >= i->first)
      return false;
  return true;
}
