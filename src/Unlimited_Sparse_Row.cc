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

void
PPL::Unlimited_Sparse_Row
::ascii_dump(std::ostream& s) const {
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
PPL::Unlimited_Sparse_Row
::ascii_load(std::istream& s) {
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
    tree.insert(current_key, current_data);
  }
  PPL_ASSERT(OK());
  return true;
}

void
PPL::Unlimited_Sparse_Row::normalize() {
  // Compute the GCD of all the coefficients.
  unordered_const_iterator i = unordered_begin();
  unordered_const_iterator i_end = unordered_end();
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
  for (unordered_iterator j = unordered_begin(), j_end = unordered_end();
       j != j_end; ++j) {
    Coefficient& x_j = j->second;
    exact_div_assign(x_j, x_j, gcd);
  }
  PPL_ASSERT(OK());
}

