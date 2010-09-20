/* Sparse_Row class implementation (non-inline functions).
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

#include "Sparse_Row.defs.hh"
#include "Dense_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

namespace {

class Sparse_Row_from_Dense_Row_helper_iterator {
public:
  Sparse_Row_from_Dense_Row_helper_iterator(const PPL::Dense_Row& row1,
                                            PPL::dimension_type i1)
    : row(row1), i(i1) {
    PPL_ASSERT(i <= row.size());
    if (i < row.size() && row[i] == 0)
      ++(*this);
  }

  Sparse_Row_from_Dense_Row_helper_iterator& operator++() {
    PPL_ASSERT(i < row.size());
    ++i;
    while (i < row.size() && row[i] == 0)
      ++i;
    return *this;
  }

  Sparse_Row_from_Dense_Row_helper_iterator operator++(int) {
    Sparse_Row_from_Dense_Row_helper_iterator tmp = *this;
    ++(*this);
    return tmp;
  }

  PPL::Coefficient_traits::const_reference
  operator*() const {
    PPL_ASSERT(i < row.size());
    return row[i];
  }

  PPL::dimension_type
  index() const {
    PPL_ASSERT(i < row.size());
    return i;
  }

  bool
  operator==(const Sparse_Row_from_Dense_Row_helper_iterator& itr) const {
    PPL_ASSERT(&row == &(itr.row));
    return i == itr.i;
  }

  bool
  operator!=(const Sparse_Row_from_Dense_Row_helper_iterator& itr) const {
    return !(*this == itr);
  }

private:
  const PPL::Dense_Row& row;
  PPL::dimension_type i;
};

// Returns the number of nonzero elements in row.
PPL::dimension_type
Sparse_Row_from_Dense_Row_helper_function(const PPL::Dense_Row& row) {
  PPL::dimension_type count = 0;
  for (PPL::dimension_type i = row.size(); i-- > 0; )
    if (row[i] != 0)
      ++count;
  return count;
}

} // namespace

PPL::Sparse_Row::Sparse_Row(const PPL::Dense_Row& row)
  : tree(Sparse_Row_from_Dense_Row_helper_iterator(row, 0),
         Sparse_Row_from_Dense_Row_helper_iterator(row, row.size()),
         Sparse_Row_from_Dense_Row_helper_function(row)),
    size_(row.size()) {
  PPL_ASSERT(OK());
}

bool
PPL::Sparse_Row::operator==(const Sparse_Row& row) const {
  if (flags() != row.flags())
    return false;
  if (size() != row.size())
    return false;
  const_iterator i = begin();
  const_iterator i_end = end();
  const_iterator j = row.begin();
  const_iterator j_end = row.end();
  while (i != i_end && j != j_end) {
    if (*i == 0) {
      ++i;
      continue;
    }
    if (*j == 0) {
      ++j;
      continue;
    }
    if (i.index() != j.index() || *i != *j)
      return false;
    ++i;
    ++j;
  }
  for ( ; i != i_end; ++i)
    if (*i != 0)
      return false;
  for ( ; j != j_end; ++j)
    if (*j != 0)
      return false;
  return true;
}

void
PPL::Sparse_Row::swap(dimension_type i, dimension_type j) {
  PPL_ASSERT(i < size_);
  PPL_ASSERT(j < size_);

  if (tree.empty())
    return;

  iterator itr_i = tree.bisect(i);
  iterator itr_j = tree.bisect(j);
  if (itr_i.index() == i)
    if (itr_j.index() == j)
      // Both elements are in the tree
      std::swap(*itr_i, *itr_j);
    else {
      // i is in the tree, j isn't
      PPL_DIRTY_TEMP_COEFFICIENT(tmp);
      std::swap(*itr_i, tmp);
      tree.erase(itr_i);
      // Now both iterators have been invalidated.
      itr_j = tree.insert(j);
      std::swap(*itr_j, tmp);
    }
  else
    if (itr_j.index() == j) {
      // j is in the tree, i isn't
      PPL_DIRTY_TEMP_COEFFICIENT(tmp);
      std::swap(*itr_j, tmp);
      // Now both iterators have been invalidated.
      tree.erase(itr_j);
      itr_i = tree.insert(i);
      std::swap(*itr_i, tmp);
    } else {
      // Do nothing, elements are both unstored zeroes.
    }
}

PPL::Sparse_Row::iterator
PPL::Sparse_Row::reset(iterator first, iterator last) {
  if (first == last)
    return first;
  PPL_ASSERT(last != end());
  --last;
  const dimension_type j = last.index();
  PPL_ASSERT(first.index() <= j);
  // We can't just compare first and last at each iteration, because last will
  // be invalidated by the first erase.
  while (first.index() < j)
    first = reset(first);

  first = reset(first);

  PPL_ASSERT(OK());
  return first;
}

void
PPL::Sparse_Row::reset_after(dimension_type i) {
  PPL_ASSERT(i < size_);

  iterator itr = lower_bound(i);
  // This is a const reference to an internal iterator, that is kept valid.
  // If we just stored a copy, that would be invalidated by the calls to
  // reset().
  const iterator& itr_end = end();

  while (itr != itr_end)
    itr = reset(itr);

  PPL_ASSERT(OK());
}

void
PPL::Sparse_Row::normalize() {
  // Compute the GCD of all the coefficients.
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  const_iterator i = begin();
  const_iterator i_end = end();
  for ( ; i != i_end; ++i) {
    Coefficient_traits::const_reference x_i = *i;
    if (const int x_i_sign = sgn(x_i)) {
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
    Coefficient_traits::const_reference x_i = *i;
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
    Coefficient& x_j = *j;
    exact_div_assign(x_j, x_j, gcd);
  }

  PPL_ASSERT(OK());
}

void
PPL::Sparse_Row::normalize(Coefficient& gcd) {
  // Compute the GCD of all the coefficients.
  const_iterator i = begin();
  const_iterator i_end = end();
  for ( ; i != i_end; ++i) {
    Coefficient_traits::const_reference x_i = *i;
    if (const int x_i_sign = sgn(x_i)) {
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
    Coefficient_traits::const_reference x_i = *i;
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
    Coefficient& x_j = *j;
    exact_div_assign(x_j, x_j, gcd);
  }

  PPL_ASSERT(OK());
}

void
PPL::Sparse_Row::linear_combine(const Sparse_Row& y,
                                Coefficient_traits::const_reference coeff1,
                                Coefficient_traits::const_reference coeff2) {
  PPL_ASSERT(coeff1 != 0);
  PPL_ASSERT(coeff2 != 0);
  PPL_ASSERT(this != &y);
  if (coeff1 == 1) {
    iterator i = end();
    for (const_iterator j = y.begin(), j_end = y.end(); j != j_end; ++j) {
      i = insert(i, j.index());
      add_mul_assign(*i, *j, coeff2);
      if (*i == 0)
        i = reset(i);
    }
  } else {
    iterator i = begin();
    // This is a const reference to an internal iterator, that is kept valid.
    // If we just stored a copy, that would be invalidated by the calls to
    // reset() and insert().
    const iterator& i_end = end();
    const_iterator j = y.begin();
    const_iterator j_end = y.end();
    while (i != i_end && j != j_end) {
      if (i.index() == j.index()) {
        (*i) *= coeff1;
        add_mul_assign(*i, *j, coeff2);
        if (*i == 0)
          i = reset(i);
        else
          ++i;
        ++j;
      } else
        if (i.index() < j.index()) {
          (*i) *= coeff1;
          ++i;
        } else {
          PPL_ASSERT(i.index() > j.index());
          i = insert(i, j.index(), *j);
          (*i) *= coeff2;
          ++i;
          ++j;
        }
    }
    PPL_ASSERT(i == i_end || j == j_end);
    for ( ; i != i_end; ++i)
      (*i) *= coeff1;
    for ( ; j != j_end; ++j) {
      i = insert(i, j.index(), *j);
      (*i) *= coeff2;
    }
  }
}

void
PPL::Sparse_Row::ascii_dump(std::ostream& s) const {
  s << "size " << size_ << ' ';
  dimension_type n_elements = 0;
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    ++n_elements;
  s << "elements " << n_elements << ' ';
  for (const_iterator i = begin(), i_end = end(); i != i_end; ++i)
    s << "[ " << i.index() << " ]= " << *i << ' ';
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Sparse_Row)

bool
PPL::Sparse_Row::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  if (!(s >> size_))
    return false;
  clear();
  dimension_type n_elements;
  dimension_type current_key;
  Coefficient current_data;

  if (!(s >> str) || str != "elements")
    return false;

  if (!(s >> n_elements))
    return false;

  for (dimension_type i = 0; i < n_elements; ++i) {
    if (!(s >> str) || str != "[")
      return false;
    if (!(s >> current_key))
      return false;
    if (!(s >> str) || str != "]=")
      return false;
    if (!(s >> current_data))
      return false;
    tree.insert(current_key, current_data);
  }
  PPL_ASSERT(OK());
  return true;
}

bool
PPL::Sparse_Row::OK() const {
  if (begin() == end())
    return true;
  const_iterator last = end();
  --last;
  return (last.index() < size_);
}
