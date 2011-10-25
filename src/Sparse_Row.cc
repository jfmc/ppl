/* Sparse_Row class implementation (non-inline functions).
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

#include <ppl-config.h>

#include "Sparse_Row.defs.hh"
#include "Dense_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

namespace {

class Sparse_Row_from_Dense_Row_helper_iterator {
public:
  Sparse_Row_from_Dense_Row_helper_iterator(const PPL::Dense_Row& row1)
    : row(row1), i(0) {
    if (row.size() != 0 && row[0] == 0)
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
  : tree(Sparse_Row_from_Dense_Row_helper_iterator(row),
         Sparse_Row_from_Dense_Row_helper_function(row)),
    size_(row.size()),
    flags_(row.flags()) {
  PPL_ASSERT(OK());
}

PPL::Sparse_Row&
PPL::Sparse_Row::operator=(const PPL::Dense_Row& row) {
  CO_Tree tmp_tree(Sparse_Row_from_Dense_Row_helper_iterator(row),
                   Sparse_Row_from_Dense_Row_helper_function(row));
  std::swap(tree, tmp_tree);
  size_ = row.size();
  flags_ = row.flags();
  PPL_ASSERT(OK());

  return *this;
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

namespace {

class sparse_row_linear_combine_helper_iterator {
public:
  sparse_row_linear_combine_helper_iterator(
    const PPL::Sparse_Row& x, const PPL::Sparse_Row& y,
    PPL::Coefficient_traits::const_reference coeff1_1,
    PPL::Coefficient_traits::const_reference coeff2_1)
    : coeff1(coeff1_1), coeff2(coeff2_1) {
    i = x.begin();
    i_end = x.end();
    j = y.begin();
    j_end = y.end();
    update_current_data();
  }

  void operator++() {
    if (from_i)
      ++i;
    if (from_j)
      ++j;
    update_current_data();
  }

  PPL::Coefficient_traits::const_reference operator*() {
    return current_value;
  }

  PPL::dimension_type index() {
    return current_index;
  }

private:
  void update_current_data() {
    if (i == i_end) {
      if (j == j_end) {
        return;
      } else {
        // i == i_end, j != j_end, so use j.
        current_index = j.index();
        current_value = *j;
        current_value *= coeff2;
        from_i = false;
        from_j = true;
      }
    } else {
      if (j == j_end) {
        // i != i_end, j == j_end, so use i.
        current_index = i.index();
        current_value = *i;
        current_value *= coeff1;
        from_i = true;
        from_j = false;
      } else {
        // i != i_end and j != j_end.
        if (i.index() < j.index()) {
          // i.index() < j.index(), so use i.
          current_index = i.index();
          current_value = *i;
          current_value *= coeff1;
          from_i = true;
          from_j = false;
        } else {
          if (i.index() != j.index()) {
            PPL_ASSERT(i.index() > j.index());
            // i.index() > j.index(), so use j.
            current_index = j.index();
            current_value = *j;
            current_value *= coeff2;
            from_i = false;
            from_j = true;
          } else {
            // i.index() == j.index(), so use both i and j.
            current_index = i.index();
            current_value = *i;
            current_value *= coeff1;
            PPL::add_mul_assign(current_value, *j, coeff2);
            from_i = true;
            from_j = true;
          }
        }
      }
    }
    PPL_ASSERT(!from_i || i != i_end);
    PPL_ASSERT(!from_j || j != j_end);
  }

  PPL::Coefficient_traits::const_reference coeff1;
  PPL::Coefficient_traits::const_reference coeff2;
  PPL::Sparse_Row::const_iterator i;
  PPL::Sparse_Row::const_iterator i_end;
  PPL::Sparse_Row::const_iterator j;
  PPL::Sparse_Row::const_iterator j_end;
  PPL::dimension_type current_index;
  PPL::Coefficient current_value;
  bool from_i;
  bool from_j;
};

} // namespace

void
PPL::Sparse_Row::linear_combine(const Sparse_Row& y,
                                Coefficient_traits::const_reference coeff1,
                                Coefficient_traits::const_reference coeff2) {
  PPL_ASSERT(coeff1 != 0);
  PPL_ASSERT(coeff2 != 0);
  PPL_ASSERT(this != &y);

  if (coeff1 == 1) {
    // Optimize for this special case.
    iterator i = end();
    for (const_iterator j = y.begin(), j_end = y.end(); j != j_end; ++j) {
      i = insert(i, j.index());
      add_mul_assign(*i, *j, coeff2);
      if (*i == 0)
        i = reset(i);
    }
    return;
  }

  dimension_type counter = 0;
  // Count the number of elements that are stored in y but not in *this.
  {
    iterator i = begin();
    iterator i_end = end();
    const_iterator j = y.begin();
    const_iterator j_end = y.end();
    if (i != i_end) {
      while (j != j_end) {
        PPL_ASSERT(i != i_end);
        if (i.index() == j.index()) {
          ++i;
          ++j;
          if (i == i_end)
            break;
        } else
          if (i.index() < j.index()) {
            i = lower_bound(i, j.index());
            if (i == i_end)
              break;
          } else {
            PPL_ASSERT(i.index() > j.index());
            ++counter;
            ++j;
          }
      }
    }
    PPL_ASSERT(i == i_end || j == j_end);
    for ( ; j != j_end; ++j)
      ++counter;
  }
  // This condition is arbitrary. Changing it affects performance but not
  // correctness. The values have been tuned using some ppl_lpsol benchmarks
  // on 2 October 2010.
  if (counter == 0 || counter < 7 * size() / 64) {
    // Few insertions needed, do them directly.
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
  } else {
    // Too many insertions needed, a full copy is probably faster than
    // inserting all those new elements into *this.
    CO_Tree new_tree(sparse_row_linear_combine_helper_iterator(*this, y,
                                                                coeff1,
                                                                coeff2),
                     counter + tree.size());
    std::swap(tree, new_tree);

    // Now remove stored zeroes.
    iterator i = begin();
    // Note that end() can not be called only once, because reset()
    // invalidates all iterators.
    while (i != end()) {
      if (*i == 0) {
#ifndef NDEBUG
        const dimension_type old_index = i.index();
#endif
        i = reset(i);
        PPL_ASSERT(find(old_index) == end());
      } else
        ++i;
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

bool
PPL::Sparse_Row::OK(dimension_type /* capacity */) const {
  return OK();
}

bool
PPL::operator==(const Sparse_Row& x, const Sparse_Row& y) {
  if (x.size() != y.size())
    return false;
  Sparse_Row::const_iterator i = x.begin();
  Sparse_Row::const_iterator i_end = x.end();
  Sparse_Row::const_iterator j = y.begin();
  Sparse_Row::const_iterator j_end = y.end();
  while (i != i_end && j != j_end) {
    if (i.index() == j.index()) {
      if (*i != *j)
        return false;
      ++i;
      ++j;
    } else {
      if (i.index() < j.index()) {
        if (*i != 0)
          return false;
        ++i;
      } else {
        PPL_ASSERT(i.index() > j.index());
        if (*j != 0)
          return false;
        ++j;
      }
    }
  }
  for ( ; i != i_end; ++i)
    if (*i != 0)
      return false;
  for ( ; j != j_end; ++j)
    if (*j != 0)
      return false;
  return true;
}

bool
PPL::operator!=(const Sparse_Row& x, const Sparse_Row& y) {
  return !(x == y);
}
