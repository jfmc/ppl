/* Linear_System class implementation (non-inline functions).
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>

#include "Linear_System.defs.hh"

#include "Integer.defs.hh"
#include "Row.defs.hh"
#include "globals.defs.hh"
#include "SatMatrix.defs.hh"
#include <algorithm>
#include <iostream>
#include <string>
#include <deque>

#include "swapping_sort.icc"

namespace PPL = Parma_Polyhedra_Library;

PPL::dimension_type
PPL::Linear_System::num_lines_or_equalities() const {
  assert(num_pending_rows() == 0);
  const Linear_System& x = *this;
  dimension_type n = 0;
  for (dimension_type i = num_rows(); i-- > 0; )
    if (x[i].is_line_or_equality())
      ++n;
  return n;
}

void
PPL::Linear_System::merge_rows_assign(const Linear_System& y) {
  assert(row_size >= y.row_size);
  // Both systems have to be sorted and have no pending rows.
  assert(check_sorted() && y.check_sorted());
  assert(num_pending_rows() == 0 && y.num_pending_rows() == 0);

  Linear_System& x = *this;

  // A temporary vector of rows...
  std::vector<Row> tmp;
  // ... with enough capacity not to require any reallocations.
  tmp.reserve(compute_capacity(x.num_rows() + y.num_rows(), max_num_rows()));

  dimension_type xi = 0;
  dimension_type x_num_rows = x.num_rows();
  dimension_type yi = 0;
  dimension_type y_num_rows = y.num_rows();

  while (xi < x_num_rows && yi < y_num_rows) {
    const int comp = compare(x[xi], y[yi]);
    if (comp <= 0) {
      // Elements that can be taken from `x' are actually _stolen_ from `x'
      std::swap(x[xi++], *tmp.insert(tmp.end(), Linear_Row()));
      if (comp == 0)
	// A duplicate element.
	++yi;
    }
    else {
      // (comp > 0)
      Linear_Row copy(y[yi++], row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Linear_Row()));
    }
  }
  // Insert what is left.
  if (xi < x_num_rows)
    while (xi < x_num_rows)
      std::swap(x[xi++], *tmp.insert(tmp.end(), Linear_Row()));
  else
    while (yi < y_num_rows) {
      Linear_Row copy(y[yi++], row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Linear_Row()));
    }

  // We get the result vector and let the old one be destroyed.
  std::swap(tmp, rows);
  // There are no pending rows.
  unset_pending_rows();
  assert(check_sorted());
}

void
PPL::Linear_System::set_rows_topology() {
  Linear_System& x = *this;
  if (is_necessarily_closed())
    for (dimension_type i = num_rows(); i-- > 0; )
      x[i].set_necessarily_closed();
  else
    for (dimension_type i = num_rows(); i-- > 0; )
      x[i].set_not_necessarily_closed();
}

void
PPL::Linear_System::ascii_dump(std::ostream& s) const {
  const Linear_System& x = *this;
  dimension_type x_num_rows = x.num_rows();
  dimension_type x_num_columns = x.num_columns();
  s << "topology " << (is_necessarily_closed()
		       ? "NECESSARILY_CLOSED"
		       : "NOT_NECESSARILY_CLOSED")
    << std::endl
    << x_num_rows << " x " << x_num_columns
    << (x.sorted ? "(sorted)" : "(not_sorted)")
    << std::endl
    << "index_first_pending " << x.first_pending_row()
    << std::endl;
  for (dimension_type i = 0; i < x_num_rows; ++i)
    x[i].ascii_dump(s);
}

bool
PPL::Linear_System::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;
  if (str == "NECESSARILY_CLOSED")
    set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
      return false;
  resize_no_copy(nrows, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  set_sorted(str == "(sorted)");
  dimension_type index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> index))
    return false;
  set_index_first_pending_row(index);

  // FIXME: must be completed.

  // Check for well-formedness.
  assert(OK(true));
  return true;
}

void
PPL::Linear_System::insert(const Linear_Row& r) {
  // The added row must be strongly normalized and have the same
  // topology of the system.
  assert(r.check_strong_normalized());
  assert(topology() == r.topology());
  // This method is only used when the system has no pending rows.
  assert(num_pending_rows() == 0);

  const dimension_type old_num_rows = num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type r_size = r.size();

  // Resize the system, if necessary.
  if (r_size > old_num_columns) {
    add_zero_columns(r_size - old_num_columns);
    if (!is_necessarily_closed() && old_num_rows != 0)
      // Move the epsilon coefficients to the last column
      // (note: sorting is preserved).
      swap_columns(old_num_columns - 1, r_size - 1);
    add_row(r);
  }
  else if (r_size < old_num_columns)
    if (is_necessarily_closed() || old_num_rows == 0)
      add_row(Linear_Row(r, old_num_columns, row_capacity));
    else {
      // Create a resized copy of the row (and move the epsilon
      // coefficient to its last position).
      Linear_Row tmp_row(r, old_num_columns, row_capacity);
      std::swap(tmp_row[r_size - 1], tmp_row[old_num_columns - 1]);
      add_row(tmp_row);
    }
  else
    // Here r_size == old_num_columns.
    add_row(r);

  // The added row was not a pending row.
  assert(num_pending_rows() == 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::insert_pending(const Linear_Row& r) {
  // The added row must be strongly normalized and have
  // the same topology of the system.
  assert(r.check_strong_normalized());
  assert(topology() == r.topology());

  const dimension_type old_num_rows = num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type r_size = r.size();

  // Resize the system, if necessary.
  if (r_size > old_num_columns) {
    add_zero_columns(r_size - old_num_columns);
    if (!is_necessarily_closed() && old_num_rows != 0)
      // Move the epsilon coefficients to the last column
      // (note: sorting is preserved).
      swap_columns(old_num_columns - 1, r_size - 1);
    add_pending_row(r);
  }
  else if (r_size < old_num_columns)
    if (is_necessarily_closed() || old_num_rows == 0)
      add_pending_row(Linear_Row(r, old_num_columns, row_capacity));
    else {
      // Create a resized copy of the row (and move the epsilon
      // coefficient to its last position).
      Linear_Row tmp_row(r, old_num_columns, row_capacity);
      std::swap(tmp_row[r_size - 1], tmp_row[old_num_columns - 1]);
      add_pending_row(tmp_row);
    }
  else
    // Here r_size == old_num_columns.
    add_pending_row(r);

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

#if 0
void
PPL::Linear_System::merge_rows_assign(const Linear_System& y) {
  assert(row_size >= y.row_size);
  assert(check_sorted() && y.check_sorted());
  // We can use this method only when the matrices do not
  // contain any pending rows.
  assert(num_pending_rows() == 0 && y.num_pending_rows() == 0);

  Linear_System& x = *this;

  // A temporary vector of rows...
  std::vector<Row> tmp;
  // ... with enough capacity not to require any reallocations.
  tmp.reserve(compute_capacity(x.num_rows() + y.num_rows(), max_num_rows()));

  std::vector<Row>::iterator xi = x.rows.begin();
  const std::vector<Row>::iterator xend = x.rows.end();
  std::vector<Row>::const_iterator yi = y.rows.begin();
  const std::vector<Row>::const_iterator yend = y.rows.end();

  while (xi != xend && yi != yend) {
    const int comp = compare(*xi, *yi);
    if (comp <= 0) {
      // Elements that can be taken from `x' are actually _stolen_ from `x'
      std::swap(*xi++, *tmp.insert(tmp.end(), Row()));
      if (comp == 0)
	// A duplicate element.
	++yi;
    }
    else {
      // (comp > 0)
      Row copy(*yi++, row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Row()));
    }
  }
  // Insert what is left.
  if (xi != xend)
    while (xi != xend)
      std::swap(*xi++, *tmp.insert(tmp.end(), Row()));
  else
    while (yi != yend) {
      Row copy(*yi++, row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Row()));
    }

  // We get the result vector and let the old one be destroyed.
  std::swap(tmp, rows);
  // There are no pending rows.
  unset_pending_rows();
  assert(check_sorted());
}
#endif

void
PPL::Linear_System::add_pending_rows(const Linear_System& y) {
  Linear_System& x = *this;
  assert(x.row_size >= y.row_size);

  const dimension_type x_n_rows = x.num_rows();
  const dimension_type y_n_rows = y.num_rows();
  // Grow to the required size without changing sortedness.
  const bool was_sorted = sorted;
  add_zero_rows(y_n_rows, Linear_Row::Flags(row_topology));
  sorted = was_sorted;

  // Copy the rows of `y', forcing size and capacity.
  for (dimension_type i = y_n_rows; i-- > 0; ) {
    Row copy(y[i], x.row_size, x.row_capacity);
    std::swap(copy, x[x_n_rows+i]);
  }
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::add_rows(const Linear_System& y) {
  assert(num_pending_rows() == 0);

  // Adding no rows is a no-op.
  if (y.num_rows() == 0)
    return;

  // Check if sortedness is preserved.
  if (is_sorted())
    if (y.is_sorted() && y.num_pending_rows() == 0) {
      const dimension_type n_rows = num_rows();
    if (n_rows > 0)
      set_sorted(compare((*this)[n_rows-1], y[0]) <= 0);
  }

  // Add the rows of `y' as if they were pending.
  add_pending_rows(y);
  // There are no pending_rows.
  unset_pending_rows();

  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::sort_rows() {
  const dimension_type num_pending = num_pending_rows();
  // We sort the non-pending rows only.
  sort_rows(0, first_pending_row());
  set_index_first_pending_row(num_rows() - num_pending);
  sorted = true;
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::sort_rows(const dimension_type first_row,
			      const dimension_type last_row) {
  assert(first_row <= last_row && last_row <= num_rows());
  // We cannot mix pending and non-pending rows.
  assert(first_row >= first_pending_row() || last_row <= first_pending_row());

  // First sort without removing duplicates.
  std::vector<Row>::iterator first = rows.begin() + first_row;
  std::vector<Row>::iterator last = rows.begin() + last_row;
  swapping_sort(first, last, Row_Less_Than());
  // Second, move duplicates to the end.
  std::vector<Row>::iterator new_last = swapping_unique(first, last);
  // Finally, remove duplicates.
  rows.erase(new_last, last);
  // NOTE: we cannot check for well-formedness of the system here,
  // because the caller still has to update `index_first_pending'.
}

void
PPL::Linear_System::add_row(const Linear_Row& r) {
  // The added row must be strongly normalized and have
  // the same number of elements of the existing rows of the system.
  assert(r.check_strong_normalized());
  assert(r.size() == row_size);
  // This method is only used when the system has no pending rows.
  assert(num_pending_rows() == 0);
  const bool was_sorted = is_sorted();
  const dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size, max_num_rows()));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(r, row_capacity);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new rows into place.
    std::swap(rows, new_rows);
  }
  else {
    // Reallocation will NOT take place.
    // Inserts a new empty row at the end,
    // then substitutes it with a copy of the given row.
    Row tmp(r, row_capacity);
    std::swap(*rows.insert(rows.end(), Row()), tmp);
  }

  //  We update `index_first_pending', because it must
  // equal to `num_rows()'.
  set_index_first_pending_row(num_rows());

  if (was_sorted) {
    const dimension_type nrows = num_rows();
    // The added row may have caused the system to be not sorted anymore.
    if (nrows > 1) {
      // If the system is not empty and the inserted row
      // is the greatest one, the system is set to be sorted.
      // If it is not the greatest one then the system is no longer sorted.
      Linear_System& x = *this;
      set_sorted(compare(x[nrows-2], x[nrows-1]) <= 0);
    }
    else
      // A system having only one row is sorted.
      set_sorted(true);
  }
  // The added row was not a pending row.
  assert(num_pending_rows() == 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::add_pending_row(const Linear_Row& r) {
  // The added row must be strongly normalized and have
  // the same number of elements of the existing rows of the system.
  assert(r.check_strong_normalized());
  assert(r.size() == row_size);

  const dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size, max_num_rows()));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(r, row_capacity);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new rows into place.
    std::swap(rows, new_rows);
  }
  else {
    // Reallocation will NOT take place.
    // Inserts a new empty row at the end,
    // then substitutes it with a copy of the given row.
    Row tmp(r, row_capacity);
    std::swap(*rows.insert(rows.end(), Row()), tmp);
  }

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

void
PPL::Linear_System::add_pending_row(const Linear_Row::Flags flags) {
  const dimension_type new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size, max_num_rows()));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Linear_Row new_row(row_size, row_capacity, flags);
    dimension_type i = new_rows_size-1;
    std::swap(new_rows[i], new_row);
    // Steal the old rows.
    while (i-- > 0)
      new_rows[i].swap(rows[i]);
    // Put the new vector into place.
    std::swap(rows, new_rows);
  }
  else
    // Reallocation will NOT take place.
    // Insert a new empty row at the end,
    // then construct it assigning it the given type.
    // FIXME: this is a kludge
    static_cast<Linear_Row&>(*rows.insert(rows.end(), Row()))
      .construct(row_size, row_capacity, flags);

  // The added row was a pending row.
  assert(num_pending_rows() > 0);
}

void
PPL::Linear_System::normalize() {
  Linear_System& x = *this;
  // We normalize also the pending rows.
  for (dimension_type i = num_rows(); i-- > 0; )
    x[i].normalize();
  set_sorted(false);
}

void
PPL::Linear_System::strong_normalize() {
  Linear_System& x = *this;
  // We strongly normalize also the pending rows.
  for (dimension_type i = num_rows(); i-- > 0; )
    x[i].strong_normalize();
  set_sorted(false);
}

void
PPL::Linear_System::sign_normalize() {
  Linear_System& x = *this;
  // We sign-normalize also the pending rows.
  for (dimension_type i = num_rows(); i-- > 0; )
    x[i].sign_normalize();
  set_sorted(false);
}

/*! \relates Parma_Polyhedra_Library::Linear_System */
bool
PPL::operator==(const Linear_System& x, const Linear_System& y) {
  if (x.num_columns() != y.num_columns())
    return false;
  const dimension_type x_num_rows = x.num_rows();
  const dimension_type y_num_rows = y.num_rows();
  if (x_num_rows != y_num_rows)
    return false;
  if (x.first_pending_row() != y.first_pending_row())
    return false;
  // Notice that calling operator==(const Matrix&, const Matrix&)
  // would be wrong here, as equality of the type fields would
  // not be checked.
  for (dimension_type i = x_num_rows; i-- > 0; )
    if (x[i] != y[i])
      return false;
  return true;
}

void
PPL::Linear_System::sort_and_remove_with_sat(SatMatrix& sat) {
  Linear_System& sys = *this;
  // We can only sort the non-pending part of the system.
  assert(sys.first_pending_row() == sat.num_rows());
  if (sys.first_pending_row() <= 1) {
    sys.set_sorted(true);
    return;
  }

  // First, sort `sys' (keeping `sat' consistent) without removing duplicates.
  With_SatMatrix_iterator first(sys.rows.begin(), sat.rows.begin());
  With_SatMatrix_iterator last = first + sat.num_rows();
  swapping_sort(first, last, Row_Less_Than());
  // Second, move duplicates in `sys' to the end (keeping `sat' consistent).
  With_SatMatrix_iterator new_last = swapping_unique(first, last);

  const dimension_type num_duplicates = last - new_last;
  const dimension_type new_first_pending_row
    = sys.first_pending_row() - num_duplicates;

  if (sys.num_pending_rows() > 0) {
    // In this case, we must put the duplicates after the pending rows.
    const dimension_type n_rows = sys.num_rows() - 1;
    for (dimension_type i = 0; i < num_duplicates; ++i)
      std::swap(sys[new_first_pending_row + i], sys[n_rows - i]);
  }
  // Erasing the duplicated rows...
  sys.erase_to_end(sys.num_rows() - num_duplicates);
  sys.set_index_first_pending_row(new_first_pending_row);
  // ... and the corresponding rows of the saturation matrix.
  sat.rows_erase_to_end(sat.num_rows() - num_duplicates);
  assert(sys.check_sorted());
  // Now the system is sorted.
  sys.set_sorted(true);
}

void
PPL::Linear_System::gram_shmidt() {
  assert(num_pending_rows() == 0);

  // The first part of this algorithm is is an adaptation of the one
  // proposed in a 1996 TR by Erlingsson, Kaltofen, and Musser
  // "Generic Gram-Shmidt Orthogonalization by Exact Division".

  // It is assumed that the lines/equalities come first in the system,
  // which contains no redundant row.

  const dimension_type rank = num_lines_or_equalities();
  if (rank == 0)
    return;

#if 0
  std::cout << "+++ Before Gram-Shmidt +++" << std::endl;
  ascii_dump(std::cout);
#endif

  static std::vector<std::vector<Integer> > mu;
  mu.reserve(compute_capacity(rank, mu.max_size()));
  for (dimension_type i = mu.size(); i < rank; i++) {
    std::vector<Integer> mu_i(i+1);
    mu.push_back(mu_i);
  }

  Linear_System& x = *this;

  // Compute the scalar products `x[i]*x[j]',
  // for all 0 <= j <= i < rank, storing them into `mu[i][j]'.
  for (dimension_type i = rank; i-- > 0; ) {
    const Linear_Row& x_i = x[i];
    std::vector<Integer>& mu_i = mu[i];
    for (dimension_type j = i+1; j-- > 0; )
      scalar_product_assign(mu_i[j], x_i, x[j]);
  }

  const dimension_type n_columns = num_columns();

  TEMP_INTEGER(accum);
  // Start from the second line/equality of the system.
  for (dimension_type i = 1; i < rank; i++) {
    Linear_Row& x_i = x[i];
    std::vector<Integer>& mu_i = mu[i];

    // Finish computing `mu[i][j]', for all j <= i.
    for (dimension_type j = 0; j <= i; j++) {
      const std::vector<Integer>& mu_j = mu[j];
      if (j > 0)
	mu_i[j] *= mu[j-1][j-1];
      accum = 0;
      for (dimension_type h = 0; h < j; h++) {
        accum *= mu[h][h];
	// The following line optimizes the computation of
	// accum += mu_i[h] * mu_j[h].
	add_mul_assign(accum, mu_i[h], mu_j[h]);
	if (h > 0)
	  exact_div_assign(accum, mu[h-1][h-1]);
      }
      mu_i[j] -= accum;
    }

    // Let the `i'-th line become orthogonal wrt the `j'-th line,
    // for all 0 <= j < i.
    for (dimension_type j = 0; j < i; j++) {
      const Linear_Row& x_j = x[j];
      const Integer& mu_ij = mu_i[j];
      const Integer& mu_jj = mu[j][j];
      for (dimension_type k = n_columns; k-- > 0; ) {
        x_i[k] *= mu_jj;
	// The following line optimizes the computation of
        // x_i[k] -= mu_ij * x_j[k].
        sub_mul_assign(x_i[k], mu_ij, x_j[k]);
	if (j > 0)
	  exact_div_assign(x_i[k], mu[j-1][j-1]);
      }
    }
  }

  // Normalize the coefficients of the orthogonal base found.
  for (dimension_type i = rank; i-- > 0; )
    x[i].strong_normalize();

#if 0
  std::cout << "+++ After Gram-Shmidt on the base +++" << std::endl;
  ascii_dump(std::cout);
#endif

#ifndef NDEBUG
  // Check that the new base is indeed orthogonal.
  for (dimension_type i = rank; i-- > 0; ) {
    const Linear_Row& x_i = x[i];
    for (dimension_type j = i; j-- > 0; )
      if (scalar_product_sign(x_i, x[j]) != 0) {
	std::cout << "Not an orthogonal base" << std::endl;
	std::cout << "i = " << i << ", j = " << j << std::endl;
	std::cout << "After Gram-Shmidt on the base" << std::endl;
	ascii_dump(std::cout);
	assert(false);
      }
  }
#endif

  // Let denominator = <v_0, v_0> * ... * <v_{rank-1}, v_{rank-1}>
  // be the product of the squared norms of the orthogonal base.
  // Define d[j] = denominator / <v_j, v_j>.
  // Then, the formula to be computed, for each vector w which is not
  // in the orthogonal base, is the following:
  //
  // w' = denominator * w - \sum_{j=0}^{rank-1} (d[j] * <w, v_j> * v_j)
  //
  // factors[j] will contain d[j] * <w, v_j>.

  static std::vector<Integer> d;
  static std::vector<Integer> factors;
  d.reserve(compute_capacity(rank, d.max_size()));
  factors.reserve(compute_capacity(rank, factors.max_size()));
  if (d.size() < rank) {
    const dimension_type growth = rank - d.size();
    d.insert(d.end(), growth, 0);
    factors.insert(factors.end(), growth, 0);
  }

  // Computing all the factors d[0], ..., d[rank-1], and the denominator.
  Integer denominator = 1;
  for (dimension_type i = rank; i-- > 0; ) {
    const Linear_Row& x_i = x[i];
    scalar_product_assign(d[i], x_i, x_i);
    denominator *= d[i];
  }
  for (dimension_type i = rank; i-- > 0; )
    exact_div_assign(d[i], denominator, d[i]);

  // Orthogonalize the rows that are not lines/equalities.
  const dimension_type n_rows = num_rows();
  for (dimension_type i = rank; i < n_rows; i++) {
    Linear_Row& w = x[i];
    // Compute `factors' according to `w'.
    for (dimension_type j = rank; j-- > 0; ) {
      scalar_product_assign(factors[j], w, x[j]);
      factors[j] *= d[j];
    }
    for (dimension_type k = n_columns; k-- > 0; )
      w[k] *= denominator;
    for (dimension_type j = rank; j-- > 0; ) {
      const Linear_Row& v_j = x[j];
      for (dimension_type k = n_columns; k-- > 0; )
        sub_mul_assign(w[k], factors[j], v_j[k]);
    }
    assert(w.is_ray_or_point_or_inequality());
    w.normalize();

#if 0
  std::cout << "+++ After Gram-Shmidt on the whole system +++" << std::endl;
  ascii_dump(std::cout);
#endif

#ifndef NDEBUG
    // Check that w is indeed orthogonal wrt all the vectors in the base.
    for (dimension_type h = rank; h-- > 0; )
      if (scalar_product_sign(w, x[h]) != 0) {
	std::cout << "Not orthogonal" << std::endl;
	std::cout << "i = " << i << ", h = " << h << std::endl;
	std::cout << "After Gram-Shmidt on the whole system" << std::endl;
	ascii_dump(std::cout);
	assert(false);
      }
#endif
  }
  // Linear_System may be no longer sorted (unless it has one line/equality
  // and at most one ray/point/inequality).
  if (rank > 1 || n_rows > rank + 1)
    set_sorted(false);
  // A well-formed system has to be returned.
  assert(OK(true));
}

PPL::dimension_type
PPL::Linear_System::gauss() {
  // This method is only applied to a well-formed system
  // having no pending rows.
  assert(OK(true));
  assert(num_pending_rows() == 0);

  Linear_System& x = *this;
  dimension_type rank = 0;
  // Will keep track of the variations on the system of equalities.
  bool changed = false;
  const dimension_type n_columns =  num_columns();
  const dimension_type n_lines_or_equalities = num_lines_or_equalities();
  for (dimension_type j = n_columns; j-- > 0; ) {
    for (dimension_type i = rank; i < n_lines_or_equalities; ++i) {
      // Looking for the first non-zero coefficient (the pivot)
      // in the j-th column, starting from the last column.
      if (x[i][j] != 0) {
	// We want the pivot to be placed on the secondary diagonal,
	// if it is not the case, we swap the row containing it
	// with the one indexed by rank (that can be a previous one
	// or the same: in this case we do not swap).
	if (i > rank) {
	  std::swap(x[i], x[rank]);
	  // After swapping the system is no longer sorted.
	  changed = true;
	}
	// Linear combining the row containing the pivot with
	// all the ones that follow it such that all the elements
	// on the j-th column (of these rows) become 0.
	for (dimension_type k = i + 1; k < n_lines_or_equalities; ++k) {
	  if (x[k][j] != 0) {
	    x[k].linear_combine(x[rank], j);
	    changed = true;
	  }
	}
	// Have to consider the rows following the rank-th one
	// because until that one are already triangularized.
	++rank;
	break;
      }
    }
  }
  if (changed)
    set_sorted(false);
  // A well-formed system is returned.
  assert(OK(true));
  return rank;
}

void
PPL::Linear_System::back_substitute(const dimension_type rank) {
  // This method is only applied to a well-formed system
  // having no pending rows.
  assert(OK(true));
  assert(num_pending_rows() == 0);
  // The system describes a non-empty polyhedron and thus it always
  // contains a row which is not a line/equality (corresponding to
  // a vertex or to a low-level constraint).
  assert(num_rows() > rank);

  Linear_System& x = *this;
  const dimension_type nrows = num_rows();
  // Trying to keep sortedness.
  bool still_sorted = is_sorted();
  // This deque of booleans will be used to flag those rows that,
  // before exiting, need to be re-checked for sortedness.
  std::deque<bool> check_for_sortedness;
  if (still_sorted)
    check_for_sortedness.insert(check_for_sortedness.end(), nrows, false);

  for (dimension_type k = rank; k-- > 0; ) {
    // For each row, starting from the rank-th one,
    // looks for the last non-zero element.
    // j will be the index of such a element.
    Linear_Row& x_k = x[k];
    dimension_type j = num_columns() - 1;
    while (j != 0 && x_k[j] == 0)
      --j;

    // Go through the equalities above `x_k'.
    for (dimension_type i = k; i-- > 0; ) {
      Linear_Row& x_i = x[i];
      assert(x_i.is_line_or_equality());
      if (x_i[j] != 0) {
	// Combine linearly `x_i' with `x_k'
	// so that `x_i[j]' becomes zero.
	x_i.linear_combine(x_k, j);
	if (still_sorted) {
	  // Trying to keep sortedness: remember which rows
	  // have to be re-checked for sortedness at the end.
	  if (i > 0)
	    check_for_sortedness[i-1] = true;
	  check_for_sortedness[i] = true;
	}
      }
    }

    // Due to strong normalization during previous iterations,
    // the pivot coefficient `x_k[j]' may now be negative.
    // Since an inequality (or ray or point) cannot be multiplied
    // by a negative factor, the coefficient of the pivot must be
    // forced to be positive.
    const bool have_to_negate = (x_k[j] < 0);
    if (have_to_negate)
      for (dimension_type h = num_columns(); h-- > 0; )
	PPL::negate(x_k[h]);
    // Note: we do not mark index `k' in `check_for_sortedness',
    // because we will later negate back the row.

    // Go through all the inequalities of the system.
    for (dimension_type i = rank; i < nrows; ++i) {
      Linear_Row& x_i = x[i];
      if (x_i[j] != 0) {
	// Combine linearly the `x_i' with `x_k'
	// so that `x_i[j]' becomes zero.
	x_i.linear_combine(x_k, j);
	if (still_sorted) {
	  // Trying to keep sortedness: remember which rows
	  // have to be re-checked for sortedness at the end.
	  if (i > rank)
	    check_for_sortedness[i-1] = true;
	  check_for_sortedness[i] = true;
	}
      }
    }

    if (have_to_negate)
      // Negate `x_k' to restore strong-normalization.
      for (dimension_type h = num_columns(); h-- > 0; )
	PPL::negate(x_k[h]);
  }

  // Trying to keep sortedness.
  for (dimension_type i = 0; still_sorted && i < nrows-1; ++i)
    if (check_for_sortedness[i])
      // Have to check sortedness of `mat[i]' wrt `mat[i+1]'.
      still_sorted = (compare(x[i], x[i+1]) <= 0);
  // Set the sortedness flag.
  set_sorted(still_sorted);

  // A well-formed system is returned.
  assert(OK(true));
}

void
PPL::Linear_System::add_rows_and_columns(const dimension_type n) {
  assert(n > 0);
  const bool was_sorted = is_sorted();
  const dimension_type old_n_rows = num_rows();
  const dimension_type old_n_columns = num_columns();
  add_zero_rows_and_columns(n, n, Linear_Row::Flags(row_topology));
  Linear_System& x = *this;
  // The old system is moved to the bottom.
  for (dimension_type i = old_n_rows; i-- > 0; )
    std::swap(x[i], x[i + n]);
  for (dimension_type i = n, c = old_n_columns; i-- > 0; ) {
    // The top right-hand sub-system (i.e., the system made
    // of new rows and columns) is set to the specular image
    // of the identity matrix.
    Linear_Row& r = x[i];
    r[c++] = 1;
    r.set_is_line_or_equality();
    // Note: `r' is strongly normalized.
  }
  // If the old system was empty, the last row added is either
  // a positivity constraint or a point.
  if (old_n_columns == 0) {
    x[n-1].set_is_ray_or_point_or_inequality();
    // Since ray, points and inequalities come after lines
    // and equalities, this case implies the system is sorted.
    set_sorted(true);
  }
  else if (was_sorted)
    set_sorted(compare(x[n-1], x[n]) <= 0);

  // A well-formed system has to be returned.
  assert(OK(true));
}

void
PPL::Linear_System::sort_pending_and_remove_duplicates() {
  assert(num_pending_rows() > 0);
  assert(is_sorted());
  Linear_System& x = *this;

  // The non-pending part of the system is already sorted.
  // Now sorting the pending part..
  const dimension_type first_pending = x.first_pending_row();
  x.sort_rows(first_pending, x.num_rows());
  // Recompute the number of rows, because we may have removed
  // some rows occurring more than once in the pending part.
  dimension_type num_rows = x.num_rows();

  dimension_type k1 = 0;
  dimension_type k2 = first_pending;
  dimension_type num_duplicates = 0;
  // In order to erase them, put at the end of the system
  // those pending rows that also occur in the non-pending part.
  while (k1 < first_pending && k2 < num_rows) {
    const int cmp = compare(x[k1], x[k2]);
    if (cmp == 0) {
      // We found the same row.
      ++num_duplicates;
      --num_rows;
      // By initial sortedness, we can increment index `k1'.
      ++k1;
      // Do not increment `k2'; instead, swap there the next pending row.
      if (k2 < num_rows)
	std::swap(x[k2], x[k2 + num_duplicates]);
    }
    else if (cmp < 0)
      // By initial sortedness, we can increment `k1'.
      ++k1;
    else {
      // Here `cmp > 0'.
      // Increment `k2' and, if we already found any duplicate,
      // swap the next pending row in position `k2'.
      ++k2;
      if (num_duplicates > 0 && k2 < num_rows)
	std::swap(x[k2], x[k2 + num_duplicates]);
    }
  }
  // If needed, swap any duplicates found past the pending rows
  // that has not been considered yet; then erase the duplicates.
  if (num_duplicates > 0) {
    if (k2 < num_rows)
      for (++k2; k2 < num_rows; ++k2)
	std::swap(x[k2], x[k2 + num_duplicates]);
    x.erase_to_end(num_rows);
  }
  // Do not check for strong normalization,
  // because no modification of rows has occurred.
  assert(OK(false));
}

bool
PPL::Linear_System::check_sorted() const {
  const Linear_System& x = *this;
  for (dimension_type i = first_pending_row(); i-- > 1; )
    if (compare(x[i], x[i-1]) < 0)
      return false;
  return true;
}

bool
PPL::Linear_System::OK(const bool check_strong_normalized) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // `index_first_pending' must be less then or equal to `num_rows()'.
  if (first_pending_row() > num_rows()) {
#ifndef NDEBUG
    cerr << "Linear_System has a negative number of pending rows!"
	 << endl;
#endif
    return false;
  }

  // An empty system is OK,
  // unless it is an NNC system with exactly one column.
  if (num_rows() == 0)
    if (is_necessarily_closed() || num_columns() != 1)
      return true;
    else {
#ifndef NDEBUG
      cerr << "NNC Linear_System has one column" << endl;
#endif
      return false;
    }

  // A non-empty system will contain constraints or generators; in
  // both cases it must have at least one column for the inhomogeneous
  // term and, if it is NNC, another one for the epsilon coefficient.
  const dimension_type min_cols = is_necessarily_closed() ? 1 : 2;
  if (num_columns() < min_cols) {
#ifndef NDEBUG
    cerr << "Linear_System has fewer columns than the minimum "
	 << "allowed by its topology:"
	 << endl
	 << "num_columns is " << num_columns()
	 << ", minimum is " << min_cols
	 << endl;
#endif
    return false;
  }

  const Linear_System& x = *this;
  const dimension_type n_rows = num_rows();
  for (dimension_type i = 0; i < n_rows; ++i) {
    if (!x[i].OK(row_size, row_capacity))
      return false;
    // Checking for topology mismatches.
    if (x.topology() != x[i].topology()) {
#ifndef NDEBUG
      cerr << "Topology mismatch between the system "
	   << "and one of its rows!"
	   << endl;
#endif
      return false;
    }
  }

  if (check_strong_normalized) {
    // Check for strong normalization of rows.
    // Note: normalization cannot be checked inside the
    // Linear_Row::OK() method, because a Linear_Row object may also
    // implement a LinExpression object, which in general cannot be
    // (strongly) normalized.
    Linear_System tmp = x;
    tmp.strong_normalize();
    if (x != tmp) {
#ifndef NDEBUG
      cerr << "Linear_System rows are not strongly normalized!"
	   << endl;
#endif
      return false;
    }
  }

  if (sorted && !check_sorted()) {
#ifndef NDEBUG
    cerr << "The system declares itself to be sorted but it is not!"
	 << endl;
#endif
    return false;
  }

  // All checks passed.
  return true;
}
