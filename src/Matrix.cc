/* Matrix class implementation (non-inline functions).
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Matrix.defs.hh"

#include "Integer.defs.hh"
#include "Row.defs.hh"
#include "globals.hh"
#include "SatMatrix.defs.hh"
#include <algorithm>
#include <iostream>
#include <string>

namespace PPL = Parma_Polyhedra_Library;

/*!
  Returns the number of the rows of the matrix
  that represent lines or equalities.
*/
size_t
PPL::Matrix::num_lines_or_equalities() const {
  size_t n = 0;
  for (size_t i = num_rows(); i != 0; )
    if (rows[--i].is_line_or_equality())
      ++n;
  return n;
}

/*!
  \param num_rows      The number of rows of the matrix that will be created.
  \param num_columns   The number of columns of the matrix
                       that will be created.

  This constructor creates an unsorted
  \p num_rows \f$\times\f$ \p num_columns matrix which rows are
  all initialized to rays or vertices or inequalities.
*/
PPL::Matrix::Matrix(size_t num_rows, size_t num_columns)
  : rows(num_rows),
    row_size(num_columns),
    row_capacity(compute_capacity(num_columns)),
    sorted(false) {
  // Construct in direct order: will destroy in reverse order.
  for (size_t i = 0; i < num_rows; ++i)
    rows[i].construct(Row::RAY_OR_VERTEX_OR_INEQUALITY,
		      num_columns, row_capacity);
}


PPL::Matrix::Matrix(const Matrix& y)
  : rows(y.rows),
    row_size(y.row_size),
    row_capacity(compute_capacity(y.row_size)),
    sorted(y.sorted) {
}


PPL::Matrix&
PPL::Matrix::operator =(const Matrix& y) {
  rows = y.rows;
  row_size = y.row_size;
  row_capacity = compute_capacity(row_size);
  sorted = y.sorted;
  return *this;
}


/*!
  \param new_num_rows      The number of rows of the
                           resized matrix.
  \param new_num_columns   The number of columns of the
                           resized matrix.

  Creates a new matrix with the given dimensions and copies the content
  of the old elements to the new ones.
  The new matrix is larger than the original one and
  the old matrix is copied in the upper, left-hand corner of the new
  matrix.
*/
void
PPL::Matrix::grow(size_t new_num_rows, size_t new_num_columns) {
  size_t old_num_rows = rows.size();

  assert(new_num_rows >= old_num_rows);
  assert(new_num_columns >= row_size);

  // Note that, if we have `new_num_rows == old_num_rows', the matrix
  // will keep its sortedness.
  // This is obvious if `new_num_columns == row_size'.
  // If `new_num_columns > row_size', then sortedness is maintained
  // because trailing zeroes will be added to all rows.
  bool was_sorted = is_sorted();

  if (new_num_rows > old_num_rows) {
    if (new_num_columns <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_num_rows) {
	// Reallocation will take place.
	std::vector<Row> new_rows;
	new_rows.reserve(compute_capacity(new_num_rows));
	new_rows.insert(new_rows.end(), new_num_rows, Row());
	// Construct the new rows.
	size_t i = new_num_rows;
	while (i-- > old_num_rows)
	  new_rows[i].construct(Row::RAY_OR_VERTEX_OR_INEQUALITY,
				new_num_columns, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation will NOT take place.
	rows.insert(rows.end(), new_num_rows - old_num_rows, Row());
	for (size_t i = new_num_rows; i-- > old_num_rows; )
	  rows[i].construct(Row::RAY_OR_VERTEX_OR_INEQUALITY,
			    new_num_columns, row_capacity);
      }
    }
    else {
      // We cannot even recycle the old rows.
      Matrix new_matrix;
      new_matrix.rows.reserve(compute_capacity(new_num_rows));
      new_matrix.rows.insert(new_matrix.rows.end(), new_num_rows, Row());
      // Construct the new rows.
      new_matrix.row_size = new_num_columns;
      new_matrix.row_capacity = compute_capacity(new_num_columns);
      size_t i = new_num_rows;
      while (i-- > old_num_rows)
	new_matrix.rows[i].construct(Row::RAY_OR_VERTEX_OR_INEQUALITY,
				     new_matrix.row_size,
				     new_matrix.row_capacity);
      // Copy the old rows.
      ++i;
      while (i-- > 0) {
	Row new_row(rows[i],
		    new_matrix.row_size,
		    new_matrix.row_capacity);
	std::swap(new_matrix.rows[i], new_row);
      }
      // Rows have been added: see if the matrix is known to be sorted.
      new_matrix.set_sorted(was_sorted
			    && (new_matrix[old_num_rows-1]
				<= new_matrix[old_num_rows]));
      // Put the new vector into place.
      swap(new_matrix);
      assert(OK());
      return;
    }
  }
  // Here we have the right number of rows.
  if (new_num_columns > row_size) {
    // We need more columns.
    if (new_num_columns <= row_capacity)
      // But we have enough capacity: we resize existing rows.
      for (size_t i = old_num_rows; i-- > 0; )
	rows[i].grow_no_copy(new_num_columns);
    else {
      // Capacity exhausted: we must reallocate the rows and
      // make sure all the rows have the same capacity.
      size_t new_row_capacity = compute_capacity(new_num_columns);
      for (size_t i = old_num_rows; i-- > 0; ) {
	Row new_row(rows[i], new_num_columns, new_row_capacity);
	std::swap(rows[i], new_row);
      }
      row_capacity = new_row_capacity;
    }
    // Rows have grown or shrunk.
    row_size = new_num_columns;
  }
  // If rows have been added, we should check if we are still sorted.
  if (old_num_rows == 0)
    // The matrix was empty: now it is sorted.
    set_sorted(true);
  else if (new_num_rows > old_num_rows)
    // Rows were added.
    if (was_sorted)
      set_sorted((*this)[old_num_rows-1] <= (*this)[old_num_rows]);
  // If no rows was added the matrix keeps its sortedness.

  assert(OK());
}

/*!
  \param new_num_rows      The number of rows of the
                           resized matrix.
  \param new_num_columns   The number of columns of the
                           resized matrix.
		
  Creates a new matrix with the given dimensions without copying
  the content of the old elements to the new ones.
*/
void
PPL::Matrix::resize_no_copy(size_t new_num_rows, size_t new_num_columns) {
  size_t old_num_rows = rows.size();
  // Note that, if we have `new_num_rows <= old_num_rows' and
  // `new_num_columns >= row_size', the matrix will keep its sortedness.
  // This is obvious if `new_num_columns == row_size'.
  // If `new_num_columns > row_size', then sortedness is maintained
  // because trailing zeroes will be added to all rows.
  if (new_num_rows > old_num_rows) {
    if (new_num_columns <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_num_rows) {
	// Reallocation will take place.
	std::vector<Row> new_rows;
	new_rows.reserve(compute_capacity(new_num_rows));
	new_rows.insert(new_rows.end(), new_num_rows, Row());
	// Construct the new rows.
	size_t i = new_num_rows;
	while (i-- > old_num_rows)
	  new_rows[i].construct(Row::LINE_OR_EQUALITY,
				new_num_columns, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation will NOT take place.
	rows.insert(rows.end(), new_num_rows - old_num_rows, Row());
	for (size_t i = new_num_rows; i-- > old_num_rows; )
	  rows[i].construct(Row::LINE_OR_EQUALITY,
			    new_num_columns, row_capacity);
      }
      // Even though `*this' may happen to keep its sortedness,
      // we feel checking that this is the case is not worth the effort.
      // Moreover, it is very likely the matrix will be overwritten
      // as soon as we return.
      set_sorted(false);
    }
    else {
      // We cannot even recycle the old rows: allocate a new matrix and swap.
      Matrix new_matrix(new_num_rows, new_num_columns);
      swap(new_matrix);
      return;
    }
  }
  else if (new_num_rows < old_num_rows) {
    // Drop some rows.
    rows.erase(rows.begin() + new_num_rows, rows.end());
    old_num_rows = new_num_rows;
  }
  // Here we have the right number of rows.
  if (new_num_columns != row_size) {
    if (new_num_columns < row_size) {
      // Shrink the existing rows.
      for (size_t i = old_num_rows; i-- > 0; )
	rows[i].shrink(new_num_columns);
      // Ditto.
      set_sorted(false);
    }
    else
      // We need more columns.
      if (new_num_columns <= row_capacity)
	// But we have enough capacity: we resize existing rows.
	for (size_t i = old_num_rows; i-- > 0; )
	  rows[i].grow_no_copy(new_num_columns);
      else {
	// Capacity exhausted: we must reallocate the rows and
	// make sure all the rows have the same capacity.
	size_t new_row_capacity = compute_capacity(new_num_columns);
	for (size_t i = old_num_rows; i-- > 0; ) {
	  Row new_row(Row::LINE_OR_EQUALITY, new_num_columns, new_row_capacity);
	  std::swap(rows[i], new_row);
	}
	row_capacity = new_row_capacity;
      }
    // Rows have grown or shrunk.
    row_size = new_num_columns;
  }
}

/*!
  This virtual raw write method prints the number of rows, the
  number of columns and the \p sorted flag. The specialized
  methods in ConSys and GenSys take care of properly printing
  the contents of the Matrix.
*/
void
PPL::Matrix::print(std::ostream& s) const {
  using std::endl;

  const Matrix& x = *this;
  const char separator = ' ';
  s << x.num_rows() << separator << 'x' << separator
    << x.num_columns() << separator
    << (x.sorted ? "(sorted)" : "(not_sorted)")
    << endl;
}

// A placeholder for print().
std::ostream&
PPL::operator <<(std::ostream& s, const Matrix& m) {
  m.print(s);
  return s;
}

/*!
  This virtual raw read method is meant to read into a Matrix
  a <CODE>print()</CODE> output. The specialized methods in
  <CODE>ConSys</CODE> and <CODE>GenSys</CODE> take care of properly
  reading the contents.
*/
void
PPL::Matrix::get(std::istream& s) {
  size_t nrows;
  size_t ncols;
  std::string tempstr;
  s >> nrows
    >> tempstr
    >> ncols;
  resize_no_copy(nrows, ncols);
  s >> tempstr;
  assert(tempstr == "(sorted)" || tempstr == "(not_sorted)");
  set_sorted(tempstr == "(sorted)");
}

// A placeholder for get().
std::istream&
PPL::operator >>(std::istream& s, Matrix& m) {
  m.get(s);
  return s;
}

/*!
  \param y   The matrix to be merged with \p *this one.

  Merge \p y with \p *this removing duplicates (i.e., rows that
  appear either in \p y and in \p *this) and obtaining a new
  sorted matrix that will be assigned to \p *this.
  Both matrices are assumed to be sorted on entry.
*/
void
PPL::Matrix::merge_rows_assign(const Matrix& y) {
  assert(row_size >= y.row_size);
  assert(check_sorted() && y.check_sorted());

  Matrix& x = *this;

  // A temporary vector of rows...
  std::vector<Row> tmp;
  // ... with enough capacity not to require any reallocations.
  tmp.reserve(compute_capacity(x.num_rows() + y.num_rows()));

  std::vector<Row>::iterator xi = x.rows.begin();
  std::vector<Row>::iterator xend = x.rows.end();
  std::vector<Row>::const_iterator yi = y.rows.begin();
  std::vector<Row>::const_iterator yend = y.rows.end();

  while (xi != xend && yi != yend) {
    int comp = compare(*xi, *yi);
    if (comp <= 0) {
      // Elements that can be taken from `x' are actually _stolen_ from `x'
      std::swap(*xi++, *tmp.insert(tmp.end()));
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
      std::swap(*xi++, *tmp.insert(tmp.end()));
  else
    while (yi != yend) {
      Row copy(*yi++, row_size, row_capacity);
      std::swap(copy, *tmp.insert(tmp.end(), Row()));
    }

  // We get the result vector and let the old one be destroyed.
  std::swap(tmp, rows);

  assert(check_sorted());
}

/*!
  Sorts the rows (in growing order) and eliminates duplicated ones.
*/
void
PPL::Matrix::sort_rows() {
  Matrix& x = *this;
  size_t num_rows = x.num_rows();
  Row x_i;
  for (size_t i = 1; i < num_rows; ) {
    x_i.assign(x[i]);
    size_t j;
    int cmp = 1;
    for (j = i; j > 0; --j) {
      cmp = compare(x[j-1], x_i);
      if (cmp <= 0)
	break;
      x[j].assign(x[j-1]);
    }
    if (cmp == 0) {
      for ( ; j < i; ++j)
	x[j].assign(x[j+1]);
      x[i].assign(x_i);
      --num_rows;
      std::swap(x[i], x[num_rows]);
    }
    else {
      x[j].assign(x_i);
      ++i;
    }
  }
  Row null;
  x_i.assign(null);
  rows.erase(rows.begin()+num_rows, rows.end());
  sorted = true;
  assert(OK());
}

/*!
  Adds a copy of the given row to the matrix.
*/
void
PPL::Matrix::add_row(const Row& row) {
  // The added row must have the same number
  // of elements of the existing rows of the matrix.
  assert(row.size() == row_size);
  bool was_sorted = is_sorted();
  size_t new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(row, row_capacity);
    size_t i = new_rows_size-1;
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
    Row tmp(row, row_capacity);
    std::swap(*rows.insert(rows.end(), Row()), tmp);
  }
  // Check whether the modified Matrix happens to be sorted.
  if (was_sorted) {
    size_t nrows = num_rows();
    // The added row may have caused the matrix to be not sorted anymore.
    if (nrows > 1) {
      // If the matrix is not empty and the inserted row
      // is the greatest one, the matrix is set to be sorted.
      // If it is not the greatest one then the matrix is no longer sorted.
      Matrix& x = *this;
      set_sorted(x[nrows-2] <= x[nrows-1]);
    }
    else
      // A matrix having only one row is sorted.
      set_sorted(true);
  }
}

/*!
  Adds the given row to the matrix.
*/
void
PPL::Matrix::insert(const Row& row) {
  if (row.size() > row_size)
    grow(num_rows(), row.size());
  if (row.size() < row_size)
    add_row(Row(row, row_size, row_capacity));
  else
    add_row(row);
}

/*!
  Adds a new empty row to the matrix setting its type to the given
  \p type.
*/
void
PPL::Matrix::add_row(Row::Type type) {
  bool was_sorted = is_sorted();
  size_t new_rows_size = rows.size() + 1;
  if (rows.capacity() < new_rows_size) {
    // Reallocation will take place.
    std::vector<Row> new_rows;
    new_rows.reserve(compute_capacity(new_rows_size));
    new_rows.insert(new_rows.end(), new_rows_size, Row());
    // Put the new row in place.
    Row new_row(type, row_size, row_capacity);
    size_t i = new_rows_size-1;
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
    rows.insert(rows.end(), Row())->construct(type, row_size, row_capacity);

  // Check whether the modified Matrix happens to be sorted.
  if (was_sorted) {
    size_t nrows = num_rows();
    // The added row may have caused the matrix to be not sorted anymore.
    if (nrows > 1) {
      // If the matrix is not empty and the inserted row
      // is the greatest one, the matrix is set to be sorted.
      // If it is not the greatest one then the matrix is no longer sorted.
      Matrix& x = *this;
      set_sorted(x[nrows-2] <= x[nrows-1]);
    }
    else
      // A matrix having only one row is sorted.
      set_sorted(true);
  }
}

/*!
  Normalize the matrix.
*/
void
PPL::Matrix::normalize() {
  for (size_t i = num_rows(); i-- > 0; )
    rows[i].normalize();
  set_sorted(false);
}

/*!
  Strongly normalizes each row.
*/
void
PPL::Matrix::strong_normalize() {
  for (size_t i = num_rows(); i-- > 0; )
    rows[i].strong_normalize();
  set_sorted(false);
}

/*!
  Returns <CODE>true</CODE> if and only if \p x and
  \p y are identical.
*/
bool
PPL::operator ==(const Matrix& x, const Matrix& y) {
  if (x.num_columns() != y.num_columns())
    return false;
  size_t x_num_rows = x.num_rows();
  if (x_num_rows != y.num_rows())
    return false;
  for (size_t i = x_num_rows; i-- > 0; )
    if (compare(x[i], y[i]) != 0)
      return false;
  return true;
}

/*!
  \param sat   Saturation matrix whose rows represent the rows of \p *this.

  If \p *this has constraints on its rows, then the rows of \p sat are
  indexed by constraints, otherwise they are indexed by generators.

  Sorts the matrix keeping \p sat consistent, then removes duplicates.
*/
void
PPL::Matrix::sort_and_remove_with_sat(SatMatrix& sat) {
  Matrix& x = *this;
  size_t num_kept_rows = x.num_rows();
  assert(num_kept_rows == sat.num_rows());
  if (num_kept_rows <= 1) {
    set_sorted(true);
    return;
  }
  for (size_t i = 0; i < num_kept_rows - 1; ++i) {
    for (size_t j = num_kept_rows - 1 ; j > i ; --j) {
      int cmp = compare(x[j], x[j - 1]);
      if (cmp == 0) {
	// If the compared rows are equals, we move the one with
	// the greatest index (and the corresponding row of
	// the saturation matrix) to the bottom of the matrix.
	// Now the number of row is one less.
	--num_kept_rows;
	std::swap(x[j], x[num_kept_rows]);
	std::swap(sat[j], sat[num_kept_rows]);
      }
      else if (cmp < 0) {
	// If `x[j]' is less than `x[j - 1]' we swap the two rows
	// and the corresponding rows of the saturation matrix.
	std::swap(x[j], x[j - 1]);
	std::swap(sat[j], sat[j - 1]);
      }
    }
  }
  // Erasing the duplicated rows...
  x.erase_to_end(num_kept_rows);
  // ... and the corresponding rows of the saturation matrix.
  sat.rows_erase_to_end(num_kept_rows);
  assert(check_sorted());
  // Now the matrix is sorted.
  x.set_sorted(true);
}

/*!
  This method works only on equalities: this is because it requires that
  equalities come first in the matrix. This way they are all grouped
  in the top of the matrix and it is simpler to find them.

  <CODE>gauss()</CODE> method finds a minimal system for
  equalities and returns its rank i.e., the
  number of linearly independent equalities.
  The result is an upper triangular matrix obtained choosing
  (for each equality) the pivot starting from the right-most columns.
*/
size_t
PPL::Matrix::gauss() {
  size_t rank = 0;
  // Will keep track of the variations on the matrix of equalities.
  bool changed = false;
  size_t nb_columns =  num_columns();
  size_t nb_lines_or_equalities = num_lines_or_equalities();
  for (size_t j = nb_columns; j-- > 0; ) {
    for (size_t i = rank; i < nb_lines_or_equalities; ++i) {
      // Looking for the first non-zero coefficient (the pivot)
      // in the j-th column, starting from the last column.
      if (rows[i][j] != 0) {
	// We want the pivot to be placed on the secondary diagonal,
	// if it is not the case, we swap the row containing it
	// with the one indexed by rank (that can be a previous one
	// or the same: in this case we do not swap).
	if (i > rank) {
	  std::swap(rows[i], rows[rank]);
	  // After swapping the matrix is no longer sorted.
	  changed = true;
	}
	// We want the pivot to be greater than zero to
	// simplify future computing (back-substitution).
	if (rows[rank][j] < 0) {
	  for (size_t k = nb_columns; k-- > 0; )
	    rows[rank][k].negate();
	  // Matrix has changed.
	  changed = true;
	}
	// Linear combining the row containing the pivot with
	// all the ones that follow it such that all the elements
	// on the j-th column (of these rows) become 0.
	for (size_t k = i + 1; k < nb_lines_or_equalities; ++k) {
	  if (rows[k][j] != 0) {
	    rows[k].linear_combine(rows[rank], j);
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
  return rank;
}

/*!
  Takes an upper triangular matrix.
  For each row, starting from the one having the minimum number of
  coefficients not equal to zero, computes the expression of an element
  as a function of the remaining ones and then substitutes this expression
  in all the other rows.
*/
void
PPL::Matrix::back_substitute(size_t rank) {
  bool was_sorted = is_sorted();
  size_t nrows = num_rows();
  for (size_t k = rank; k-- > 0; ) {
    // For each row, starting from the rank-th one,
    // looks for the last non-zero element.
    // j will be the index of such a element.
    size_t j = num_columns() - 1;
    while (j != 0 && rows[k][j] == 0)
      --j;

    for (size_t i = 0; i < nrows; ++i)
      // i runs through all the rows of the matrix.
      if (i > k && i < rank)
	// Coefficients on the j-th column of the rows
	// following the one we are considering (k-th one)
	// until the last one linearly independent, have to be
	// be zero, as the matrix is triangular.
	//          j
	// .  .  .  .  .  .
	// .  .  .  .  .  0
	// .  .  .  .  0  0  k
	// .  .  .  0  0  0
	// .  .  0  0  0  0  rank
	// .  .  .  .  .  .  inequality
	// .  .  .  .  .  .  inequality
	// .  .  .  .  .  .  inequality
	assert(rows[i][j] == 0);
      else if (rows[i][j] != 0 && i != k) {
	// We have already a row with j-th coefficient non-zero
	// (the k-th one), so we linear combine all the other
	// rows (but these already treated above) with the
	// k-th one such that they have a zero coefficient
	// in position j.
#if EXTRA_NORMALIZATION
	// An inequality cannot be multiplied by a negative number.
	// As a consequence, if we combine an equality (or a line)
	// with an inequality (or ray or vertex) the "pivot" element
	// of the equality must be positive.
	// That is what happens here: if the pivot `rows[k][j]'
	// is negative, the row `rows[k]' is negated before the
	// linear combination takes place.
	if (rows[i].is_ray_or_vertex_or_inequality())
	  if (rows[k][j] < 0)
	    for (size_t h = num_columns(); h-- > 0; )
	      rows[k][h].negate();
#endif
	
	rows[i].linear_combine(rows[k], j);

	// Sort checking.
	if (was_sorted) {
	  // Keeping 'sorted' flag consistent.
	  if (nrows <= 1)
	    // One-row or zero-row matrix are sorted.
	    set_sorted(true);
	  else if (nrows == 2)
	    set_sorted(rows[0] <= rows[1]);
	  else
	    // i-th row is become a linear combination of others
	    // rows, so it is changed: if it is still sorted
	    // with respect to the adjacent one(s)
	    // the matrix remains sorted.
	    if (i != 0 && i != nrows-1)
	      set_sorted(rows[i-1] <= rows[i] && rows[i] <= rows[i+1]);
	    else if (i == 0)
	      set_sorted(rows[0] <= rows[1]);
	    else if (i == nrows-1)
	      set_sorted(rows[nrows-2] <= rows[nrows-1]);
	  was_sorted = is_sorted();
	}
      }
  }
}

/*!
  \param n      The number of rows and columns to be added.

  Turn the \f$r \times c\f$ matrix \f$M\f$ into
  the \f$(r+n) \times (c+n)\f$ matrix
  \f$\bigl({0 \atop M}{J \atop 0}\bigr)\f$,
  where \f$J\f$ is the specular image
  of the \f$n \times n\f$ identity matrix.
*/
void
PPL::Matrix::add_rows_and_columns(size_t n) {
  assert(n > 0);
  bool was_sorted = is_sorted();
  size_t old_num_rows = num_rows();
  size_t old_num_columns = num_columns();
  grow(old_num_rows + n, old_num_columns + n);
  Matrix& x = *this;
  // The old matrix is moved to the bottom.
  for (size_t i = old_num_rows; i-- > 0; )
    std::swap(x[i], x[i + n]);
  for (size_t i = n, c = old_num_columns; i-- > 0; ) {
    // The top right-hand sub-matrix (i.e., the matrix made
    // of new rows and columns) is set to the specular image
    // of the identity matrix.
    Row& r = x[i];
    r[c++] = 1;
    r.set_is_line_or_equality();
  }
  // If the old matrix was empty, the last row added is either
  // a positivity constraint or a vertex.
  if (old_num_columns == 0) {
    x[n-1].set_is_ray_or_vertex_or_inequality();
    // Since ray, vertices and inequalities come after lines
    // and equalities, this case implies the matrix is sorted.
    set_sorted(true);
  }
  else if (was_sorted)
    set_sorted(x[n-1] <= x[n]);

  assert(OK());
}


/*!
  Returns <CODE>true</CODE> if \p *this is sorted, <CODE>false</CODE>
  otherwise.
*/
bool
PPL::Matrix::check_sorted() const {
  const Matrix& x = *this;
  for (size_t i = num_rows(); i-- > 1; )
    if (x[i] < x[i-1])
      return false;
  return true;
}


bool
PPL::Matrix::OK() const {
  // A non-empty matrix will contain constraints or generators; in
  // both cases it must have at least one column for the inhomogeneous
  // term.
  if (num_rows() > 0 && num_columns() < 1) {
    std::cerr << "A Matrix must have at least two columns!"
	      << std::endl;
    return false;
  }

  const Matrix& x = *this;
  bool is_broken = false;
  size_t nrows = num_rows();
  for (size_t i = 0; i < nrows; ++i)
    is_broken |= !x[i].OK(row_size, row_capacity);

  if (sorted && !check_sorted()) {
    is_broken = true;
    std::cerr << "The matrix declares itself to be sorted but it is not!"
	      << std::endl;
  }

  return !is_broken;
}
