/* DB_Matrix class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_DB_Matrix_inlines_hh
#define PPL_DB_Matrix_inlines_hh 1

#include <cassert>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename T>
inline void
DB_Matrix<T>::swap(DB_Matrix& y) {
  std::swap(rows, y.rows);
  std::swap(row_size, y.row_size);
  std::swap(row_capacity, y.row_capacity);
}

template <typename T>
inline dimension_type
DB_Matrix<T>::max_num_rows() {
  // FIXME: isn't this ridiculous?  Creating a vector only to know what
  // its maximum size is?  Why is vector::max_size() not static?
  return std::vector<DB_Row<T> >().max_size();
}

template <typename T>
inline dimension_type
DB_Matrix<T>::max_num_columns() {
  return DB_Row<T>::max_size();
}

template <typename T>
inline
DB_Matrix<T>::const_iterator::const_iterator()
  : i(Iter()) {
}

template <typename T>
inline
DB_Matrix<T>::const_iterator::const_iterator(const Iter& b)
  : i(b) {
}

template <typename T>
inline
DB_Matrix<T>::const_iterator::const_iterator(const const_iterator& y)
  : i(y.i) {
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator&
DB_Matrix<T>::const_iterator::operator=(const const_iterator& y) {
  i = y.i;
  return *this;
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator::reference
DB_Matrix<T>::const_iterator::operator*() const {
  return *i;
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator::pointer
DB_Matrix<T>::const_iterator::operator->() const {
  return &*i;
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator&
DB_Matrix<T>::const_iterator::operator++() {
  ++i;
  return *this;
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator
DB_Matrix<T>::const_iterator::operator++(int) {
  return const_iterator(i++);
}

template <typename T>
inline bool
DB_Matrix<T>::const_iterator::operator==(const const_iterator& y) const {
  return i == y.i;
}

template <typename T>
inline bool
DB_Matrix<T>::const_iterator::operator!=(const const_iterator& y) const {
  return !operator==(y);
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator
DB_Matrix<T>::begin() const {
  return const_iterator(rows.begin());
}

template <typename T>
inline typename DB_Matrix<T>::const_iterator
DB_Matrix<T>::end() const {
  return const_iterator(rows.end());
}

template <typename T>
inline
DB_Matrix<T>::DB_Matrix()
  : rows(),
    row_size(0),
    row_capacity(0) {
}

template <typename T>
inline
DB_Matrix<T>::~DB_Matrix() {
}

template <typename T>
inline DB_Row<T>&
DB_Matrix<T>::operator[](const dimension_type k) {
  assert(k < rows.size());
  return rows[k];
}

template <typename T>
inline const DB_Row<T>&
DB_Matrix<T>::operator[](const dimension_type k) const {
  assert(k < rows.size());
  return rows[k];
}

template <typename T>
inline dimension_type
DB_Matrix<T>::num_rows() const {
  return rows.size();
}

/*! \relates DB_Matrix */
template <typename T>
inline bool
operator!=(const DB_Matrix<T>& x, const DB_Matrix<T>& y) {
  return !(x == y);
}

template <typename T>
inline
DB_Matrix<T>::DB_Matrix(const dimension_type n_rows)
  : rows(n_rows),
    row_size(n_rows),
    row_capacity(compute_capacity(n_rows)) {
  // Construct in direct order: will destroy in reverse order.
  for (dimension_type i = 0; i < n_rows; ++i)
    rows[i].construct(n_rows, row_capacity);
  assert(OK());
}

template <typename T>
inline
DB_Matrix<T>::DB_Matrix(const DB_Matrix& y)
  : rows(y.rows),
    row_size(y.row_size),
    row_capacity(compute_capacity(y.row_size)) {
}

template <typename T>
inline DB_Matrix<T>&
DB_Matrix<T>::operator=(const DB_Matrix& y) {
  // Without the following guard against auto-assignments we would
  // recompute the row capacity based on row size, possibly without
  // actually increasing the capacity of the rows.  This would lead to
  // an inconsistent state.
  if (this != &y) {
    // The following assignment may do nothing on auto-assignments...
    rows = y.rows;
    row_size = y.row_size;
    // ... hence the following assignment must not be done on
    // auto-assignments.
    row_capacity = compute_capacity(y.row_size);
  }
  return *this;
}

template <typename T>
void
DB_Matrix<T>::grow(const dimension_type new_n_rows) {
  const dimension_type old_n_rows = rows.size();
  assert(new_n_rows >= old_n_rows);

  if (new_n_rows > old_n_rows) {
    if (new_n_rows <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_n_rows) {
	// Reallocation will take place.
	std::vector<DB_Row<T> > new_rows;
	new_rows.reserve(compute_capacity(new_n_rows));
	new_rows.insert(new_rows.end(), new_n_rows, DB_Row<T>());
	// Construct the new rows.
	dimension_type i = new_n_rows;
	while (i-- > old_n_rows)
	  new_rows[i].construct(new_n_rows, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation will NOT take place.
	rows.insert(rows.end(), new_n_rows - old_n_rows, DB_Row<T>());
	for (dimension_type i = new_n_rows; i-- > old_n_rows; )
	  rows[i].construct(new_n_rows, row_capacity);
      }
    }
    else {
      // We cannot even recycle the old rows.
      DB_Matrix new_matrix;
      new_matrix.rows.reserve(compute_capacity(new_n_rows));
      new_matrix.rows.insert(new_matrix.rows.end(), new_n_rows, DB_Row<T>());
      // Construct the new rows.
      new_matrix.row_size = new_n_rows;
      new_matrix.row_capacity = compute_capacity(new_n_rows);
      dimension_type i = new_n_rows;
      while (i-- > old_n_rows)
	new_matrix.rows[i].construct(new_matrix.row_size,
				     new_matrix.row_capacity);
      // Copy the old rows.
      ++i;
      while (i-- > 0) {
	DB_Row<T> new_row(rows[i],
			  new_matrix.row_size,
			  new_matrix.row_capacity);
	std::swap(new_matrix.rows[i], new_row);
      }
      // Put the new vector into place.
      swap(new_matrix);
      return;
    }
  }
  // Here we have the right number of rows.
  if (new_n_rows > row_size) {
    // We need more columns.
    if (new_n_rows <= row_capacity)
      // But we have enough capacity: we resize existing rows.
      for (dimension_type i = old_n_rows; i-- > 0; )
	rows[i].expand_within_capacity(new_n_rows);
    else {
      // Capacity exhausted: we must reallocate the rows and
      // make sure all the rows have the same capacity.
      const dimension_type new_row_capacity = compute_capacity(new_n_rows);
      for (dimension_type i = old_n_rows; i-- > 0; ) {
	DB_Row<T> new_row(rows[i], new_n_rows, new_row_capacity);
	std::swap(rows[i], new_row);
      }
      row_capacity = new_row_capacity;
    }
    // Rows have grown or shrunk.
    row_size = new_n_rows;
  }
}

template <typename T>
void
DB_Matrix<T>::resize_no_copy(const dimension_type new_n_rows) {
  dimension_type old_n_rows = rows.size();

  if (new_n_rows > old_n_rows) {
    // Rows will be inserted.
    if (new_n_rows <= row_capacity) {
      // We can recycle the old rows.
      if (rows.capacity() < new_n_rows) {
	// Reallocation (of vector `rows') will take place.
	std::vector<DB_Row<T> > new_rows;
	new_rows.reserve(compute_capacity(new_n_rows));
	new_rows.insert(new_rows.end(), new_n_rows, DB_Row<T>());
	// Construct the new rows (be careful: each new row must have
	// the same capacity as each one of the old rows).
	dimension_type i = new_n_rows;
	while (i-- > old_n_rows)
	  new_rows[i].construct(new_n_rows, row_capacity);
	// Steal the old rows.
	++i;
	while (i-- > 0)
	  new_rows[i].swap(rows[i]);
	// Put the new vector into place.
	std::swap(rows, new_rows);
      }
      else {
	// Reallocation (of vector `rows') will NOT take place.
	rows.insert(rows.end(), new_n_rows - old_n_rows, DB_Row<T>());
	// Be careful: each new row must have
	// the same capacity as each one of the old rows.
	for (dimension_type i = new_n_rows; i-- > old_n_rows; )
	  rows[i].construct(new_n_rows, row_capacity);
      }
    }
    else {
      // We cannot even recycle the old rows: allocate a new matrix and swap.
      DB_Matrix new_matrix(new_n_rows);
      swap(new_matrix);
      return;
    }
  }
  else if (new_n_rows < old_n_rows) {
    // Drop some rows.
    rows.erase(rows.begin() + new_n_rows, rows.end());
    // Shrink the existing rows.
    for (dimension_type i = new_n_rows; i-- > 0; )
      rows[i].shrink(new_n_rows);
    old_n_rows = new_n_rows;
  }
  // Here we have the right number of rows.
  if (new_n_rows > row_size) {
    // We need more columns.
    if (new_n_rows <= row_capacity)
      // But we have enough capacity: we resize existing rows.
      for (dimension_type i = old_n_rows; i-- > 0; )
	rows[i].expand_within_capacity(new_n_rows);
    else {
      // Capacity exhausted: we must reallocate the rows and
      // make sure all the rows have the same capacity.
      const dimension_type new_row_capacity = compute_capacity(new_n_rows);
      for (dimension_type i = old_n_rows; i-- > 0; ) {
	DB_Row<T> new_row(new_n_rows, new_row_capacity);
	std::swap(rows[i], new_row);
      }
      row_capacity = new_row_capacity;
    }
  }
  // DB_Rows have grown or shrunk.
  row_size = new_n_rows;
}

template <typename T>
void
DB_Matrix<T>::ascii_dump(std::ostream& s) const {
  const DB_Matrix<T>& x = *this;
  const char separator = ' ';
  const dimension_type nrows = x.num_rows();
  s << nrows << separator << std::endl;
  for (dimension_type i = 0; i < nrows;  ++i) {
    for (dimension_type j = 0; j < nrows; ++j) {
      using namespace IO_Operators;
      s << x[i][j] << separator;
    }
    s << std::endl;
  }
}

template <typename T>
bool
DB_Matrix<T>::ascii_load(std::istream& s) {
  dimension_type nrows;
   if (!(s >> nrows))
    return false;
  resize_no_copy(nrows);
  DB_Matrix& x = *this;
  for (dimension_type i = 0; i < nrows;  ++i)
    for (dimension_type j = 0; j < nrows; ++j) {
      using namespace IO_Operators;
      if (!(s >> x[i][j]))
	return false;
    }
  // Check for well-formedness.
  assert(OK());
  return true;
}


/*! \relates DB_Matrix */
template <typename T>
inline bool
operator==(const DB_Matrix<T>& x, const DB_Matrix<T>& y) {
  const dimension_type x_num_rows = x.num_rows();
  if (x_num_rows != y.num_rows())
    return false;
  for (dimension_type i = x_num_rows; i-- > 0; )
    if (x[i] != y[i])
      return false;
  return true;
}

template <typename T>
bool
DB_Matrix<T>::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  // The matrix must be square.
  if (num_rows() != row_size) {
#ifndef NDEBUG
    cerr << "DB_Matrix has fewer columns than rows:\n"
	 << "row_size is " << row_size
	 << ", num_rows() is " << num_rows() << "!"
	 << endl;
#endif
    return false;
  }

  const DB_Matrix& x = *this;
  const dimension_type n_rows = x.num_rows();
  for (dimension_type i = 0; i < n_rows; ++i) {
    if (!x[i].OK(row_size, row_capacity))
      return false;
  }

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::DB_Matrix */  //FIXME!!
template <typename T>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const DB_Matrix<T>& c) {
  const dimension_type n = c.num_rows();
  for (dimension_type i = 0; i < n; ++i) {
    for (dimension_type j = 0; j < n; ++j)
      s << c[i][j] << " ";
    s << std::endl;
  }
  return s;
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::DB_Matrix */
template <typename T>
inline void
swap(Parma_Polyhedra_Library::DB_Matrix<T>& x,
     Parma_Polyhedra_Library::DB_Matrix<T>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_DB_Matrix_inlines_hh)
