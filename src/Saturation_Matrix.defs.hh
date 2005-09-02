/* Saturation_Matrix class declaration.
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

#ifndef PPL_Saturation_Matrix_defs_hh
#define PPL_Saturation_Matrix_defs_hh 1

#include "Saturation_Matrix.types.hh"
#include "Linear_System.defs.hh"
#include "Saturation_Row.defs.hh"
#include <vector>
#include <iosfwd>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A saturation matrix.
/*!
  A saturation matrix is used to encode the relation between the
  generators and the constraints of a polyhedron: if a generator
  saturates a constraint the corresponding element of the saturation
  matrix is \f$0\f$, otherwise (i.e., if the generator satisfies but
  does not saturate the constraint) the corresponding element is \f$1\f$.
  \note
  since the constraints and generators are taken from the same polyhedron
  description, it cannot be the case that a generator <EM>violates</EM>
  a constraint.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Saturation_Matrix {
public:
  //! Default constructor.
  Saturation_Matrix();

  //! \brief
  //! Construct a saturation matrix with \p n_rows rows
  //! and \p n_columns columns.
  Saturation_Matrix(dimension_type n_rows, dimension_type n_columns);

  //! Copy-constructor.
  Saturation_Matrix(const Saturation_Matrix& y);

  //! Destructor.
  ~Saturation_Matrix();

  //! Assignment operator.
  Saturation_Matrix& operator=(const Saturation_Matrix& y);

  //! Swaps \p *this with \p y.
  void swap(Saturation_Matrix& y);

  //! Subscript operator.
  Saturation_Row& operator[](dimension_type k);

  //! Constant subscript operator.
  const Saturation_Row& operator[](dimension_type k) const;

  //! Clears the matrix deallocating all its rows.
  void clear();

  //! Transposes the matrix.
  void transpose();

  //! Makes \p *this a transposed copy of \p y.
  void transpose_assign(const Saturation_Matrix& y);

  //! Returns the maximum number of rows of a Saturation_Matrix.
  static dimension_type max_num_rows();

  //! Returns the number of columns of \p *this.
  dimension_type num_columns() const;

  //! Returns the number of rows of \p *this.
  dimension_type num_rows() const;

  //! Sorts the rows and removes duplicates.
  void sort_rows();

  //! Looks for \p row in \p *this, which is assumed to be sorted.
  /*!
    \return
    <CODE>true</CODE> if \p row belongs to \p *this, false otherwise.

    \param row
    The row that will be searched for in the matrix.

    Given a sorted saturation matrix (this ensures better efficiency),
    tells whether it contains the given row.
  */
  bool sorted_contains(const Saturation_Row& row) const;

  //! Adds \p row to \p *this.
  void add_row(const Saturation_Row& row);

  //! Erases the rows from the \p first_to_erase -th to the last one.
  void rows_erase_to_end(dimension_type first_to_erase);

  //! Erases the columns from the \p first_to_erase -th to the last one.
  void columns_erase_to_end(dimension_type first_to_erase);

  //! Resizes the matrix copying the old contents.
  void resize(dimension_type new_n_rows, dimension_type new_n_columns);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

#ifndef NDEBUG
  //! Checks whether \p *this is sorted. It does NOT check for duplicates.
  bool check_sorted() const;
#endif

private:
  //! Contains the rows of the matrix.
  std::vector<Saturation_Row> rows;

  //! Size of the initialized part of each row.
  dimension_type row_size;

  //! Ordering predicate (used when implementing the sort algorithm).
  struct Saturation_Row_Less_Than {
    bool operator()(const Saturation_Row& x, const Saturation_Row& y) const;
  };

  friend
  void Parma_Polyhedra_Library::
  Linear_System::sort_and_remove_with_sat(Saturation_Matrix& sat);

};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Saturation_Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Saturation_Matrix& x,
	  Parma_Polyhedra_Library::Saturation_Matrix& y);

} // namespace std

#include "Saturation_Matrix.inlines.hh"

#endif // !defined(PPL_Saturation_Matrix_defs_hh)
