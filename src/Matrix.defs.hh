/* Matrix class declaration.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Matrix_defs_hh
#define _Matrix_defs_hh 1

#include "Matrix.types.hh"
#include "Row.defs.hh"
#include "SatMatrix.types.hh"
#include "ConSys.types.hh"
#include "GenSys.types.hh"
#include "Integer.types.hh"
#include <vector>
#include <cstddef>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  An object of this class is a matrix.
  It is characterized by the number of rows, by the number of columns
  and by a Boolean element \p sorted that says if a matrix is sorted
  or not.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Matrix {
protected:
  //! Default constructor: builds a zero-matrix.
  Matrix();
  //! Constructor: bulids a sorted matrix with \p num_rows rows
  //! and \p num_columns columns.
  Matrix(size_t num_rows, size_t num_columns);
  //! Copy-constructor.
  Matrix(const Matrix& y);
  //! Destructor.
  virtual ~Matrix();

  //! Assignment operator.
  Matrix& operator=(const Matrix& y);

  class const_iterator {
  private:
    typedef std::vector<Row>::const_iterator Iter;
    Iter i;

  public:
    typedef std::forward_iterator_tag iterator_category;
    typedef std::iterator_traits<Iter>::value_type value_type;
    typedef std::iterator_traits<Iter>::difference_type difference_type;
    typedef std::iterator_traits<Iter>::pointer pointer;
    typedef std::iterator_traits<Iter>::reference reference;

    const_iterator()
      : i(Iter()) {
    }

    explicit const_iterator(const Iter& b)
      : i(b) {
    }

    const_iterator(const const_iterator& y)
      : i(y.i) {
    }

    const_iterator& operator=(const const_iterator& y) {
      i = y.i;
      return *this;
    }

    reference operator*() const {
      return *i;
    }

    pointer operator->() const {
      return &*i;
    }

    const_iterator& operator++() {
      ++i;
      return *this;
    }

    const_iterator operator++(int) {
      return const_iterator(i++);
    }

    bool operator==(const const_iterator& y) const {
      return i == y.i;
    }

    bool operator!=(const const_iterator& y) const {
      return !operator==(y);
    }
  };

public:
  const_iterator begin() const {
    return const_iterator(rows.begin());
  }

  const_iterator end() const {
    return const_iterator(rows.end());
  }

private:
  //! Contains the rows of the matrix.
  std::vector<Row> rows;
  //! Size of the initialized part of each row.
  size_t row_size;
  //! Capacity allocated for each row, i.e., number of
  //! <CODE>Integer</CODE> objects that each row can contain.
  size_t row_capacity;
  //! <CODE>true</CODE> if rows are sorted in the ascending order as
  //! defined by <CODE>bool operator<(const Row& x, const Row& y)</CODE>.
  //! If <CODE>false</CODE> we cannot conclude that rows are not sorted.
  bool sorted;

public:
  //! Swaps \p *this with \p y.
  void swap(Matrix& y);

  //! Sets the \p sorted flag of the matrix to \p value.
  void set_sorted(bool value);

  //! Make the matrix grow adding more rows and/or more columns.
  void grow(size_t new_num_rows, size_t new_num_columns);
  //! Resizes the matrix without worrying about the old contents.
  void resize_no_copy(size_t new_num_rows, size_t new_num_columns);

  //! Turn the matrix \f$M\f$ into \f$(M \, 0)\f$.
  void add_zero_columns(size_t n);
  //! Turn the matrix \f$M\f$ into \f$\bigl({0 \atop M}{J \atop 0}\bigr)\f$.
  void add_rows_and_columns(size_t n);

  //! Accessories
  //@{
  bool is_sorted() const;
  size_t num_columns() const;
  size_t num_rows() const;
  size_t num_lines_or_equalities() const;
  //@}

  //! Checks whether a Matrix is sorted.
  //! It does NOT check for duplicates.
  bool check_sorted() const;

  //! @name Subscript operators.
  //@{
  Row& operator[](size_t k);
  const Row& operator[](size_t k) const;
  //@}
  //! Normalize the matrix.
  void normalize();
  //! Strongly normalize the matrix.
  void strong_normalize();
  //! Sorts the rows (in growing order) and eliminates duplicated ones.
  void sort_rows();
  //! Merges rows of \p y with rows of \p *this obtaining a sorted matrix.
  void merge_rows_assign(const Matrix& y);
  //! Adds a new empty row to the matrix setting its type to \p type.
  void add_row(Row::Type type);
  //! Adds a copy of the given row \p row to the matrix.
  void add_row(const Row& row);
  //! Adds a copy of the given row \p row to the matrix.
  void insert(const Row& row);
  //! Clears the matrix deallocating all its rows.
  void clear();

  //! Input/Output.
  //@{
  virtual void get(std::istream& s);
  virtual void print(std::ostream& s) const;
  //@}

  //! Erases from the matrix a set of previous selected rows.
  void erase_to_end(size_t first_to_erase);

  //! Sorts the matrix keeping the saturation matrix consistent
  //! and removes duplicates.
  void sort_and_remove_with_sat(SatMatrix& sat);
  //! Minimizes a system of equations.
  size_t gauss();
  //! Back-substitutes the coefficients to reduce
  //! the complexity of the matrix.
  void back_substitute(size_t rank);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

namespace std {
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*!
    Specialize std::swap to use the fast swap that is provided
    as a member function instead of using the default algorithm
    (which creates a temporary and uses assignment).
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void swap(Parma_Polyhedra_Library::Matrix& x,
	    Parma_Polyhedra_Library::Matrix& y);
}

namespace Parma_Polyhedra_Library {
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Returns <CODE>true</CODE> if and only if \p x and \p y are identical.
  /*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool operator==(const Matrix& x, const Matrix& y);
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
  /*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool operator!=(const Matrix& x, const Matrix& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Input operator.
  /*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  std::istream& operator>>(std::istream& s, Matrix& m);
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Output operator.
  /*! \relates Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  std::ostream& operator<<(std::ostream& s, const Matrix& m);
}

#include "Matrix.inlines.hh"

#endif
