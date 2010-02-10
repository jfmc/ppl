/* Sparse_Matrix class declaration.
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

#ifndef PPL_Sparse_Matrix_defs_hh
#define PPL_Sparse_Matrix_defs_hh 1

#include "Sparse_Matrix.types.hh"
#include "Sparse_Row.defs.hh"
#include "globals.types.hh"
#include <vector>

class Parma_Polyhedra_Library::Sparse_Matrix {

public:
  class iterator;
  typedef std::vector<Unlimited_Sparse_Row>::const_iterator const_iterator;

  //! Constructs a square matrix filled with zeroes with the given size.
  Sparse_Matrix(dimension_type n=0);

  //! Constructs a matrix filled with zeroes with the given dimensions.
  Sparse_Matrix(dimension_type num_rows,dimension_type num_columns);

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  Sparse_Matrix_Row operator[](dimension_type i);
  const Unlimited_Sparse_Row& operator[](dimension_type i) const;

  dimension_type num_rows() const;
  dimension_type num_columns() const;

  void resize(dimension_type n);
  void resize(dimension_type num_rows,dimension_type num_columns);

  //! Adds to the matrix \p n rows of zeroes.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of rows to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times c\f$ matrix \f$\genfrac{(}{)}{0pt}{}{M}{0}\f$.
  */
  void add_zero_rows(const dimension_type n);

  //! Adds \p n columns of zeroes to the matrix.
  /*!
    Provided for compatibility with Dense_Matrix.

    \param n
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$r \times (c+n)\f$ matrix \f$(M \, 0)\f$.
  */
  void add_zero_columns(const dimension_type n);

  //! Adds \p n rows and \p m columns of zeroes to the matrix.
  /*!
    \param n
    The number of rows to be added: must be strictly positive.

    \param m
    The number of columns to be added: must be strictly positive.

    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+m)\f$ matrix
    \f$\bigl(\genfrac{}{}{0pt}{}{M}{0} \genfrac{}{}{0pt}{}{0}{0}\bigr)\f$.
  */
  void add_zero_rows_and_columns(const dimension_type n,
                                 const dimension_type m);

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Calls func on each row. func should take a Sparse_Matrix_Row& argument.
  template <typename Func>
  void for_each_row(Func func);

  //! Calls func on each row. func should take a const Unlimited_Sparse_Row&
  //! argument.
  template <typename Func>
  void for_each_row(Func func) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  std::vector<Unlimited_Sparse_Row> rows;
  dimension_type num_columns_;
};

class Parma_Polyhedra_Library::Sparse_Matrix::iterator {

public:
  iterator(const iterator&);

  Sparse_Matrix_Row operator*();

  iterator& operator++();
  iterator operator++(int);

private:
  iterator(std::vector<Unlimited_Sparse_Row>::iterator,
           const dimension_type size);

  std::vector<Unlimited_Sparse_Row>::iterator itr;
  const dimension_type size_;

  friend class Parma_Polyhedra_Library::Sparse_Matrix;
};

class Parma_Polyhedra_Library::Sparse_Matrix_Row {

public:
  Sparse_Matrix_Row(Unlimited_Sparse_Row& row,const dimension_type size);

  //! A const iterator that may skip some zeros in the row.
  typedef Unlimited_Sparse_Row::const_iterator const_iterator;

  //! An iterator that may skip some zeros in the row.
  typedef Unlimited_Sparse_Row::iterator iterator;

  //! Resets to zero the value pointed by i.
  iterator reset(iterator i);

  //! Resets to zero the values in the range [first,last).
  iterator reset(iterator first,iterator last);

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  iterator find(const dimension_type c);
  iterator lower_bound(const dimension_type c);
  iterator upper_bound(const dimension_type c);
  const_iterator find(const dimension_type c) const;
  const_iterator lower_bound(const dimension_type c) const;
  const_iterator upper_bound(const dimension_type c) const;

  operator const Unlimited_Sparse_Row&() const;
  operator Sparse_Row() const;

  //! Checks the invariant.
  bool OK() const;

private:
  Unlimited_Sparse_Row& row_;
  const dimension_type size_;
};

#include "Sparse_Matrix.inlines.hh"

#endif // !defined(PPL_Sparse_Matrix_defs_hh)
