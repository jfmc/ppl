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
#include "Unlimited_Sparse_Row.defs.hh"
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

  //! Removes column i from the matrix, shifting other columns to the left.
  void remove_column(dimension_type i);

  //! Permutes the columns of the matrix.
  /*!
    \param cycles
    A vector representing the non-trivial cycles of the permutation
    according to which the columns must be rearranged.

    The \p cycles vector contains, one after the other, the
    non-trivial cycles (i.e., the cycles of length greater than one)
    of a permutation of non-zero column indexes.  Each cycle is
    terminated by zero.  For example, assuming the matrix has 6
    columns, the permutation \f$ \{ 1 \mapsto 3, 2 \mapsto 4,
    3 \mapsto 6, 4 \mapsto 2, 5 \mapsto 5, 6 \mapsto 1 \}\f$ can be
    represented by the non-trivial cycles \f$(1 3 6)(2 4)\f$ that, in
    turn can be represented by a vector of 6 elements containing 1, 3,
    6, 0, 2, 4, 0.
  */
  void permute_columns(const std::vector<dimension_type>& cycles);

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
    Provided for compatibility with Dense_Matrix.

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

  //! Makes the matrix shrink by removing its \p n trailing columns.
  //! Provided for compatibility with Dense_Matrix.
  void remove_trailing_columns(const dimension_type n);

  bool ascii_load(std::istream& s);

  PPL_OUTPUT_DECLARATIONS

  //! Calls func on each row. func should take a Sparse_Matrix_Row& argument.
  template <typename Func>
  void for_each_row(const Func& func);

  //! Calls func on each row. func should take a const Unlimited_Sparse_Row&
  //! argument.
  template <typename Func>
  void for_each_row(const Func& func) const;

  /*! \brief
    Erases from the matrix all the rows but those having
    an index less than \p first_to_erase.

    Provided for compatibility with Dense_Row.
    It is equivalent to resize(first_to_erase,num_columns()).
  */
  void erase_to_end(dimension_type first_to_erase);

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
  //! A const iterator that may skip some zeros in the row.
  typedef Unlimited_Sparse_Row::const_iterator const_iterator;

  //! An iterator that may skip some zeros in the row.
  typedef Unlimited_Sparse_Row::iterator iterator;

  //! An iterator that may skip some zeros in the row.
  //! May be invalidated by apparently unrelated operations, use with care.
  //! See the method documentation for details.
  typedef Unlimited_Sparse_Row::dangerous_iterator dangerous_iterator;

  Sparse_Matrix_Row(Unlimited_Sparse_Row& row,const dimension_type size);

  //! Swaps this row with the row x. The two rows must have the same size.
  void swap(Sparse_Matrix_Row x);

  //! Swaps the i-th element with the j-th element.
  //! Iterators pointing to these elements are invalidated.
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  void swap(iterator i, iterator j);

  dimension_type size() const;

  //! Resets to zero the value pointed to by i.
  //! dangerous_iterator objects equal to i and ++i are invalidated.
  dangerous_iterator reset(dangerous_iterator i);

  //! Resets to zero the values in the range [first,last).
  //! All dangerous_iterator objects in [first,last] are invalidated (note
  //! that last is invalidated, too).
  dangerous_iterator reset(dangerous_iterator first,dangerous_iterator last);

  //! Resets to zero the i-th element.
  //! For each dangerous_iterator itr that pointed to i, dangerous_iterator
  //! objects equal to itr and ++itr are invalidated.
  void reset(dimension_type i);

  //! Resets to zero the elements in [i,j).
  //! For each dangerous_iterator i_itr that pointed to i, and j_itr that
  //! pointed to j, dangerous_iterator objects in [i_itr,j_itr] are
  //! invalidated (note that j_itr is invalidated, too).
  void reset(dimension_type i,dimension_type j);

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! For read-only access it's better to use get(), that avoids allocating
  //! space for zeroes. Both methods are O(n).
  //! If i was not previously stored, or reset(i) was called, this operation
  //! invalidates dangerous_iterator objects equal to the former
  //! lower_bound(i).
  Coefficient& operator[](const dimension_type i);

  //! Equivalent to get(), provided for convenience.
  const Coefficient& operator[](const dimension_type i) const;

  //! Gets the i-th element in the sequence.
  /*!
    This function is O(n).

    This function must not be called before main(), it relies on
    a static variable to work.
  */
  const Coefficient& get(const dimension_type i) const;

  //! A faster equivalent of p1 = &(get(c1)); p1 = &(get(c2));
  void get2(const dimension_type c1,const dimension_type c2,
            const Coefficient*& p1,const Coefficient*& p2) const;

  dangerous_iterator begin();
  dangerous_iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func,const dimension_type n);

  /*! \brief Executes func on each non-zero element and may execute it on some
             zeros.

      This signature is needed for compatibility with Dense_Row.
      \param func A functor that takes a (Coefficient&) or
                  (const Coefficient&) argument.
      \param n    The logical size of this row (ignored)
  */
  template <typename Func>
  void for_each_nonzero(const Func& func,const dimension_type n) const;

  dangerous_iterator find(const dimension_type c);
  dangerous_iterator lower_bound(const dimension_type c);
  dangerous_iterator upper_bound(const dimension_type c);
  const_iterator find(const dimension_type c) const;
  const_iterator lower_bound(const dimension_type c) const;
  const_iterator upper_bound(const dimension_type c) const;

  operator const Unlimited_Sparse_Row&() const;

  //! Checks the invariant.
  bool OK() const;

private:

  /*!
    @c applier_to_data 's @c operator() applies func to the second element of
    its argument.
  */
  template <typename Func>
  class applier_to_data :
    public std::unary_function<std::pair<dimension_type,Coefficient>&,void> {
  public:
    applier_to_data(const Func& func);
    void operator()(std::pair<dimension_type,Coefficient>& x) const;
  private:
    Func f;
  };

  template <typename Func>
  static applier_to_data<Func> apply_to_data(const Func& func);

private:

  Unlimited_Sparse_Row& row_;
  const dimension_type size_;
};


#include "Sparse_Matrix.inlines.hh"

#endif // !defined(PPL_Sparse_Matrix_defs_hh)
