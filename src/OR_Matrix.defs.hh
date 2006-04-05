/* OR_Matrix class declaration.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ .  */

#ifndef PPL_OR_Matrix_defs_hh
#define PPL_OR_Matrix_defs_hh 1

#include "globals.types.hh"
#include "Ptr_Iterator.defs.hh"
#include "OR_Matrix.types.hh"
#include "DB_Row.defs.hh"
#include "Checked_Number.defs.hh"
#include <vector>
#include <cstddef>
#include <iosfwd>
#include <deque>

#ifndef EXTRA_ROW_DEBUG
#define EXTRA_ROW_DEBUG 1
#endif

namespace Parma_Polyhedra_Library {

// Put it in the namespace here to declare it friend later.
template <typename T>
bool operator==(const OR_Matrix<T>& x, 
					 const OR_Matrix<T>& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::OR_Matrix */
template <typename T>
std::ostream&
operator<<(std::ostream& s, const OR_Matrix<T>& m);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for systems of constraints.
/*!
  An object of this class represents a constraint system.
  Each OR_Matrix object can be viewed as a multiset of rows
  (where each row implements a constraint system)and is
  characterized by the row dimensions (the number of rows).
  Given a constraint x_j - x_i <= c, at the i-th row and the j-th column
  in the OR_Matrix, we insert the element c. If there is not a constraint,
  we insert plus_infinity.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

template <typename T>
class Parma_Polyhedra_Library::OR_Matrix {
private:
  //! \brief
  //! An object that behaves like a matrix's row with respect to
  //! the subscript operators.
  template <typename U>
  class Pseudo_Row {
  public:
    //! \brief
    //! Copy-constructor allowing the construction of a const pseudo-row
    //! from a non-const pseudo-row.
    //! Ordinary copy constructor.
    template <typename V>
    Pseudo_Row(const Pseudo_Row<V>& y);

    //! Destructor.
    ~Pseudo_Row();

    //! Subscript operator.
    U& operator[](dimension_type k) const;

    // FIXME!!!
    //private:
  public:
    //! Holds a reference to the beginning of this row.
    U* first;

    //! Default constructor: creates a past-the-end object.
    Pseudo_Row();

#if EXTRA_ROW_DEBUG

    //! \brief
    //! Private constructor for a Pseudo_Row with size \p s beginning
    //! at \p y.
    Pseudo_Row(U& y, dimension_type s);

#else // !EXTRA_ROW_DEBUG

    //! Private constructor for a Pseudo_Row beginning at \p y.
    explicit Pseudo_Row(U& y);

#endif // !EXTRA_ROW_DEBUG

    //! Assignment operator.
    Pseudo_Row& operator=(const Pseudo_Row& y);

#if EXTRA_ROW_DEBUG

    //! The size of the row.
    dimension_type size_;

    //! Returns the size of the row.
    dimension_type size() const;

#endif // EXTRA_ROW_DEBUG

    template <typename V> friend class Pseudo_Row;
    template <typename V> friend class any_row_iterator;
    friend class OR_Matrix;
  }; // class Pseudo_Row

public:
  //! A (non const) reference to a matrix's row.
  typedef Pseudo_Row<T> row_reference_type;

  //! A const reference to a matrix's row.
  typedef Pseudo_Row<const T> const_row_reference_type;

private:
  //! \brief
  //! A template class to derive both OR_Matrix::iterator
  //! and OR_Matrix::const_iterator.
  template <typename U>
  class any_row_iterator {
  public:
    typedef std::random_access_iterator_tag iterator_category;
    typedef Pseudo_Row<U> value_type;
    typedef long difference_type;
    typedef const Pseudo_Row<U>* pointer;
    typedef const Pseudo_Row<U>& reference;

    //! Constructor to build past-the-end objects.
    any_row_iterator(dimension_type n_rows);

    //! \brief
    //! Builds an iterator pointing at the beginning of an OR_Matrix whose
    //! first element is \p base;
    explicit any_row_iterator(U& base);

    //! \brief
    //! Copy-constructor allowing the construction of a const_iterator
    //! from a non-const iterator.
    template <typename V>
    any_row_iterator(const any_row_iterator<V>& y);

    //! \brief
    //! Assignment operator allowing the assignment of a non-const iterator
    //! to a const_iterator.
    template <typename V>
    any_row_iterator& operator=(const any_row_iterator<V>& y);

    //! Dereference operator.
    reference operator*() const;

    //! Indirect member selector.
    pointer operator->() const;

    //! Prefix increment operator.
    any_row_iterator& operator++();

    //! Postfix increment operator.
    any_row_iterator operator++(int);

    //! Prefix decrement operator.
    any_row_iterator& operator--();

    //! Postfix decrement operator.
    any_row_iterator operator--(int);

    //! Subscript operator.
    reference operator[](difference_type m) const;

    //! Assignment-increment operator.
    any_row_iterator& operator+=(difference_type m);

    //! Assignment-decrement operator.
    any_row_iterator& operator-=(difference_type m);

    //! Returns the difference between \p *this and \p y.
    difference_type operator-(const any_row_iterator& y) const;

    //! Returns the sum of \p *this and \p m.
    any_row_iterator operator+(difference_type m) const;

    //! Returns the difference of \p *this and \p m.
    any_row_iterator operator-(difference_type m) const;

    //! Returns <CODE>true</CODE> if and only if \p *this is equal to \p y.
    bool operator==(const any_row_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if \p *this
    //! is different from \p y.
    bool operator!=(const any_row_iterator& y) const;

    //! Returns <CODE>true</CODE> if and only if \p *this is less than \p y.
    bool operator<(const any_row_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if \p *this is less than
    //! or equal to \p y.
    bool operator<=(const any_row_iterator& y) const;

    //! Returns <CODE>true</CODE> if and only if \p *this is greater than \p y.
    bool operator>(const any_row_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if \p *this is greater than
    //! or equal to \p y.
    bool operator>=(const any_row_iterator& y) const;

    dimension_type row_size() const;

    dimension_type index() const;

  private:
    //! Represents the beginning of a row.
    Pseudo_Row<U> value;

    //! External index.
    dimension_type e;

    //! Internal index: i = (e+1)*(e+1)/2.
    dimension_type i;

    template <typename V> friend class any_row_iterator;
  }; // class any_row_iterator

public:
  //! A (non const) row iterator.
  typedef any_row_iterator<T> row_iterator;

  //! A const row iterator.
  typedef any_row_iterator<const T> const_row_iterator;

  //! A (non const) element iterator.
  typedef typename DB_Row<T>::iterator element_iterator;

  //! A const element iterator.
  typedef typename DB_Row<T>::const_iterator const_element_iterator;

public:
  //! Returns the maximum number of rows of a DB_Matrix.
  static dimension_type max_num_rows();

  // Fixme: this comment sucks!!!! 
  //! Builds a `pseudo_triangular' matrix.
  /*!
    DB_Rows' size and capacity are initialized to \f$0\f$.
  */
  OR_Matrix();

  //! Builds a matrix with specified dimensions.
  /*!
    \param n_rows      The number of rows and columns of the matrix that
                       will be created.

    This constructor creates a square \p n_rows \f$\times\f$ \p n_columns
    matrix.
  */
  OR_Matrix(dimension_type n_rows);

  //! Copy-constructor.
  OR_Matrix(const OR_Matrix& y);

  //! Destructor.
  ~OR_Matrix();

  //! Assignment operator.
  OR_Matrix& operator=(const OR_Matrix& y);

private:
  //! Contains the rows of the matrix.
  /*!
    FIXME: insert here a description of the allocation of the rows (plural)
    of the OR_Matrix into the single row provided by `db_row' (singular).
  */
  DB_Row<T> vec;

  //! Contains the number of rows of the matrix.
  dimension_type num_rows_;

  //! Contains the capacity of the vec.
  dimension_type vec_capacity;

  //! \brief
  //! Returns the index into <CODE>vec</CODE> of the first element
  //! of the row of index \p k.
  static dimension_type row_first_element_index(dimension_type k);

public:
  //! \brief
  //! Returns the size of the row of index \p k.
  static dimension_type row_size(dimension_type k);
 
  //! Swaps \p *this with \p y.
  void swap(OR_Matrix& y);


  //! Makes the matrix grow by adding more rows and more columns.
  /*!
    \param new_n_rows      The number of rows  and columns of the
                           resized matrix.

    A new matrix, with the specified dimensions, is created.
    The contents of the old matrix are copied upper, left-hand corner
    of the new matrix, which is then assigned to \p *this.
  */
  void grow(dimension_type new_n_rows);

  //! Resizes the matrix without worrying about the old contents.
  /*!
    \param new_n_rows      The number of rows and columns of the
                           resized matrix.

    A new matrix, with the specified dimensions, is created
    without copying the content of the old matrix and assigned
    to \p *this.
  */
  void resize_no_copy(dimension_type new_n_rows);

  //! \brief
  //! Makes the matrix shrink by removing those rows having an index
  //! greater than or equal to \p new_n_rows.
  void remove_rows(dimension_type new_n_rows);

  //! Adds \p n non-zero rows and columns to the matrix.
  /*!
    \param n      The number of rows and columns to be added.

    Turn the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+n)\f$ matrix
    \f$\bigl({0 \atop M}{J \atop 0}\bigr)\f$,
    where \f$J\f$ is the specular image
    of the \f$n \times n\f$ identity matrix.
  */
  void add_rows(dimension_type n);

  //! Returns the space-dimension of the rows in the matrix.
  dimension_type space_dimension() const;

  //! Returns the number of rows in the matrix.
  dimension_type num_rows() const;

  //! \name Subscript operators.
  //@{
  //! Returns a reference to the \p k-th row of the matrix.
  row_reference_type operator[](dimension_type k);

  //! Returns a constant reference to the \p k-th row of the matrix.
  const_row_reference_type operator[](dimension_type k) const;
  //@}


  //! \brief
  //! Returns an iterator pointing to the first row,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  row_iterator row_begin();

  //! Returns the past-the-end const_iterator.
  row_iterator row_end();

  //! \brief
  //! Returns a const row iterator pointing to the first row,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_row_iterator row_begin() const;

  //! Returns the past-the-end const row iterator.
  const_row_iterator row_end() const;

  //! \brief
  //! Returns an iterator pointing to the first element,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  element_iterator element_begin();

  //! Returns the past-the-end const_iterator.
  element_iterator element_end();

  //! \brief
  //! Returns a const element iterator pointing to the first element,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_element_iterator element_begin() const;

  //! Returns the past-the-end const element iterator.
  const_element_iterator element_end() const;

  //! Clears the matrix deallocating all its rows.
  void clear();

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
  /*!
    This virtual method is meant to read into a OR_Matrix object
    the information produced by the output of <CODE>ascii_dump()</CODE>.
    The specialized methods provided by Constraint_System and 
    Generator_System take care of properly reading the contents 
    of the matrix.
  */
  bool ascii_load(std::istream& s);

  //! \brief
  //! Erases from the matrix all the rows but those having
  //! an index less than \p first_to_erase.
  void erase_to_end(dimension_type first_to_erase);

  friend bool Parma_Polyhedra_Library::operator==<T>(const OR_Matrix<T>& x, 
						     const OR_Matrix<T>& y);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

namespace std {

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::OR_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
void swap(Parma_Polyhedra_Library::OR_Matrix<T>& x,
	  Parma_Polyhedra_Library::OR_Matrix<T>& y);

} // namespace std


namespace Parma_Polyhedra_Library {

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are identical.
/*! \relates OR_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
bool operator==(const OR_Matrix<T>& x, const OR_Matrix<T>& y);

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates OR_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
bool operator!=(const OR_Matrix<T>& x, const OR_Matrix<T>& y);


} // namespace Parma_Polyhedra_Library

#include "OR_Matrix.inlines.hh"

#endif // !defined(PPL_OR_Matrix_defs_hh)
