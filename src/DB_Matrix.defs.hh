/* DB_Matrix class declaration.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ .  */

#ifndef PPL_DB_Matrix_defs_hh
#define PPL_DB_Matrix_defs_hh 1

#include "DB_Matrix.types.hh"
#include "DB_Row.defs.hh"
#include <vector>
#include <cstddef>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::DB_Matrix */
template <typename T>
std::ostream&
operator<<(std::ostream& s, const DB_Matrix<T>& c);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for the square matrices.
/*!
  The templatic class DB_Matrix<T> allow the representation of the
  square matrices.
  Each DB_Matrix<T> object can be viewed as a multiset of DB_Row<T>.
  The class T is a family of extended numbers that must provide 
  representation for \f$ -\infty \f$, \f$0\f$,\f$ +\infty \f$ 
  (and, consequently for <EM>nan</EM>, <EM>not a number</EM>, 
  since this arises as the ``result'' of undefined sums 
  like \f$ +\infty + (-\infty) \f$).
  
  The class T must provide the following methods:

  \code
    bool is_nan() const      
  \endcode
  returns <CODE>true</CODE> if and only \p *this represents 
  the  <EM>not a number</EM> value.
  \code
    bool OK() const 
  \endcode
  returns <CODE>true</CODE> if and only if \p *this satisfies all
  its invariants.  
  \code
    std::ostream& operator<<(std::ostream& s, const T& x)
  \endcode
  writes a textual representation of \p x to \p s.
  \code  
    std::istream& operator>>(std::istream& s, T& x)
  \endcode
  reads a textual representation of an object of type T and
  assigns it to \p x.

*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

template <typename T>
class Parma_Polyhedra_Library::DB_Matrix {

public:
  //! Returns the maximum number of rows of a DB_Matrix.
  static dimension_type max_num_rows();

  //! Returns the maximum number of columns of a DB_Matrix.
  static dimension_type max_num_columns();

  //! Builds a square matrix.
  /*!
    DB_Rows' size and capacity are initialized to \f$0\f$.
  */
  DB_Matrix();
  
  //! Builds a matrix with specified dimensions.
  /*!
    \param n_rows      The number of rows and columns of the matrix that
                       will be created.

    This constructor creates a square \p n_rows \f$\times\f$ \p n_rows
    matrix.
  */
  DB_Matrix(dimension_type n_rows);

  //! Copy-constructor.
  DB_Matrix(const DB_Matrix& y);

  //! Destructor.
  virtual ~DB_Matrix();

  //! Assignment operator.
  DB_Matrix& operator=(const DB_Matrix& y);

public:

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! An iterator over a matrix.
  /*!
    A const_iterator is used to provide read-only access
    to each row contained in a DB_Matrix object.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  class const_iterator {
  private:
    typedef typename std::vector<DB_Row<T> >::const_iterator Iter;
    //! The const iterator on the rows' vector \p rows.
    Iter i;

  public:
    typedef std::forward_iterator_tag iterator_category;
    typedef typename std::iterator_traits<Iter>::value_type value_type;
    typedef typename std::iterator_traits<Iter>::difference_type difference_type;
    typedef typename std::iterator_traits<Iter>::pointer pointer;
    typedef typename std::iterator_traits<Iter>::reference reference;

    //! Default constructor.
    const_iterator();

    //! \brief
    //! Builds a const iterator on the matrix starting from
    //! an iterator \p b on the elements of the vector \p rows.
    explicit const_iterator(const Iter& b);

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    reference operator*() const; 

    //! Indirect member selector.
    pointer operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator==(const const_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator!=(const const_iterator& y) const;
  };

  //! \brief
  //! Returns the const_iterator pointing to the first row,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

private:
  //! Contains the rows of the matrix.
  std::vector<DB_Row<T> > rows;


  //! Size of the initialized part of each row.
  dimension_type row_size;

  //! \brief
  //! Capacity allocated for each row, i.e., number of
  //! <CODE>long</CODE> objects that each row can contain.
  dimension_type row_capacity;

public:
  //! Swaps \p *this with \p y.
  void swap(DB_Matrix& y);


  //! Makes the matrix grow by adding more rows and more columns.
  /*!
    \param new_n_rows      The number of rows and columns of the
                           resized matrix.

    A new matrix, with the specified dimension, is created.
    The contents of the old matrix are copied upper, left-hand corner
    of the new matrix, which is then assigned to \p *this.
  */
  void grow(dimension_type new_n_rows);

  //! Resizes the matrix without worrying about the old contents.
  /*!
    \param new_n_rows      The number of rows and columns of the
                           resized matrix.

    A new matrix, with the specified dimension, is created
    without copying the content of the old matrix and assigned
    to \p *this.
  */
  void resize_no_copy(dimension_type new_n_rows);

  //! Adds \p n non-zero rows and columns to the matrix.
  /*!
    \param n      The number of rows and columns to be added.

    Turn the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+n)\f$ matrix
    \f$\bigl({0 \atop M}{J \atop 0}\bigr)\f$,
    where \f$J\f$ is the specular image
    of the \f$n \times n\f$ identity matrix.
  */
  void add_rows_and_columns(dimension_type n);

  //! Returns the space-dimension of the rows in the matrix.
  dimension_type space_dimension() const;

  //! Returns the number of rows in the matrix.
  dimension_type num_rows() const;

  //! \name Subscript operators.
  //@{
  //! Returns a reference to the \p k-th row of the matrix.
  DB_Row<T>& operator[](dimension_type k);

  //! Returns a constant reference to the \p k-th row of the matrix.
  const DB_Row<T>& operator[](dimension_type k) const;
  //@}

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  virtual void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
  /*!
    This virtual method is meant to read into a DB_Matrix object
    the information produced by the output of <CODE>ascii_dump()</CODE>.
  */
  virtual bool ascii_load(std::istream& s);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

namespace std {

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::DB_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
void swap(Parma_Polyhedra_Library::DB_Matrix<T>& x,
	  Parma_Polyhedra_Library::DB_Matrix<T>& y);

} // namespace std


namespace Parma_Polyhedra_Library {

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are identical.
/*! \relates DB_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
bool operator==(const DB_Matrix<T>& x, const DB_Matrix<T>& y);

#ifdef Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates DB_Matrix */
#endif // Parma_Polyhedra_Library_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
bool operator!=(const DB_Matrix<T>& x, const DB_Matrix<T>& y);

} // namespace Parma_Polyhedra_Library

#include "DB_Matrix.inlines.hh"

#endif // !defined(PPL_DB_Matrix_defs_hh)
