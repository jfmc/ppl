/* Matrix class declaration.
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
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Matrix_defs_hh
#define PPL_Matrix_defs_hh 1

#include "Matrix.types.hh"
#include "Row.defs.hh"
#include "SatMatrix.types.hh"
#include "ConSys.types.hh"
#include "GenSys.types.hh"
#include "Integer.types.hh"
#include <vector>
#include <cstddef>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for systems of constraints and generators.
/*!
  An object of this class represents either a constraint system
  or a generator system.
  Each Matrix object can be viewed as a multiset of rows
  (where each row implements a constraint or a generator)
  and is characterized by the topological kind of the rows,
  by the matrix dimensions (the number of rows and columns)
  and by a Boolean flag that, when <CODE>true</CODE>,
  ensures that the rows of the matrix are sorted.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Matrix {
protected:
  //! Builds an empty matrix with specified topology.
  /*!
    Rows' size and capacity are initialized to \f$0\f$.
  */
  Matrix(Topology topol);

  //! Builds a matrix with specified topology and dimensions.
  /*!
    \param topol       The topology of the matrix that will be created.
    \param n_rows      The number of rows of the matrix that will be created.
    \param n_columns   The number of columns of the matrix
                       that will be created.

    This constructor creates an unsorted \p n_rows \f$\times\f$ \p n_columns
    matrix whose rows are all initialized to rays or points or inequalities
    of the given topology.
  */
  Matrix(Topology topol, dimension_type n_rows, dimension_type n_columns);

  //! Copy-constructor.
  Matrix(const Matrix& y);

  //! \brief
  //! Split-constructor: builds a matrix by stealing from \p y
  //! the rows having index greater or equal to \p first_stolen.
  /*!
    \param y              The matrix being split. On entry, it is assumed
                          that \p y has \p first_stolen + 1 rows at least.
                          On exit, it will have \p first_stolen rows.
    \param first_stolen   The index where \p y is split.
  */
  Matrix(Matrix& y, dimension_type first_stolen);
  
  //! Destructor.
  virtual ~Matrix();

  //! Assignment operator.
  Matrix& operator=(const Matrix& y);

public:
  //! An iterator over a matrix.
  /*!
    A const_iterator is used to provide read-only access
    to each row contained in a Matrix object.
  */
  class const_iterator {
  private:
    typedef std::vector<Row>::const_iterator Iter;
    //! The const iterator on the rows' vector \p rows.
    Iter i;

  public:
    typedef std::forward_iterator_tag iterator_category;
    typedef std::iterator_traits<Iter>::value_type value_type;
    typedef std::iterator_traits<Iter>::difference_type difference_type;
    typedef std::iterator_traits<Iter>::pointer pointer;
    typedef std::iterator_traits<Iter>::reference reference;

    //! Default constructor.
    const_iterator()
      : i(Iter()) {
    }

    //! \brief
    //! Builds a const iterator on the matrix starting from
    //! an iterator \p b on the elements of the vector \p rows.
    explicit const_iterator(const Iter& b)
      : i(b) {
    }

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y)
      : i(y.i) {
    }

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y) {
      i = y.i;
      return *this;
    }

    //! Dereference operator.
    reference operator*() const {
      return *i;
    }

    //! Indirect member selector.
    pointer operator->() const {
      return &*i;
    }

    //! Prefix increment operator.
    const_iterator& operator++() {
      ++i;
      return *this;
    }

    //! Postfix increment operator.
    const_iterator operator++(int) {
      return const_iterator(i++);
    }

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator==(const const_iterator& y) const {
      return i == y.i;
    }

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator!=(const const_iterator& y) const {
      return !operator==(y);
    }
  };

  //! \brief
  //! Returns the const_iterator pointing to the first row,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const {
    return const_iterator(rows.begin());
  }

  //! Returns the past-the-end const_iterator.
  const_iterator end() const {
    return const_iterator(rows.end());
  }

private:
  //! Contains the rows of the matrix.
  std::vector<Row> rows;

  //! The topological kind of the rows in the matrix.
  Topology row_topology;

  //! Size of the initialized part of each row.
  dimension_type row_size;

  //! \brief
  //! Capacity allocated for each row, i.e., number of
  //! <CODE>Integer</CODE> objects that each row can contain.
  dimension_type row_capacity;

  //! The index of the first pending row.
  dimension_type index_first_pending;

  //! \brief
  //! <CODE>true</CODE> if rows are sorted in the ascending order as
  //! defined by <CODE>bool operator<(const Row& x, const Row& y)</CODE>.
  //! If <CODE>false</CODE> we cannot conclude that rows are not sorted.
  bool sorted;

public:
  //! Swaps \p *this with \p y.
  void swap(Matrix& y);

  //! Sets the sortedness flag of the matrix to \p value.
  void set_sorted(bool value);

  //! Sets the matrix topology to <CODE>NECESSARILY_CLOSED</CODE>.
  void set_necessarily_closed();
  
  //! Sets the matrix topology to <CODE>NOT_NECESSARILY_CLOSED</CODE>.
  void set_not_necessarily_closed();
  
  //! Sets the topology of all rows equal to the matrix topology.
  void set_rows_topology();

  //! Sets the index to indicate that the matrix has no pending rows.
  void unset_pending_rows();

  //! Sets the index of the first pending row to \p first_pending.
  void set_index_first_pending_row(dimension_type first_pending);

  //! Makes the matrix grow by adding more rows and/or more columns.
  /*!
    \param new_n_rows      The number of rows of the
                           resized matrix.
    \param new_n_columns   The number of columns of the
                           resized matrix.

    A new matrix, with the specified dimensions, is created.
    The contents of the old matrix are copied upper, left-hand corner
    of the new matrix, which is then assigned to \p *this.
  */
  void grow(dimension_type new_n_rows, dimension_type new_n_columns);
  
  //! Resizes the matrix without worrying about the old contents.
  /*!
    \param new_n_rows      The number of rows of the
                           resized matrix.
    \param new_n_columns   The number of columns of the
                           resized matrix.
    
    A new matrix, with the specified dimensions, is created
    without copying the content of the old matrix and assigned
    to \p *this.
  */
  void resize_no_copy(dimension_type new_n_rows,
		      dimension_type new_n_columns);

  //! Adds \p n columns of zeros to the matrix.
  /*!
    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$r \times (c+n)\f$ matrix \f$(M \, 0)\f$.
  */
  void add_zero_columns(dimension_type n);
  
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

  //! Swaps the columns having indexes \p i and \p j.
  void swap_columns(dimension_type i,  dimension_type j);

  //! @name Accessors.
  //@{
  //! Returns the matrix topology.
  Topology topology() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if
  //! the matrix topology is <CODE>NECESSARILY_CLOSED</CODE>.
  bool is_necessarily_closed() const;

  //! Returns the value of the sortedness flag.
  bool is_sorted() const;

  //! Returns the space-dimension of the rows in the matrix.
  /*!
    The computation of the space dimension correctly ignores
    the column encoding the inhomogeneous terms of constraint
    (resp., the divisors of generators);
    if the matrix topology is <CODE>NOT_NECESSARILY_CLOSED</CODE>,
    also the column of the \f$\epsilon\f$-dimension coefficients
    will be ignored.
  */
  dimension_type space_dimension() const;

  //! \brief
  //! Returns the number of columns of the matrix
  //! (i.e., the size of the rows).
  dimension_type num_columns() const;

  //! Returns the number of rows in the matrix.
  dimension_type num_rows() const;

  //! Returns the index of the first pending row.
  dimension_type first_pending_row() const;

  //! Returns the number of rows that are in the pending part of the matrix.
  dimension_type num_pending_rows() const;

  //! \brief
  //! Returns the number of rows in the matrix
  //! that represent either lines or equalities.
  dimension_type num_lines_or_equalities() const;
  //@}

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is sorted,
  //! without checking for duplicates.
  bool check_sorted() const;

  //! @name Subscript operators.
  //@{
  //! Returns a reference to the \p k-th row of the matrix.
  Row& operator[](dimension_type k);

  //! Returns a constant reference to the \p k-th row of the matrix.
  const Row& operator[](dimension_type k) const;
  //@}

  //! Normalizes the matrix.
  void normalize();

  //! Strongly normalizes the matrix.
  void strong_normalize();

  //! \brief
  //! Sorts the non-pending rows (in growing order) and eliminates
  //! duplicated ones.
  void sort_rows();

  //! \brief
  //! Sorts the rows (in growing order) form \p first_row to
  //! \p last_row and eliminates duplicated ones.
  void sort_rows(dimension_type first_row, dimension_type last_row);
 
  //! \brief
  //! Sorts the pending rows and eliminates those that also occur
  //! in the non-pending part of the matrix.
  void sort_pending_and_remove_duplicates();
  
  //! Adds a copy of the given row to the matrix.
  void add_row(const Row& row);

  //! Adds a copy of the given row to the pending part of the matrix.
  void add_pending_row(const Row& row);

  //! Adds a new empty row to the matrix, setting only its type.
  void add_pending_row(Row::Type type);

  //! \brief
  //! Adds a copy of the given row to the matrix,
  //! automatically resizing the matrix or the row, if needed.
  void insert(const Row& row);

  //! \brief
  //! Adds a copy of the given row to the pending part of the matrix,
  //! automatically resizing the matrix or the row, if needed.
  void insert_pending(const Row& row);

  //! Adds to \p *this a copy of the rows of `y'.
  /*!
    It is assumed that \p *this has no pending rows.
  */
  void add_rows(const Matrix& y);

  //! Adds a copy of the rows of `y' to the pending part of `*this'.
  void add_pending_rows(const Matrix& y);

  //! \brief
  //! Assigns to \p *this the result of merging its rows with
  //! those of \p y, obtaining a sorted matrix.
  /*!
    Duplicated rows will occur only once in the result.
    Both matrices are assumed to be sorted on entry.
  */
  void merge_rows_assign(const Matrix& y);

  //! Clears the matrix deallocating all its rows.
  void clear();

  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
  /*!
    This virtual method prints the topology, the number of rows,
    the number of columns and the \p sorted flag.
    The specialized methods provided by ConSys and GenSys
    take care of properly printing the contents of the matrix.
  */
  virtual void ascii_dump(std::ostream& s) const;

  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
  /*!
    This virtual method is meant to read into a Matrix object
    the information produced by the output of <CODE>ascii_dump()</CODE>.
    The specialized methods provided by ConSys and GenSys
    take care of properly reading the contents of the matrix.
  */
  virtual bool ascii_load(std::istream& s);

  //! \brief
  //! Erases from the matrix all the rows but those having
  //! an index less than \p first_to_erase.
  void erase_to_end(dimension_type first_to_erase);

  //! \brief
  //! Sorts the matrix, removing duplicates,
  //! keeping the saturation matrix consistent.
  /*!
    \param sat   Saturation matrix with rows corresponding to
                 the rows of \p *this.
  */
  void sort_and_remove_with_sat(SatMatrix& sat);

  //! Minimizes the subsystem of equations contained in \p *this.
  /*!
    This method works only on the equalities of the matrix:
    the matrix is required to be partially sorted, so that
    all the equalities are grouped at its top.
    The method finds a minimal system for the equalities and
    returns its rank, i.e., the number of linearly independent equalities.
    The result is an upper triangular submatrix of equalities:
    for each equality, the pivot is chosen starting from
    the right-most columns.
  */
  dimension_type gauss();

  //! \brief
  //! Back-substitutes the coefficients to reduce
  //! the complexity of the matrix.
  /*!
    Takes an upper triangular matrix.
    For each row, starting from the one having the minimum number of
    coefficients different from zero, computes the expression of an element
    as a function of the remaining ones and then substitutes this expression
    in all the other rows.
  */
  void back_substitute(dimension_type rank);

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! Specializes <CODE>std::swap</CODE>.
  /*! \relates Parma_Polyhedra_Library::Matrix */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void swap(Parma_Polyhedra_Library::Matrix& x,
	  Parma_Polyhedra_Library::Matrix& y);

} // namespace std


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

} // namespace Parma_Polyhedra_Library

#include "Matrix.inlines.hh"

#endif // !defined(PPL_Matrix_defs_hh)
