/* DB_Row class declaration.   
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
site: http://www.cs.unipr.it/ppl/ .*/

#ifndef PPL_DB_Row_defs_hh
#define PPL_DB_Row_defs_hh 1

#include "DB_Row.types.hh"
#include "globals.hh"
#include "Ptr_Iterator.defs.hh"
#include <cstddef>
#include <vector>

#ifndef EXTRA_ROW_DEBUG
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! When EXTRA_ROW_DEBUG evaluates to <CODE>true</CODE>, each instance
//! of the class DB_Row carries its own capacity; this enables extra
//! consistency checks to 5 be performed.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define EXTRA_ROW_DEBUG 0
#endif


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The handler of the actual DB_Row implementation.
/*!
  Exception-safety is the only responsibility of this class: it has
  to ensure that its \p impl member is correctly deallocated.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
class Parma_Polyhedra_Library::DB_Row_Impl_Handler {
public:
  //! Default constructor.
  DB_Row_Impl_Handler();

  //! Destructor.
  ~DB_Row_Impl_Handler();

  class Impl;

  //! A pointer to the actual implementation.
  Impl* impl;

#if EXTRA_ROW_DEBUG
  //! The capacity of \p impl (only available during debugging).
  dimension_type capacity_;
#endif // EXTRA_ROW_DEBUG

private:
  //! Private and unimplemented: copy construction is not allowed.
  DB_Row_Impl_Handler(const DB_Row_Impl_Handler&);

  //! Private and unimplemented: copy assignment is not allowed.
  DB_Row_Impl_Handler& operator=(const DB_Row_Impl_Handler&);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for the single rows of matrices.
/*!
  The templatic class DB_Row<T> allow the efficient representation of
  the single rows of a matrix. It contains elements of type T stored 
  as a vector. The class T is a family of extended numbers that
  must provide representation for 
  \f$ -\infty \f$, \f$0\f$,\f$ +\infty \f$ (and, consequently for <EM>nan</EM>,
  <EM>not a number</EM>, since this arises as the ``result'' of 
  undefined sums like \f$ +\infty + (-\infty) \f$).

  The class T must provide the following methods:
  
  \code
    T()
  \endcode
  is the default constructor: no assumption is made on the particular
  object constructed, provided <CODE>T().OK()</CODE> gives <CODE>true</CODE>
  (see below). 
  \code
    ~T()
  \endcode
  is the destructor.
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
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
class Parma_Polyhedra_Library::DB_Row : private DB_Row_Impl_Handler<T> {
public:

  //! Pre-constructs a row: construction must be completed by construct().
  DB_Row();

  //! \name Post-constructors.
  //@{
  //! Constructs properly a default-constructed element.
  /*!
    Builds a row with size \p sz and minimum capacity.
  */
  void construct(dimension_type sz);

  //! Constructs properly a default-constructed element.
  /*!
    \param sz         The size of the row that will be constructed.
    \param capacity   The minimum capacity of the row that will be constructed.
    
    The row that we are constructing has a minimum capacity, i.e., it
    can contain at least \p capacity elements, \p sz of which will be
    constructed now.
  */
  void construct(dimension_type sz, dimension_type capacity);
  //@}

  //! Tight constructor: resizing will require reallocation.
  DB_Row(dimension_type sz);

  //! Sizing constructor with capacity.
  DB_Row(dimension_type sz, dimension_type capacity);

  //! Ordinary copy constructor.
  DB_Row(const DB_Row& y);

  //! Copy constructor with specified capacity.
  /*!
    It is assumed that \p capacity is greater than or equal to \p y size.
  */
  DB_Row(const DB_Row& y, dimension_type capacity);

  //! Copy constructor with specified size and capacity.
  /*!
    It is assumed that \p sz is greater than or equal to the size of \p y
    and, of course, that \p sz is less than or equal to \p capacity.
  */
  DB_Row(const DB_Row& y, dimension_type sz, dimension_type capacity);

  //! Destructor.
  ~DB_Row();

  //! Assignment operator.
  DB_Row& operator=(const DB_Row& y);

  //! Swaps \p *this with \p y.
  void swap(DB_Row& y);

  //! Assigns the implementation of \p y to \p *this.
  void assign(DB_Row& y);

  //! \brief.
  //! Allocates memory for a default constructed DB_Row object,
  //! allowing for \p capacity coefficients at most.
  /*!
    It is assumed that no allocation has been performed before
    (otherwise, a memory leak will occur).
    After execution, the size of the DB_Row object is zero.
  */
  void allocate(dimension_type capacity);

  //! Expands the row to size \p new_size.
  /*!
    Adds new positions to the implementation of the row
    obtaining a new row with size \p new_size.
    It is assumed that \p new_size is between the current size
    and capacity of the row.
  */
  void expand_within_capacity(dimension_type new_size);

  //! Shrinks the row by erasing elements at the end.
  /*!
    Destroys elements of the row implementation
    from position \p new_size to the end.
    It is assumed that \p new_size is not greater than the current size.
  */
  void shrink(dimension_type new_size);

  //! Returns the size() of the largest possible DB_Row.
  static dimension_type max_size();

  //! Gives the number of coefficients currently in use.
  dimension_type size() const;

  //! \name Subscript operators.
  //@{
  //! Returns a reference to the element of the row indexed by \p k.
  T& operator[](dimension_type k);

  //! Returns a constant reference to the element of the row indexed by \p k.
  const T& operator[](dimension_type k) const;
  //@}

  //! A (non const) random access iterator to access the row's elements.
  typedef Implementation::Ptr_Iterator<T*> iterator;

  //! A const random access iterator to access the row's elements.
  typedef Implementation::Ptr_Iterator<const T*> const_iterator;

  //! \brief
  //! Returns the const iterator pointing to the first element,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const iterator.
  iterator begin();

  //! Returns the past-the-end iterator.
  iterator end();

  //! \brief
  //! Returns the const iterator pointing to the first element,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const iterator.
  const_iterator end() const;

  //! Checks if all the invariants are satisfied.
  bool OK(dimension_type row_size, dimension_type row_capacity) const;

private:
  //! Exception-safe copy construction mechanism for coefficients.
  void copy_construct_coefficients(const DB_Row& y);

#if EXTRA_ROW_DEBUG
  //! Returns the capacity of the row (only available during debugging).
  dimension_type capacity() const;
#endif // defined(EXTRA_ROW_DEBUG)
};

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \name Classical comparison operators.
//@{
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \relates DB_Row */
template <typename T>
bool operator==(const DB_Row<T>& x, const DB_Row<T>& y);

/*! \relates DB_Row */
template <typename T>
bool operator!=(const DB_Row<T>& x, const DB_Row<T>& y);
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//@}
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

} // namespace Parma_Polyhedra_Library


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The real implementation of a DB_Row object.
/*!
  The class DB_Row_Impl_Handler::Impl provides the implementation of
  DB_Row objects and, in particular, of the corresponding memory
  allocation functions.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
class Parma_Polyhedra_Library::DB_Row_Impl_Handler<T>::Impl {
public:
  //! \name Custom allocator and deallocator.
  //@{

  /*! \brief
    Allocates a chunk of memory able to contain \p capacity T objects
    beyond the specified \p fixed_size and returns a pointer to the new
    allocated memory.
  */
  static void* operator new(size_t fixed_size, dimension_type capacity);

  //! Uses the standard delete operator to free the memory \p p points to.
  static void operator delete(void* p);

  //! \brief Placement version:
  //! uses the standard operator delete to free the memory \p p points to.
  static void operator delete(void* p, dimension_type capacity);
  //@}

  //! Default constructor.
  Impl();

  //! Destructor.
  /*!
    Uses <CODE>shrink()</CODE> method with argument \f$0\f$
    to delete all the row elements.
  */
  ~Impl();

  //! Expands the row to size \p new_size.
  /*!
    It is assumed that \p new_size is between the current size and capacity.
  */
  void expand_within_capacity(dimension_type new_size);

  //! Shrinks the row by erasing elements at the end.
  /*!
    It is assumed that \p new_size is not greater than the current size.
  */
  void shrink(dimension_type new_size);

  //! Exception-safe copy construction mechanism for coefficients.
  void copy_construct_coefficients(const Impl& y);

  //! Returns the size() of the largest possible Impl.
  static dimension_type max_size();

  //! \name Size accessors.
  //@{
  //! Returns the actual size of \p this.
  dimension_type size() const;

  //! Sets to \p new_sz the actual size of \p *this.
  void set_size(dimension_type new_sz);

  //! Increment the size of \p *this by 1.
  void bump_size();
  //@}

  //! \name Subscript operators.
  //@{
  //! Returns a reference to the element of \p *this indexed by \p k.
  T& operator[](dimension_type k);

  //! Returns a constant reference to the element of \p *this indexed by \p k.
  const T& operator[](dimension_type k) const;
  //@}

private:
  friend class Parma_Polyhedra_Library::DB_Row<T>;

  //! The number of coefficients in the row.
  dimension_type size_;

  //! The vector of coefficients.
  T vec_[
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       1
#endif
  ];

  //! Private and unimplemented: copy construction is not allowed.
  Impl(const Impl& y);

  //! Private and unimplemented: assignment is not allowed.
  Impl& operator=(const Impl&);

  //! Exception-safe copy construction mechanism.
  void copy_construct(const Impl& y);
};

namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::DB_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
void swap(Parma_Polyhedra_Library::DB_Row<T>& x,
	  Parma_Polyhedra_Library::DB_Row<T>& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::iter_swap</CODE>.
/*! \relates Parma_Polyhedra_Library::DB_Row */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
void iter_swap(typename std::vector<Parma_Polyhedra_Library::DB_Row<T> >
	       ::iterator x,
	       typename std::vector<Parma_Polyhedra_Library::DB_Row<T> >
	       ::iterator y);

} // namespace std

#ifndef EXTRA_NORMALIZATION
// If non-zero, lines and equalities are ALWAYS normalized so that the
// first non-zero coefficient is positive.
#define EXTRA_NORMALIZATION 0
#endif

#include "DB_Row.inlines.hh"

#endif // !defined(PPL_DB_Row_defs_hh)
