/* Row class declaration.
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

#ifndef PPL_Row_defs_hh
#define PPL_Row_defs_hh 1

#include "Row.types.hh"
#include "globals.defs.hh"
#include "Coefficient.defs.hh"
#include <vector>
#include <limits>

#ifndef PPL_ROW_EXTRA_DEBUG
#ifdef PPL_ABI_BREAKING_EXTRA_DEBUG
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  \brief
  Enables extra debugging information for class Row.

  \ingroup PPL_CXX_interface
  When <CODE>PPL_ROW_EXTRA_DEBUG</CODE> evaluates to <CODE>true</CODE>,
  each instance of the class Row carries its own capacity; this enables
  extra consistency checks to be performed.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
#define PPL_ROW_EXTRA_DEBUG 1
#else // !defined(PPL_ABI_BREAKING_EXTRA_DEBUG)
#define PPL_ROW_EXTRA_DEBUG 0
#endif // !defined(PPL_ABI_BREAKING_EXTRA_DEBUG)
#endif // !defined(PPL_ROW_EXTRA_DEBUG)

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The handler of the actual Row implementation.
/*! \ingroup PPL_CXX_interface
  Exception-safety is the only responsibility of this class: it has
  to ensure that its \p impl member is correctly deallocated.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
class Parma_Polyhedra_Library::Row_Impl_Handler {
public:
  //! Default constructor.
  Row_Impl_Handler();

  //! Destructor.
  ~Row_Impl_Handler();

  class Impl;

  //! A pointer to the actual implementation.
  Impl* impl;

#if PPL_ROW_EXTRA_DEBUG
  //! The capacity of \p impl (only available during debugging).
  dimension_type capacity_;
#endif // PPL_ROW_EXTRA_DEBUG

private:
  //! Private and unimplemented: copy construction is not allowed.
  Row_Impl_Handler(const Row_Impl_Handler&);

  //! Private and unimplemented: copy assignment is not allowed.
  Row_Impl_Handler& operator=(const Row_Impl_Handler&);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A finite sequence of coefficients.
/*! \ingroup PPL_CXX_interface */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
class Parma_Polyhedra_Library::Row : private Row_Impl_Handler {
public:
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Wrapper class to represent a set of flags with bits in a native
    unsigned integral type.
    \ingroup PPL_CXX_interface
  */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
  class Flags {
  public:
    //! Constructs an object with all the flags unset.
    Flags();

    //! Returns <CODE>true</CODE> if and only if \p *this and \p y are equal.
    bool operator==(const Flags& y) const;

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p *this and \p y
      are different.
    */
    bool operator!=(const Flags& y) const;

    PPL_OUTPUT_DECLARATIONS

    //! Uses the ASCII Flags representation from \p s to recreate *this.
    /*!
      Returns <CODE>true</CODE> if successful, <CODE>false</CODE>
      otherwise.  The ASCII representation is as output by
      \ref Parma_Polyhedra_Library::Row::Flags::ascii_dump.
    */
    bool ascii_load(std::istream& s);

  protected:
    //! A native integral type holding the bits that encode the flags.
    typedef unsigned int base_type;

    //! Index of the first bit derived classes can use.
    static const unsigned first_free_bit = 0;

    //! Total number of bits that can be stored.
    static const unsigned num_bits = std::numeric_limits<base_type>::digits;

    //! Constructs an object with flags set as in \p n.
    explicit Flags(base_type n);

    //! Returns the integer encoding \p *this.
    base_type get_bits() const;

    //! Sets the bits in \p mask.
    void set_bits(base_type mask);

    //! Resets the bits in \p mask.
    void reset_bits(base_type mask);

    /*! \brief
      Returns <CODE>true</CODE> if and only if all the bits
      in \p mask are set.
    */
    bool test_bits(base_type mask) const;

  private:
    //! The integer encoding \p *this.
    base_type bits;
  };

  class iterator;
  class const_iterator;

  //! Pre-constructs a row: construction must be completed by construct().
  Row();

  //! \name Post-constructors
  //@{
  //! Constructs properly a default-constructed element.
  /*!
    Builds a row with size and capacity \p sz and flags \p f.
  */
  void construct(dimension_type sz, Flags f);

  //! Constructs properly a default-constructed element.
  /*!
    \param sz
    The size of the row that will be constructed;

    \param capacity
    The capacity of the row that will be constructed;

    \param f
    Flags for the row that will be constructed.

    The row that is constructed has storage for \p capacity elements,
    \p sz of which are default-constructed now.
    The row flags are set to \p f.
  */
  void construct(dimension_type sz, dimension_type capacity, Flags f);
  //@} // Post-constructors

  //! Tight constructor: resizing may require reallocation.
  /*!
    Constructs a row with size and capacity \p sz, and flags \p f.
  */
  Row(dimension_type sz, Flags f);

  //! Sizing constructor with capacity.
  /*!
    \param sz
    The size of the row that will be constructed;

    \param capacity
    The capacity of the row that will be constructed;

    \param f
    Flags for the row that will be constructed.

    The row that is constructed has storage for \p capacity elements,
    \p sz of which are default-constructed now.
    The row flags are set to \p f.
  */
  Row(dimension_type sz, dimension_type capacity, Flags f);

  //! Ordinary copy constructor.
  Row(const Row& y);

  //! Copy constructor with specified capacity.
  /*!
    It is assumed that \p capacity is greater than or equal to
    the size of \p y.
  */
  Row(const Row& y, dimension_type capacity);

  //! Copy constructor with specified size and capacity.
  /*!
    It is assumed that \p sz is greater than or equal to the size of \p y
    and, of course, that \p sz is less than or equal to \p capacity.
  */
  Row(const Row& y, dimension_type sz, dimension_type capacity);

  //! Destructor.
  ~Row();

  //! Assignment operator.
  Row& operator=(const Row& y);

  //! Swaps \p *this with \p y.
  void swap(Row& y);

  //! Assigns the implementation of \p y to \p *this.
  /*!
    To be used with extra care, since it may easily cause memory leaks
    or undefined behavior.
  */
  void assign(Row& y);

  /*! \brief
    Allocates memory for a default constructed Row object, setting
    flags to \p f and allowing for \p capacity coefficients at most.

    It is assumed that no allocation has been performed before
    (otherwise, a memory leak will occur).
    After execution, the size of the Row object is zero.
  */
  void allocate(dimension_type capacity, Flags f);

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

  //! Returns a const reference to the flags of \p *this.
  const Flags& flags() const;

  //! Returns a non-const reference to the flags of \p *this.
  Flags& flags();

  //! Returns the size() of the largest possible Row.
  static dimension_type max_size();

  //! Gives the number of coefficients currently in use.
  dimension_type size() const;

  //! \name Subscript operators
  //@{
  //! Returns a reference to the element of the row indexed by \p k.
  Coefficient& operator[](dimension_type k);

  //! Returns a constant reference to the element of the row indexed by \p k.
  Coefficient_traits::const_reference operator[](dimension_type k) const;
  //@} // Subscript operators

  //! Normalizes the modulo of coefficients so that they are mutually prime.
  /*!
    Computes the Greatest Common Divisor (GCD) among the elements of
    the row and normalizes them by the GCD itself.
  */
  void normalize();

  //! Swaps the i-th element with the j-th element.
  //! Provided for compatibility with Sparse_Row
  void swap(dimension_type i, dimension_type j);

  //! Swaps the element pointed to by i with the element pointed to by j.
  //! Provided for compatibility with Sparse_Row
  void swap(iterator i, iterator j);

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  //! Resets the i-th element to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(dimension_type i);

  //! Resets the elements [first,last) to 0.
  //! Provided for compatibility with Sparse_Row
  void reset(dimension_type first, dimension_type last);

  //! Resets the element pointed to by itr to 0.
  //! Provided for compatibility with Sparse_Row.
  iterator reset(iterator itr);

  //! Gets the i-th element.
  //! Provided for compatibility with Sparse_Row.
  const Coefficient& get(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator find(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find(iterator itr, dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator find(const_iterator itr, dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator lower_bound(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator lower_bound(dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator lower_bound(iterator itr, dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  const_iterator lower_bound(const_iterator itr, dimension_type i) const;

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(dimension_type i, const Coefficient& x);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(dimension_type i);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(iterator itr, dimension_type i, const Coefficient& x);

  //! Provided for compatibility with Sparse_Row.
  iterator find_create(iterator itr, dimension_type i);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
    \param g should take a Coefficient& and a const Coefficient&.
    g(c1, c2) must do nothing if c1 is zero.
    f(c1) must be equivalent to g(c1, 0).
  */
  template <typename Func1, typename Func2>
  void combine_needs_first(const Row& y,
                           const Func1& f, const Func2& g);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, 0) must do nothing.
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2>
  void combine_needs_second(const Row& y,
                            const Func1& g, const Func2& h);

  //! Calls g(x[i],y[i]), for each i.
  /*!
    \param f should take a Coefficient&.
    \param g should take a Coefficient& and a const Coefficient&.
    \param h should take a Coefficient& and a const Coefficient&.
    g(c1, c2) must do nothing if both c1 and c2 are zero.
    f(c1) must be equivalent to g(c1, 0).
    h(c1, c2) must be equivalent to g(c1, c2) when c1 is zero.
  */
  template <typename Func1, typename Func2, typename Func3>
  void combine(const Row& y,
               const Func1& f, const Func2& g, const Func3& h);

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  /*! \brief
    Returns a lower bound to the size in bytes of the memory
    managed by \p *this.
  */
  memory_size_type external_memory_in_bytes() const;

  /*! \brief
    Returns the total size in bytes of the memory occupied by \p *this,
    provided the capacity of \p *this is given by \p capacity.
  */
  memory_size_type total_memory_in_bytes(dimension_type capacity) const;

  /*! \brief
    Returns the size in bytes of the memory managed by \p *this,
    provided the capacity of \p *this is given by \p capacity.
  */
  memory_size_type external_memory_in_bytes(dimension_type capacity) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  /*! \brief
    Checks if all the invariants are satisfied and that the actual
    size and capacity match the values provided as arguments.
  */
  bool OK(dimension_type row_size, dimension_type row_capacity) const;

private:
  //! Exception-safe copy construction mechanism for coefficients.
  void copy_construct_coefficients(const Row& y);

#if PPL_ROW_EXTRA_DEBUG
  //! Returns the capacity of the row (only available during debugging).
  dimension_type capacity() const;
#endif // PPL_ROW_EXTRA_DEBUG
};

class Parma_Polyhedra_Library::Row::iterator {
public:

  typedef std::pair<const dimension_type,Coefficient&> value_type;
  typedef std::pair<const dimension_type,const Coefficient&> const_type;

private:

  class Member_Access_Helper {
  public:

    Member_Access_Helper(dimension_type index, Coefficient& data);

    value_type* operator->();

  private:
    value_type value;
  };

  class Const_Member_Access_Helper {
  public:

    Const_Member_Access_Helper(dimension_type index,
                               const Coefficient& data);

    const const_type* operator->() const;

  private:
    const_type value;
  };

public:

  iterator();
  iterator(Row& row1, dimension_type i1);

  value_type operator*();
  const_type operator*() const;

  Member_Access_Helper operator->();
  Const_Member_Access_Helper operator->() const;

  iterator& operator++();
  iterator operator++(int);

  iterator& operator--();
  iterator operator--(int);

  bool operator==(const iterator& x) const;
  bool operator!=(const iterator& x) const;

  operator const_iterator() const;

  bool OK() const;

private:
  Row* row;
  dimension_type i;
};

class Parma_Polyhedra_Library::Row::const_iterator {
public:
  typedef std::pair<const dimension_type, const Coefficient&> const_type;

private:

  class Const_Member_Access_Helper {
  public:

    Const_Member_Access_Helper(dimension_type index,
                               const Coefficient& data);

    const const_type* operator->() const;

  private:
    const_type value;
  };

public:

  const_iterator();
  const_iterator(const Row& row1, dimension_type i1);

  const_type operator*() const;
  Const_Member_Access_Helper operator->() const;

  const_iterator& operator++();
  const_iterator operator++(int);

  const_iterator& operator--();
  const_iterator operator--(int);

  bool operator==(const const_iterator& x) const;
  bool operator!=(const const_iterator& x) const;

  bool OK() const;

private:
  const Row* row;
  dimension_type i;
};


namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equal.
/*! \relates Row */
bool operator==(const Row& x, const Row& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates Row */
bool operator!=(const Row& x, const Row& y);

} // namespace Parma_Polyhedra_Library


namespace std {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void swap(Parma_Polyhedra_Library::Row& x,
	  Parma_Polyhedra_Library::Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Specializes <CODE>std::iter_swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Row */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void iter_swap(std::vector<Parma_Polyhedra_Library::Row>::iterator x,
	       std::vector<Parma_Polyhedra_Library::Row>::iterator y);

} // namespace std


#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The actual implementation of a Row object.
/*! \ingroup PPL_CXX_interface
  The class Row_Impl_Handler::Impl provides the implementation of Row
  objects and, in particular, of the corresponding memory allocation
  functions.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
class Parma_Polyhedra_Library::Row_Impl_Handler::Impl {
public:
  //! \name Custom allocator and deallocator
  //@{

  //! Placement allocation function.
  /*!
    Allocates a chunk of memory able to contain \p capacity Coefficient
    objects beyond the specified \p fixed_size and returns a pointer to
    the newly allocated memory.
  */
  static void* operator new(size_t fixed_size, dimension_type capacity);

  //! Usual (non-placement) deallocation function.
  /*!
    Uses the standard delete operator to free the memory \p p points to.

    \note
    The definition of this custom deallocation function is required
    since otherwise the placement deallocation function
    <code>
    static void operator delete(void* p, dimension_type capacity);
    </code>
    would be wrongly interpreted as a usual (non-placement) deallocation
    function (see C++98 3.7.3.2p2). This happens because \c dimension_type
    is just an alias for \c std::size_t.
    See also http://gcc.gnu.org/bugzilla/show_bug.cgi?id=42115
  */
  static void operator delete(void* p);

  //! Placement deallocation function.
  /*!
    Uses the standard operator delete to free the memory \p p points to.
  */
  static void operator delete(void* p, dimension_type capacity);
  //@} // Custom allocator and deallocator

  //! Constructor.
  Impl(Row::Flags f);

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

  //! \name Flags accessors
  //@{
  //! Returns a const reference to the flags of \p *this.
  const Row::Flags& flags() const;

  //! Returns a non-const reference to the flags of \p *this.
  Row::Flags& flags();
  //@} // Flags accessors

  //! \name Size accessors
  //@{
  //! Returns the actual size of \p this.
  dimension_type size() const;

  //! Sets to \p new_size the actual size of \p *this.
  void set_size(dimension_type new_size);

  //! Increment the size of \p *this by 1.
  void bump_size();
  //@} // Size accessors

  //! \name Subscript operators
  //@{
  //! Returns a reference to the element of \p *this indexed by \p k.
  Coefficient& operator[](dimension_type k);

  //! Returns a constant reference to the element of \p *this indexed by \p k.
  Coefficient_traits::const_reference operator[](dimension_type k) const;
  //@} // Subscript operators

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes(dimension_type capacity) const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

private:
  //! The number of coefficients in the row.
  dimension_type size_;

  //! The flags of this row.
  Row::Flags flags_;

  //! The vector of coefficients.
  Coefficient vec_[
#if !PPL_CXX_SUPPORTS_FLEXIBLE_ARRAYS
	       1
#endif
  ];

  //! Private and unimplemented: default construction is not allowed.
  Impl();

  //! Private and unimplemented: copy construction is not allowed.
  Impl(const Impl& y);

  //! Private and unimplemented: assignment is not allowed.
  Impl& operator=(const Impl&);
};

#include "Row.inlines.hh"
#include "Row.templates.hh"

#endif // !defined(PPL_Row_defs_hh)
