/* Polynomial_Constraint_System class declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Polynomial_Constraint_System_defs_hh
#define PPL_Polynomial_Constraint_System_defs_hh 1

#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Polynomial_Constraint_System.types.hh"
#include "Polynomial.defs.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

// Put them in the namespace here to declare them friend later.

//! Returns <CODE>true</CODE> if and only if \p x is equivalent to \p y.
/*! \relates Polynomial_Constraint */
bool
operator==(const Polynomial_Constraint& x, const Polynomial_Constraint& y);

//! Returns <CODE>true</CODE> if and only if \p x is not equivalent to \p y.
/*! \relates Polynomial_Constraint */
bool
operator!=(const Polynomial_Constraint& x, const Polynomial_Constraint& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
std::ostream& operator<<(std::ostream& s, const Polynomial_Constraint_System& c);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint_System */
void swap(Parma_Polyhedra_Library::Polynomial_Constraint_System& x,
	  Parma_Polyhedra_Library::Polynomial_Constraint_System& y);

} // namespace std

//! A system of polynomial constraints.
/*! \ingroup PPL_CXX_interface
  FIXME: to be written.
*/
class Parma_Polyhedra_Library::Polynomial_Constraint_System {
public:
  //! Default constructor: builds an empty system of constraints.
  Polynomial_Constraint_System();

  //! Builds the singleton system containing the only constraint \p c.
  explicit Polynomial_Constraint_System(const Polynomial_Constraint& pc);

  //! Ordinary copy-constructor.
  Polynomial_Constraint_System(const Polynomial_Constraint_System& y);

  //! Destructor.
  ~Polynomial_Constraint_System();

  //! Assignment operator.
  Polynomial_Constraint_System&
  operator=(const Polynomial_Constraint_System& y);

  /*! \brief
    Returns the maximum space dimension a Polynomial_Constraint_System
    can handle.
  */
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    contains one or more strict inequality constraints.
  */
  bool has_strict_inequalities() const;

  /*! \brief
    Removes all the constraints from the constraint system
    and sets its space dimension to 0.
  */
  void clear();

  /*! \brief
    Inserts in \p *this a copy of the constraint \p c,
    increasing the number of space dimensions if needed.
  */
  void insert(const Polynomial_Constraint& pc);

  //! Returns the number of constraints.
  dimension_type num_constraints() const;

  //! Returns the number of equality constraints.
  dimension_type num_equalities() const;

  //! Returns the number of inequality constraints.
  dimension_type num_inequalities() const;

  /*! \brief
    Returns the singleton system containing only
    Polynomial_Constraint::zero_dim_false().
  */
  static const Polynomial_Constraint_System& zero_dim_empty();

private:
    //! The implementation vector type.
    typedef std::vector<Polynomial_Constraint> Vec;

public:
  //! An iterator over a system of polynomial constraints.
  /*!
    A const_iterator is used to provide read-only access
    to each constraint contained in a Polynomial_Constraint_System object.

    \par Example
    The following code prints the system of constraints
    defining the polynomial cone <CODE>pcone</CODE>:
    \code
  const Polynomial_Constraint_System pcs;
  pcone.get_constraints(pcs);
  for (Polynomial_Constraint_System::const_iterator i = pcs.begin(),
         pcs_end = pcs.end(); i != pcs_end; ++i)
    cout << *i << endl;
    \endcode
  */
  class const_iterator
    : public std::iterator<std::forward_iterator_tag,
			   Polynomial_Constraint,
			   void,
			   const Polynomial_Constraint*,
			   const Polynomial_Constraint&> {
  public:
    //! Default constructor.
    const_iterator();

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Destructor.
    ~const_iterator();

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    const Polynomial_Constraint& operator*() const;

    //! Indirect member selector.
    const Polynomial_Constraint* operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    /*! \brief
      Returns <CODE>true</CODE> if and only if
      \p *this and \p y are identical.
    */
    bool operator==(const const_iterator& y) const;

    /*! \brief
      Returns <CODE>true</CODE> if and only if
      \p *this and \p y are different.
    */
    bool operator!=(const const_iterator& y) const;

  private:
    //! The implementation const iterator.
    Vec::const_iterator i;

    //! Constructor.
    const_iterator(const Vec::const_iterator& iter);

    friend class Polynomial_Constraint_System;
  };

  /*! \brief
    Returns the const_iterator pointing to the first constraint,
    if \p *this is not empty;
    otherwise, returns the past-the-end const_iterator.
  */
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Writes to \p s an ASCII representation of the internal
    representation of \p *this.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    \ref ascii_dump) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Swaps \p *this with \p y.
  void swap(Polynomial_Constraint_System& y);

private:
  //! The implementation vector.
  Vec vec;

  friend class const_iterator;
};

#include "Polynomial_Constraint_System.inlines.hh"

#endif // !defined(PPL_Polynomial_Constraint_System_defs_hh)
