/* Declarations of global objects.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_globals_hh
#define PPL_globals_hh 1

#include "Integer.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include <exception>
#include <limits>

namespace Parma_Polyhedra_Library {

//! An unsigned integral type for representing space dimensions.
typedef size_t dimension_type;

//! Returns a value that does not designate a valid dimension.
inline dimension_type
not_a_dimension() {
  return std::numeric_limits<dimension_type>::max();
}

//! Relation symbols.
enum Relation_Symbol {
  //! Less than.
  LESS_THAN,
  //! Less than or equal to.
  LESS_THAN_OR_EQUAL,
  //! Equal to.
  EQUAL,
  //! Greater than or equal to.
  GREATER_THAN_OR_EQUAL,
  //! Greater than.
  GREATER_THAN
};

//! Complexity pseudo-classes.
enum Complexity_Class {
  //! Worst-case polynomial.
  POLYNOMIAL,
  //! Worst-case exponential but typically polynomial.
  SIMPLEX,
  //! Any complexity.
  ANY
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! FIXME: comment!
#endif
class Integer_free_list_element {
private:
  Integer i;
  Integer_free_list_element* p;

public:
  Integer_free_list_element()
    : i() {
  }

  Integer& integer() {
    return i;
  }

  Integer_free_list_element*& next() {
    return p;
  }
};

extern Integer_free_list_element* Integer_free_list_first;

inline Integer&
get_tmp_Integer() {
  Integer* p;
  if (Integer_free_list_first != 0) {
    p = &Integer_free_list_first->integer();
    Integer_free_list_first = Integer_free_list_first->next();
  }
  else
    p = reinterpret_cast<Integer*>(new Integer_free_list_element());
  return *p;
}

inline void
release_tmp_Integer(Integer& i) {
  Integer_free_list_element& e = reinterpret_cast<Integer_free_list_element&>(i);
  e.next() = Integer_free_list_first;
  Integer_free_list_first = &e;
}

class Temp_Integer_Holder {
private:
  Integer& hold;

public:
  Temp_Integer_Holder(Integer& i)
    : hold(i) {
  }
  ~Temp_Integer_Holder() {
    release_tmp_Integer(hold);
  }
};

#define TEMP_INTEGER(id) \
Integer& id = get_tmp_Integer(); \
Temp_Integer_Holder temp_Integer_holder_ ## id = (id)

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Speculative allocation function.
/*!
  \return
  The actual capacity to be allocated.

  \param requested_size
  The number of elements we need.

  Computes a capacity given a requested size.
  Allows for speculative allocation aimed at reducing the number of
  reallocations enough to guarantee amortized constant insertion time
  for our vector-like data structures.
*/
#endif
inline dimension_type
compute_capacity(dimension_type requested_size) {
  // Speculation factor 2.
  return 2*(requested_size + 1);
  // Speculation factor 1.5.
  //return requested_size += ++requested_size/2;
}

//! User objects' the PPL can throw.
/*!
  This abstract base class should be instantiated by those users
  willing to provide a polynomial upper bound to the time spent
  by any invocation of a library operator.
*/
class Throwable {
public:
  //! Throws the user defined exception object.
  virtual void throw_me() const = 0;
};

//! This pointer, which is initialized to zero, is repeatedly checked
//! along any superlinear (i.e., computationally expensive) computation
//! path in the library.
//! When it is found nonzero the exception it points to is thrown.
//! In other words, making this pointer point to an exception (and
//! leaving it in this state) ensures that the library will return
//! control to the client application, possibly by throwing the given
//! exception, within a time that is a linear function of the size
//! of the representation of the biggest object (powerset of polyhedra,
//! polyhedron, system of constraints or generators) on which the library
//! is operating upon.
//! \note The only sensible way to assign to this pointer is from within
//!       a signal handler or from a parallel thread.  For this reason,
//!       the library, apart from ensuring that the pointer is initially
//!       set to zero, never assigns to it.  In particular, it does not
//!       zero it again when the exception is thrown: it is the client's
//!       responsibility to do so.
extern const Throwable* volatile abandon_expensive_computations;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! If the pointer abandon_expensive_computations is found
//! to be nonzero, the exception it points to is thrown.
/*! \relates Throwable */
#endif
inline void
maybe_abandon() {
  if (const Throwable* p = abandon_expensive_computations)
    p->throw_me();
}

//! A tag class.
/*! Tag class to differentiate the C_Polyhedron and NNC_Polyhedron
    constructors that build a polyhedron out of a bounding box.
*/
struct From_Bounding_Box {
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_globals_hh)
