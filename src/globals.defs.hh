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

#ifndef PPL_globals_defs_hh
#define PPL_globals_defs_hh 1

#include "Coefficient.defs.hh"
#include <exception>
#include <cstddef>

namespace Parma_Polyhedra_Library {

//! An unsigned integral type for representing space dimensions.
typedef size_t dimension_type;

//! An unsigned integral type for representing memory size in bytes.
typedef size_t memory_size_type;

//! Returns a value that does not designate a valid dimension.
inline dimension_type
not_a_dimension();

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
  //! Worst-case polynomial complexity.
  POLYNOMIAL_COMPLEXITY,
  //! Worst-case exponential complexity but typically polynomial behavior.
  SIMPLEX_COMPLEXITY,
  //! Any complexity.
  ANY_COMPLEXITY
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! FIXME: comment!
#endif
class Coefficient_free_list_element {
private:
  Coefficient i;
  Coefficient_free_list_element* p;

public:
  Coefficient_free_list_element()
    : i() {
  }

  Coefficient& integer() {
    return i;
  }

  Coefficient_free_list_element*& next() {
    return p;
  }
};

extern Coefficient_free_list_element* Coefficient_free_list_first;

inline Coefficient&
get_tmp_Coefficient() {
  Coefficient* p;
  if (Coefficient_free_list_first != 0) {
    p = &Coefficient_free_list_first->integer();
    Coefficient_free_list_first = Coefficient_free_list_first->next();
  }
  else
    p = reinterpret_cast<Coefficient*>(new Coefficient_free_list_element());
  return *p;
}

inline void
release_tmp_Coefficient(Coefficient& i) {
  Coefficient_free_list_element& e
    = reinterpret_cast<Coefficient_free_list_element&>(i);
  e.next() = Coefficient_free_list_first;
  Coefficient_free_list_first = &e;
}

class Temp_Coefficient_Holder {
private:
  Coefficient& hold;

public:
  Temp_Coefficient_Holder(Coefficient& i)
    : hold(i) {
  }
  ~Temp_Coefficient_Holder() {
    release_tmp_Coefficient(hold);
  }
};

#if 1
#define TEMP_INTEGER(id) \
Coefficient& id = get_tmp_Coefficient(); \
Temp_Coefficient_Holder temp_Coefficient_holder_ ## id = (id)
#else
#define TEMP_INTEGER(id) static Coefficient id
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Speculative allocation function.
/*!
  \return
  The actual capacity to be allocated.

  \param requested_size
  The number of elements we need.

  \param maximum_size
  The maximum number of elements to be allocated. It is assumed
  to be no less than \p requested_size.

  Computes a capacity given a requested size.
  Allows for speculative allocation aimed at reducing the number of
  reallocations enough to guarantee amortized constant insertion time
  for our vector-like data structures. In all cases, the speculative
  allocation will not exceed \p maximum_size.
*/
#endif
inline dimension_type
compute_capacity(const dimension_type requested_size,
		 const dimension_type maximum_size);

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
maybe_abandon();

//! A tag class.
/*! Tag class to differentiate the C_Polyhedron and NNC_Polyhedron
    constructors that build a polyhedron out of a bounding box.
*/
struct From_Bounding_Box {
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! If \f$g\f$ is the GCD of \p x and \p y, the values of \p x and \p y
//! divided by \f$g\f$ are assigned to \p nx and \p ny, respectively.
/*!
  \note \p x and \p nx may be the same object and likewise for
        \p y and \p ny.  Any other aliasing results in undefined
	behavior.
*/
#endif
void
normalize2(const Coefficient& x, const Coefficient& y,
	   Coefficient& nx, Coefficient& ny);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns a mask for the lowest \p n bits,
#endif
template <typename T>
T low_bits_mask(unsigned n);

} // namespace Parma_Polyhedra_Library

#include "globals.inlines.hh"

#endif // !defined(PPL_globals_defs_hh)
