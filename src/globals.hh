/* Declarations of global objects.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _globals_hh
#define _globals_hh 1

#include "Integer.types.hh"
#include <exception>

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An array of temporaries used to avoid unnecessary memory allocation.
#endif
extern Integer* tmp_Integer;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Speculative allocation function.
/*!
  \param requested_size   The number of elements we need.

  \return                 The actual capacity to be allocated.

  Computes a capacity given a requested size.
  Allows for speculative allocation aimed at reducing the number of
  reallocations.
*/
#endif
inline size_t
compute_capacity(size_t requested_size) {
  return 2*(requested_size + 1);
}

//! Abstract base class for the user objects' the PPL can throw.
class Throwable {
public:
  virtual void throw_me() const = 0;
};

//! This pointer, which is initialized to zero, is repeatedly checked
//! along any exponential computation path in the library.
//! When it is found nonzero the exception it points to is thrown.
//! In other words, making this pointer point to an exception (and
//! leaving it in this state) ensures that the library will return
//! control to the client application, possibly by throwing the given
//! exception, within a time that is a linear function of the space
//! dimension of the object (polyhedron, system of constraints or
//! generators) of highest dimension on which the library is operating
//! upon.
//! \note The only sensible way to assign to this pointer is from within
//!       a signal handler or from a parallel thread.  For this reason,
//!       the library, apart from ensuring that the pointer is initially
//!       set to zero, never assigns to it.  In particular, it does not
//!       zero it again when the exception is thrown: it is the client's
//!       responsibility to do so.
extern const Throwable* volatile abandon_exponential_computations;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! If the pointer abandon_exponential_computations is found
//! to be nonzero, the exception it points to is thrown.
#endif
inline void
maybe_abandon() {
  if (abandon_exponential_computations) {
    const Throwable* p = abandon_exponential_computations;
    // Might have been zeroed since we checked.
    if (p)
      p->throw_me();
  }
}

} // namespace Parma_Polyhedra_Library

#endif
