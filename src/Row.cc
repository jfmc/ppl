/* Row class implementation (non-inline functions).
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

#include <config.h>

#include "Row.defs.hh"
#include "Integer.defs.hh"
#include "globals.defs.hh"
#include <iostream>
#include <iomanip>
#include <cassert>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Row::Impl::expand_within_capacity(const dimension_type new_size) {
  assert(size() <= new_size && new_size <= max_size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  // vec_[0] is already constructed.
  if (size() == 0 && new_size > 0)
    bump_size();
#endif
  for (dimension_type i = size(); i < new_size; ++i) {
    new (&vec_[i]) Integer();
    bump_size();
  }
}

void
PPL::Row::Impl::shrink(dimension_type new_size) {
  assert(new_size <= size());
  // Since ~Integer() does not throw exceptions, nothing here does.
  set_size(new_size);
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  // Make sure we do not try to destroy vec_[0].
  if (new_size == 0)
    ++new_size;
#endif
  // We assume construction was done "forward".
  // We thus perform destruction "backward".
  for (dimension_type i = size(); i-- > new_size; )
    vec_[i].~Integer();
}

void
PPL::Row::Impl::copy_construct(const Impl& y) {
  const dimension_type y_size = y.size();
#if CXX_SUPPORTS_FLEXIBLE_ARRAYS
  for (dimension_type i = 0; i < y_size; ++i) {
    new (&vec_[i]) Integer(y.vec_[i]);
    bump_size();
  }
#else
  assert(y_size > 0);
  if (y_size > 0) {
    vec_[0] = y.vec_[0];
    bump_size();
    for (dimension_type i = 1; i < y_size; ++i) {
      new (&vec_[i]) Integer(y.vec_[i]);
      bump_size();
    }
  }
#endif
}

void
PPL::Row::Flags::ascii_dump(std::ostream& s) const {
  s << "0x"
    << std::hex
    << std::setw(2*sizeof(Flags::base_type))
    << std::setfill('0')
    << bits;
}

void
PPL::Row::ascii_dump(std::ostream& s) const {
  const Row& x = *this;
  dimension_type x_size = x.size();
  for (dimension_type i = 0; i < x_size; ++i)
    s << x[i] << ' ';
  s << "f ";
  flags().ascii_dump(s);
  s << std::endl;
}

bool
PPL::Row::OK(const dimension_type row_size,
	     const dimension_type
#if EXTRA_ROW_DEBUG
	     row_capacity
#endif
	     ) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  bool is_broken = false;
#if EXTRA_ROW_DEBUG
# if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  if (capacity_ == 0) {
    cerr << "Illegal row capacity: is 0, should be at least 1"
	 << endl;
    is_broken = true;
  }
  else if (capacity_ == 1 && row_capacity == 0)
    // This is fine.
    ;
  else
# endif
  if (capacity_ > max_size()) {
    cerr << "Row capacity exceeds the maximum allowed size:"
	 << endl
	 << "is " << capacity_
	 << ", should be less than or equal to " << max_size() << "."
	 << endl;
    is_broken = true;
  }
  if (capacity_ != row_capacity) {
    cerr << "Row capacity mismatch: is " << capacity_
	 << ", should be " << row_capacity << "."
	 << endl;
    is_broken = true;
  }
#endif
  if (size() > max_size()) {
#ifndef NDEBUG
    cerr << "Row size exceeds the maximum allowed size:"
	 << endl
	 << "is " << size()
	 << ", should be less than or equal to " << max_size() << "."
	 << endl;
#endif
    is_broken = true;
  }
  if (size() != row_size) {
#ifndef NDEBUG
    cerr << "Row size mismatch: is " << size()
	 << ", should be " << row_size << "."
	 << endl;
#endif
    is_broken = true;
  }
#if EXTRA_ROW_DEBUG
  if (capacity_ < size()) {
#ifndef NDEBUG
    cerr << "Row is completely broken: capacity is " << capacity_
	 << ", size is " << size() << "."
	 << endl;
#endif
    is_broken = true;
  }
#endif
  return !is_broken;
}

/*! \relates Parma_Polyhedra_Library::Row */ 
bool
PPL::operator==(const Row& x, const Row& y) {
  const dimension_type x_size = x.size();
  const dimension_type y_size = y.size();
  if (x_size != y_size)
    return false;

  if (x.flags() != y.flags())
    return false;

  for (dimension_type i = x_size; i-- > 0; )
    if (x[i] != y[i])
      return false;

  return true;
}
