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

#include "Integer.defs.hh"
#include "Row.defs.hh"
#include "globals.defs.hh"
#include <algorithm>
#include <iostream>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Row::Impl::grow_no_copy(const dimension_type new_sz) {
  assert(new_sz >= size());
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  // vec[0] is already constructed.
  if (size() == 0 && new_sz > 0)
    bump_size();
#endif
  for (dimension_type i = size(); i < new_sz; ++i) {
    new (&vec_[i]) Integer();
    bump_size();
  }
}

void
PPL::Row::Impl::shrink(const dimension_type new_sz) {
#if !CXX_SUPPORTS_FLEXIBLE_ARRAYS
  assert(new_sz > 0);
#endif
  assert(new_sz <= size());
  // We assume construction was done "forward".
  // We thus perform destruction "backward".
  for (dimension_type i = size(); i-- > new_sz; )
    // ~Integer() does not throw exceptions.  So we do.
    vec_[i].~Integer();
  set_size(new_sz);
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
PPL::Row::normalize() {
  Row& x = *this;
  // Compute the GCD of all the coefficients into gcd.
  TEMP_INTEGER(gcd);
  gcd = 0;
  const dimension_type sz = size();
  for (dimension_type i = sz; i-- > 0; ) {
    const Integer& x_i = x[i];
    if (x_i != 0)
      gcd_assign(gcd, x_i);
  }
  if (gcd > 1)
    // Divide the coefficients by the GCD.
    for (dimension_type i = sz; i-- > 0; )
      exact_div_assign(x[i], gcd);
}

void
PPL::Row::sign_normalize() {
  if (is_line_or_equality()) {
    Row& x = *this;
    const dimension_type sz = x.size();
    // `first_non_zero' indicates the index of the first
    // coefficient of the row different from zero, disregarding
    // the very first coefficient (inhomogeneous term / divisor).
    dimension_type first_non_zero;
    for (first_non_zero = 1; first_non_zero < sz; ++first_non_zero)
      if (x[first_non_zero] != 0)
	break;
    if (first_non_zero < sz)
      // If the first non-zero coefficient of the row is negative,
      // we negate the entire row.
      if (x[first_non_zero] < 0) {
	for (dimension_type j = first_non_zero; j < sz; ++j)
	  negate(x[j]);
	// Also negate the first coefficient.
	negate(x[0]);
      }
  }
}

bool
PPL::Row::check_strong_normalized() const {
  Row tmp = *this;
  tmp.strong_normalize();
  return compare(*this, tmp) == 0;
}

/*! \relates Parma_Polyhedra_Library::Row */
int
PPL::compare(const Row& x, const Row& y) {
  const bool x_is_line_or_equality = x.is_line_or_equality();
  const bool y_is_line_or_equality = y.is_line_or_equality();
  if (x_is_line_or_equality != y_is_line_or_equality)
    // Equalities (lines) precede inequalities (ray/point).
    return y_is_line_or_equality ? 2 : -2;

  // Compare all the coefficients of the row starting from position 1.
  const dimension_type xsz = x.size();
  const dimension_type ysz = y.size();
  const dimension_type min_sz = std::min(xsz, ysz);
  dimension_type i;
  for (i = 1; i < min_sz; ++i)
    if (const int comp = cmp(x[i], y[i]))
      // There is at least a different coefficient.
      return (comp > 0) ? 2 : -2;

  // Handle the case where `x' and `y' are of different size.
  if (xsz != ysz) {
    for( ; i < xsz; ++i)
      if (const int sign = sgn(x[i]))
	return (sign > 0) ? 2 : -2;
    for( ; i < ysz; ++i)
      if (const int sign = sgn(y[i]))
	return (sign < 0) ? 2 : -2;
  }

  // If all the coefficients in `x' equal all the coefficients in `y'
  // (starting from position 1) we compare coefficients in position 0,
  // i.e., inhomogeneous terms.
  if (const int comp = cmp(x[0], y[0]))
    return (comp > 0) ? 1 : -1;

  // `x' and `y' are equal.
  return 0;
}

/*! \relates Parma_Polyhedra_Library::Row */
void
PPL::scalar_product_assign(Integer& z, const Row& x, const Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size());
  z = 0;
  for (dimension_type i = x.size(); i-- > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Row */
void
PPL::reduced_scalar_product_assign(Integer& z, const Row& x, const Row& y) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  assert(!x.is_necessarily_closed());
  assert(x.size() - 1 <= y.size());
  z = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; )
    // The following line optimizes the computation
    // of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Row */
void
PPL::homogeneous_scalar_product_assign(Integer& z,
				       const Row& x, const Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size());
  z = 0;
    // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = x.size(); --i > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

void
PPL::Row::linear_combine(const Row& y, const dimension_type k) {
  Row& x = *this;
  // We can combine only vector of the same dimension.
  assert(x.size() == y.size());
  assert(y[k] != 0 && x[k] != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  TEMP_INTEGER(normalized_x_k);
  TEMP_INTEGER(normalized_y_k);
  normalize2(x[k], y[k], normalized_x_k, normalized_y_k);
  for (dimension_type i = size(); i-- > 0; )
    if (i != k) {
      Integer& x_i = x[i];
      x_i *= normalized_y_k;
      sub_mul_assign(x_i, y[i], normalized_x_k);
    }
  x[k] = 0;
  x.strong_normalize();
}

bool
PPL::Row::all_homogeneous_terms_are_zero() const {
  const Row& x = *this;
  for (dimension_type i = x.size(); --i > 0; )
    if (x[i] != 0)
      return false;
  return true;
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
  if (capacity_ != row_capacity) {
    cerr << "Row capacity mismatch: is " << capacity_
	 << ", should be " << row_capacity << "."
	 << endl;
    is_broken = true;
  }
#endif
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
  // Topology consistency check.
  const dimension_type min_cols = is_necessarily_closed() ? 1 : 2;
  if (size() < min_cols) {
#ifndef NDEBUG
    cerr << "Row has fewer coefficeints than the minumum "
	 << "allowed by its topology:"
	 << endl
	 << "size is " << size()
	 << ", minimum is " << min_cols << "."
	 << endl;
#endif
    is_broken = true;
  }
  return !is_broken;
}
