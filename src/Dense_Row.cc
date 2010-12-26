/* Dense_Row class implementation (non-inline functions).
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

#include <ppl-config.h>

#include "Dense_Row.defs.hh"
#include "Coefficient.defs.hh"
#include <iostream>
#include <iomanip>
#include "assert.hh"

#include "Sparse_Row.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Dense_Row::resize(dimension_type new_size) {
  if (new_size <= size())
    shrink(new_size);
  else {
    if (new_size > capacity()) {
      // Reallocation is required.
      // TODO: Consider using realloc() here.
      // TODO: Consider using a smarter allocation strategy.
      dimension_type new_capacity = new_size;
      Coefficient* new_vec = static_cast<Coefficient*>(
          operator new(sizeof(Coefficient) * new_capacity));
      
      if (vec_ != 0) {
        memcpy(new_vec, vec_, sizeof(Coefficient) * size_);
        
        operator delete(vec_);
      }
      
      vec_ = new_vec;
      capacity_ = new_capacity;
    }
    PPL_ASSERT(new_size <= capacity_);
    // Construct the additional elements.
    while (size_ != new_size) {
      new (&vec_[size_]) Coefficient();
      size_++;
    }
  }
  PPL_ASSERT(size() == new_size);
  PPL_ASSERT(OK());
}

void
PPL::Dense_Row::resize(dimension_type new_size, dimension_type new_capacity) {
  PPL_ASSERT(new_size <= new_capacity);
  
  if (new_capacity == 0) {
    destroy();
    vec_ = 0;
    size_ = 0;
    capacity_ = 0;
    
    PPL_ASSERT(size() == new_size);
    PPL_ASSERT(capacity() == new_capacity);
    PPL_ASSERT(OK());
    
    return;
  }
  
  if (new_capacity < capacity()) {
    
    shrink(new_size);
    
    PPL_ASSERT(size_ == new_size);
    
    Coefficient* new_vec = static_cast<Coefficient*>(
        operator new(sizeof(Coefficient) * new_capacity));
    
    PPL_ASSERT(vec_ != 0);
    
    memcpy(new_vec, vec_, sizeof(Coefficient) * size_);
    
    operator delete(vec_);
    
    vec_ = new_vec;
    capacity_ = new_capacity;
  } else {
    if (new_capacity > capacity()) {
      
      Coefficient* new_vec = static_cast<Coefficient*>(
          operator new(sizeof(Coefficient) * new_capacity));
      
      if (vec_ != 0) {
        memcpy(new_vec, vec_, sizeof(Coefficient) * size_);
        
        operator delete(vec_);
      }
      
      vec_ = new_vec;
      capacity_ = new_capacity;
      
      resize(new_size);
    }
  }
  
  PPL_ASSERT(size() == new_size);
  PPL_ASSERT(capacity() == new_capacity);
  PPL_ASSERT(OK());
}

void
PPL::Dense_Row::expand_within_capacity(const dimension_type new_size) {
  PPL_ASSERT(new_size <= capacity_);
  PPL_ASSERT(size() <= new_size && new_size <= max_size());
  while (size_ != new_size) {
    new (&vec_[size_]) Coefficient();
    ++size_;
  }
  PPL_ASSERT(size() == new_size);
  PPL_ASSERT(OK());
}

void
PPL::Dense_Row::shrink(dimension_type new_size) {
  PPL_ASSERT(new_size <= size());
  // Since ~Coefficient() does not throw exceptions, nothing here does.

  // We assume construction was done "forward".
  // We thus perform destruction "backward".
  while (size_ != new_size) {
    --size_;
    vec_[size_].~Coefficient();
  }
  
  PPL_ASSERT(size() == new_size);
  PPL_ASSERT(OK());
}

PPL::Dense_Row::Dense_Row(const Sparse_Row& row) {
  init(row);
  PPL_ASSERT(size() == row.size());
  PPL_ASSERT(OK());
}

void
PPL::Dense_Row::init(const Sparse_Row& row) {
  size_ = 0;
  capacity_ = row.size();
  flags_ = row.flags();
  vec_ = static_cast<Coefficient*>(
      operator new(sizeof(Coefficient) * capacity_));
  Sparse_Row::const_iterator itr = row.begin();
  Sparse_Row::const_iterator itr_end = row.end();
  while (size_ != capacity_) {
    // Constructs (*this)[size_] with row[size_].
    if (itr != itr_end && itr.index() == size_) {
      new (&vec_[size_]) Coefficient(*itr);
      ++itr;
    } else
      new (&vec_[size_]) Coefficient();
    ++size_;
  }
  PPL_ASSERT(size() == row.size());
  PPL_ASSERT(OK());
}

PPL::Dense_Row&
PPL::Dense_Row::operator=(const Sparse_Row& row) {
  flags_ = row.flags();
  if (size() > row.size()) {
    // TODO: If the shrink() is modified to reallocate a smaller chunk,
    // this can be optimized.
    shrink(row.size());
    Sparse_Row::const_iterator itr = row.begin();
    Sparse_Row::const_iterator itr_end = row.end();
    for (dimension_type i = 0; i < size_; ++i) {
      // Computes (*this)[size_] = row[size_].
      if (itr != itr_end && itr.index() == i) {
        vec_[size_] = *itr;
        ++itr;
      } else
        vec_[size_] = Coefficient_zero();
    }
  } else {
    if (capacity() >= row.size()) {
      // size() <= row.size() <= capacity().
      Sparse_Row::const_iterator itr = row.begin();
      Sparse_Row::const_iterator itr_end = row.end();
      for (dimension_type i = 0; i < size_; ++i) {
        // The following code is equivalent to (*this)[i] = row[i].
        if (itr != itr_end && itr.index() == size_) {
          new (&vec_[size_]) Coefficient(*itr);
          ++itr;
        } else
          new (&vec_[size_]) Coefficient();
      }
      // Construct the additional elements.
      for ( ; size_ != row.size(); ++size_) {
        // Constructs (*this)[size_] with row[size_].
        if (itr != itr_end && itr.index() == size_) {
          new (&vec_[size_]) Coefficient(*itr);
          ++itr;
        } else
          new (&vec_[size_]) Coefficient();
      }
    } else {
      // Reallocation is required.
      destroy();
      init(row);
    }
  }
  PPL_ASSERT(size() == row.size());
  PPL_ASSERT(OK());

  return *this;
}

void
PPL::Dense_Row::normalize() {
  Dense_Row& x = *this;
  // Compute the GCD of all the coefficients.
  const dimension_type sz = size();
  dimension_type i = sz;
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  while (i > 0) {
    Coefficient_traits::const_reference x_i = x[--i];
    if (const int x_i_sign = sgn(x_i)) {
      gcd = x_i;
      if (x_i_sign < 0)
        neg_assign(gcd);
      goto compute_gcd;
    }
  }
  // We reach this point only if all the coefficients were zero.
  return;

compute_gcd:
  if (gcd == 1)
    return;
  while (i > 0) {
    Coefficient_traits::const_reference x_i = x[--i];
    if (x_i != 0) {
      // Note: we use the ternary version instead of a more concise
      // gcd_assign(gcd, x_i) to take advantage of the fact that
      // `gcd' will decrease very rapidly (see D. Knuth, The Art of
      // Computer Programming, second edition, Section 4.5.2,
      // Algorithm C, and the discussion following it).  Our
      // implementation of gcd_assign(x, y, z) for checked numbers is
      // optimized for the case where `z' is smaller than `y', so that
      // on checked numbers we gain.  On the other hand, for the
      // implementation of gcd_assign(x, y, z) on GMP's unbounded
      // integers we cannot make any assumption, so here we draw.
      // Overall, we win.
      gcd_assign(gcd, x_i, gcd);
      if (gcd == 1)
        return;
    }
  }
  // Divide the coefficients by the GCD.
  for (dimension_type j = sz; j-- > 0; ) {
    Coefficient& x_j = x[j];
    exact_div_assign(x_j, x_j, gcd);
  }
}

void
PPL::Dense_Row::reset(dimension_type first, dimension_type last) {
  PPL_ASSERT(first <= last);
  PPL_ASSERT(last <= size());
  for (dimension_type i = first; i < last; ++i)
    (*this)[i] = 0;
}

void
PPL::Dense_Row::linear_combine(const Dense_Row& y,
                               Coefficient_traits::const_reference coeff1,
                               Coefficient_traits::const_reference coeff2) {
  Dense_Row& x = *this;
  PPL_ASSERT(x.size() == y.size());
  for (dimension_type i = x.size(); i-- > 0; ) {
    Coefficient& x_i = x[i];
    x_i *= coeff1;
    // The test against 0 gives rise to a consistent speed up: see
    // http://www.cs.unipr.it/pipermail/ppl-devel/2009-February/014000.html
    Coefficient_traits::const_reference y_i = y[i];
    if (y_i != 0)
      add_mul_assign(x_i, y_i, coeff2);
  }
}

void
PPL::Dense_Row::ascii_dump(std::ostream& s) const {
  const Dense_Row& x = *this;
  const dimension_type x_size = x.size();
  s << "size " << x_size << " ";
  for (dimension_type i = 0; i < x_size; ++i)
    s << x[i] << ' ';
  s << "f ";
  flags().ascii_dump(s);
  s << "\n";
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Dense_Row)

bool
PPL::Dense_Row::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "size")
    return false;
  dimension_type new_size;
  if (!(s >> new_size))
    return false;

  resize(new_size);

  for (dimension_type col = 0; col < new_size; ++col)
    if (!(s >> (*this)[col]))
      return false;
  if (!(s >> str) || str != "f")
    return false;
  return flags_.ascii_load(s);
}

PPL::memory_size_type
PPL::Dense_Row::external_memory_in_bytes(dimension_type /* capacity */) const {
  return external_memory_in_bytes();
}

PPL::memory_size_type
PPL::Dense_Row::external_memory_in_bytes() const {
  memory_size_type n = capacity_ * sizeof(Coefficient);
  for (dimension_type i = size(); i-- > 0; )
    n += PPL::external_memory_in_bytes(vec_[i]);
  return n;
}

bool
PPL::Dense_Row::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  bool is_broken = false;

  if (capacity_ > max_size()) {
#ifndef NDEBUG
    cerr << "Dense_Row capacity exceeds the maximum allowed size:" << endl
         << "is " << capacity_
         << ", should be less than or equal to " << max_size() << "."
         << endl;
#endif
    is_broken = true;
  }

  if (size() > max_size()) {
#ifndef NDEBUG
    cerr << "Dense_Row size exceeds the maximum allowed size:" << endl
         << "is " << size()
         << ", should be less than or equal to " << max_size() << "." << endl;
#endif
    is_broken = true;
  }

  if (capacity_ < size()) {
#ifndef NDEBUG
    cerr << "Dense_Row is completely broken: capacity is " << capacity_
         << ", size is " << size() << "." << endl;
#endif
    is_broken = true;
  }
  
  if (capacity() == 0) {
    if (vec_ != 0)
      is_broken = true;
  } else {
    if (vec_ == 0)
      is_broken = true;
  }
  
  // TODO: Remove this.
  // It was added to help finding unconstructed Coefficients with valgrind.
  
  Coefficient sum;
  
  for (dimension_type i = size(); i-- > 0; )
    sum += vec_[i];

  return !is_broken;
}

bool
PPL::Dense_Row::OK(const dimension_type row_size,
                   const dimension_type row_capacity) const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  bool is_broken = !OK();

  // Check the declared capacity.
  if (capacity_ != row_capacity) {
#ifndef NDEBUG
    cerr << "Dense_Row capacity mismatch: is " << capacity_
         << ", should be " << row_capacity << "." << endl;
#endif // NDEBUG
    is_broken = true;
  }

  // Check the declared size.
  if (size() != row_size) {
#ifndef NDEBUG
    cerr << "Dense_Row size mismatch: is " << size()
         << ", should be " << row_size << "." << endl;
#endif
    is_broken = true;
  }
  return !is_broken;
}

/*! \relates Parma_Polyhedra_Library::Dense_Row */
bool
PPL::operator==(const Dense_Row& x, const Dense_Row& y) {
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
