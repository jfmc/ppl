/* SatRow class implementation (non-inline functions).
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

#include "SatRow.defs.hh"

#include <iostream>
#include <cassert>
#include <climits>

namespace PPL = Parma_Polyhedra_Library;

#define BITS_PER_GMP_LIMB (SIZEOF_MP_LIMB_T*CHAR_BIT)

unsigned int
PPL::SatRow::first_one(mp_limb_t w) {
  unsigned int r = 0;
  w = w & -w;
#if SIZEOF_MP_LIMB_T > 4
  if ((w & 0xffffffff) == 0) {
    w >>= 32;
    r += 32;
  }
#endif
  if ((w & 0xffff) == 0) {
    w >>= 16;
    r += 16;
  }
  if ((w & 0xff) == 0) {
    w >>= 8;
    r += 8;
  }
  if (w & 0xf0)
    r += 4;
  if (w & 0xcc)
    r += 2;
  if (w & 0xaa)
    r += 1;
  return r;
}

unsigned int
PPL::SatRow::last_one(mp_limb_t w) {
  unsigned int r = 0;
#if SIZEOF_MP_LIMB_T > 4
  if (w & 0xffffffff00000000) {
    w >>= 32;
    r += 32;
  }
#endif
  if (w & 0xffff0000) {
    w >>= 16;
    r += 16;
  }
  if (w & 0xff00) {
    w >>= 8;
    r += 8;
  }
  if (w & 0xf0) {
    w >>= 4;
    r += 4;
  }
  if (w & 0xc) {
    w >>= 2;
    r += 2;
  }
  if (w & 0x2)
    r += 1;
  return r;
}

int
PPL::SatRow::first() const {
  for (size_t li = 0, vec_size = mpz_size(vec); li < vec_size; ++li) {
    const mp_limb_t limb = mpz_getlimbn(vec, li);
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + first_one(limb);
  }
  return -1;
}

int
PPL::SatRow::next(int position) const {
  assert(position >= 0);
  ++position;

  // The alternative implementation using the mpz_scan1() function
  // of GMP was measured to be slower that ours.  Here it is, in
  // case mpz_scan1() is improved.
  //
  // unsigned long r = mpz_scan1(vec, position);
  // return (r == ULONG_MAX) ? -1 : r;

  size_t li = position / BITS_PER_GMP_LIMB;
  const size_t vec_size = mpz_size(vec);
  if (li >= vec_size)
    return -1;

  // Get the first limb.
  mp_limb_t limb = mpz_getlimbn(vec, li);

  // Mask off any bits before `position' in the first limb.
  limb &= (-(mp_limb_t) 1) << (position % BITS_PER_GMP_LIMB);

  while (limb == 0 && ++li < vec_size)
    limb = mpz_getlimbn(vec, li);

  if (limb != 0)
    return li*BITS_PER_GMP_LIMB + first_one(limb);
  else
    return -1;
}

int
PPL::SatRow::last() const {
  for (size_t li = mpz_size(vec); li-- > 0; ) {
    const mp_limb_t limb = mpz_getlimbn(vec, li);
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + last_one(limb);
  }
  return -1;
}

int
PPL::SatRow::prev(int position) const {
  assert(position >= 0);

  if (position == 0)
    return -1;

  --position;

  size_t li = position / BITS_PER_GMP_LIMB;
  const size_t vec_size = mpz_size(vec);

  mp_limb_t limb;

  // Get the first limb.
  if (li >= vec_size) {
    li = vec_size - 1;
    limb = mpz_getlimbn(vec, li);
  }
  else {
    const mp_limb_t mask
      = (-(mp_limb_t) 1)
	>> (BITS_PER_GMP_LIMB - ((position + 1) % BITS_PER_GMP_LIMB));
    limb = mpz_getlimbn(vec, li) & mask;
  }

  do {
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + last_one(limb);
    if (li == 0)
      break;
    limb = mpz_getlimbn(vec, --li);
  } while (true);
  return -1;
}

/*! \relates Parma_Polyhedra_Library::SatRow */
int
PPL::compare(const SatRow& x, const SatRow& y) {
  const size_t x_size = mpz_size(x.vec);
  const size_t y_size = mpz_size(y.vec);
  size_t x_li = 0;
  size_t y_li = 0;
  while (x_li < x_size && y_li < y_size) {
    const mp_limb_t a = mpz_getlimbn(x.vec, x_li++);
    const mp_limb_t b = mpz_getlimbn(y.vec, y_li++);
    if (a != b) {
      // Get the one's where they are different.
      const mp_limb_t diff = (a ^ b);
      // First bit that is different.
      const mp_limb_t mask = diff & ~(diff-1);
      return (a & mask) ? 1 : -1;
    }
  }
  if (x_size < y_size) {
    while (y_li < y_size)
      if (mpz_getlimbn(y.vec, y_li++) != 0)
	return -1;
    return 0;
  }
  else if (x_size > y_size) {
    while (x_li < x_size)
      if (mpz_getlimbn(x.vec, x_li++) != 0)
	return 1;
    return 0;
  }
  return 0;
}

/*! \relates Parma_Polyhedra_Library::SatRow */
bool
PPL::subset_or_equal(const SatRow& x, const SatRow& y) {
  size_t x_size = mpz_size(x.vec);
  size_t y_size = mpz_size(y.vec);
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  if (x_size <= y_size) {
    while (x_size > 0) {
      if (*xp & ~*yp)
	return false;
      xp++;
      yp++;
      x_size--;
    }
  } else {
    x_size -= y_size;
    while (y_size > 0) {
      if (*xp++ & ~*yp)
	return false;
      yp++;
      y_size--;
    }
    while (x_size > 0) {
      if (*xp)
	return false;
      xp++;
      x_size--;
    }
  }   
  return true;
}

/*! \relates Parma_Polyhedra_Library::SatRow */
bool
PPL::strict_subset(const SatRow& x, const SatRow& y) {
  const size_t x_size = mpz_size(x.vec);
  const size_t y_size = mpz_size(y.vec);
  bool one_diff = false;
  size_t x_li = 0;
  size_t y_li = 0;
  while (x_li < x_size && y_li < y_size) {
    const mp_limb_t a = mpz_getlimbn(x.vec, x_li++);
    const mp_limb_t b = mpz_getlimbn(y.vec, y_li++);
    const mp_limb_t c = a | b;
    if (c != b)
      return false;
    else if (c != a)
      one_diff = true;
  }
  if (x_size < y_size) {
    if (one_diff)
      return true;
    while (y_li < y_size)
      if (mpz_getlimbn(y.vec, y_li++) != 0)
	return true;
    return false;
  }
  else if (x_size > y_size) {
    if (!one_diff)
      return false;
    while (x_li < x_size)
      if (mpz_getlimbn(x.vec, x_li++) != 0)
	return false;
    return true;
  }
  return one_diff;
}

bool
PPL::SatRow::OK() const {
  return true;
}
