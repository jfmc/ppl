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

#if !defined(HAS_FFS) || SIZEOF_MP_LIMB_T != SIZEOF_INT
unsigned int
PPL::SatRow::first_one(mp_limb_t w) {
  unsigned int r = 0;
  w = w & -w;
#if SIZEOF_MP_LIMB_T == 8
  if ((w & 0xffffffff) == 0) {
    w >>= 32;
    r += 32;
  }
#elif SIZEOF_MP_LIMB_T != 4
#error "Size of mp_limb_t not supported by SatRow::first_one(mp_limb_t w)."
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
#endif // !defined(HAS_FFS) || SIZEOF_MP_LIMB_T != SIZEOF_INT

unsigned int
PPL::SatRow::last_one(mp_limb_t w) {
  unsigned int r = 0;
#if SIZEOF_MP_LIMB_T == 8
  if (w & 0xffffffff00000000) {
    w >>= 32;
    r += 32;
  }
#elif SIZEOF_MP_LIMB_T != 4
#error "Size of mp_limb_t not supported by SatRow::last_one(mp_limb_t w)."
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
  size_t li = 0;
  size_t vec_size = mpz_size(vec);
  mp_srcptr p = vec->_mp_d;
  for (; li < vec_size; ++li, ++p) {
    const mp_limb_t limb = *p;
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
  mp_srcptr p = vec->_mp_d + li;

  // Mask off any bits before `position' in the first limb.
  mp_limb_t limb = *p & (~(mp_limb_t) 0) << (position % BITS_PER_GMP_LIMB);

  while (true) {
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + first_one(limb);
    ++li;
    if (li == vec_size)
      break;
    ++p;
    limb = *p;
  }
  return -1;
}

int
PPL::SatRow::last() const {
  size_t li = mpz_size(vec);
  mp_srcptr p = vec->_mp_d + li;
  while (li > 0) {
    --li;
    --p;
    const mp_limb_t limb = *p;
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

  const size_t vec_size = mpz_size(vec);
  assert(vec_size > 0);
  size_t li = position / BITS_PER_GMP_LIMB;

  mp_limb_t limb;
  mp_srcptr p = vec->_mp_d;

  // Get the first limb.
  if (li >= vec_size) {
    li = vec_size - 1;
    p += li;
    limb = *p;
  }
  else {
    const mp_limb_t mask
      = (~(mp_limb_t) 0)
	>> (BITS_PER_GMP_LIMB - ((position + 1) % BITS_PER_GMP_LIMB));
    p += li;
    limb = *p & mask;
  }

  while (true) {
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + last_one(limb);
    if (li == 0)
      break;
    --li;
    --p;
    limb = *p;
  }
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
      ++xp;
      ++yp;
      --x_size;
    }
  }
  else {
    // x_size > y_size
    x_size -= y_size;
    while (y_size > 0) {
      if (*xp & ~*yp)
	return false;
      ++xp;
      ++yp;
      --y_size;
    }
    while (x_size > 0) {
      if (*xp)
	return false;
      ++xp;
      --x_size;
    }
  }   
  return true;
}

/*! \relates Parma_Polyhedra_Library::SatRow */
bool
PPL::strict_subset(const SatRow& x, const SatRow& y) {
  size_t x_size = mpz_size(x.vec);
  size_t y_size = mpz_size(y.vec);
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  bool different = false;
  if (x_size <= y_size) {
    y_size -= x_size;
    while (x_size > 0) {
      mp_limb_t xl = *xp;
      mp_limb_t yl = *yp;
      if (xl & ~yl)
	return false;
      if (!different && xl != yl)
	different = true;
      ++xp;
      ++yp;
      --x_size;
    }
    if (different)
      return true;
    while (y_size > 0) {
      if (*yp)
	return true;
      ++yp;
      --y_size;
    }
    return false;
  }
  else {
    x_size -= y_size;
    while (y_size > 0) {
      mp_limb_t xl = *xp;
      mp_limb_t yl = *yp;
      if (xl & ~yl)
	return false;
      if (!different && xl != yl)
	different = true;
      ++xp;
      ++yp;
      --y_size;
    }
    if (!different)
      return false;
    while (x_size > 0) {
      if (*xp)
	return false;
      ++xp;
      --x_size;
    }
    return true;
  }
}

bool
PPL::SatRow::OK() const {
  return true;
}
