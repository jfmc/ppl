/* Saturation_Row class implementation (non-inline functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "Saturation_Row.defs.hh"
#include <cassert>
#include <climits>

namespace PPL = Parma_Polyhedra_Library;

#define BITS_PER_GMP_LIMB (SIZEOF_MP_LIMB_T*CHAR_BIT)

#if !HAVE_DECL_FFS || SIZEOF_MP_LIMB_T != SIZEOF_INT
unsigned int
PPL::Saturation_Row::first_one(mp_limb_t w) {
  unsigned int r = 0;
  w = w & -w;
#if SIZEOF_MP_LIMB_T == 8
  if ((w & 0xffffffff) == 0) {
    w >>= 32;
    r += 32;
  }
#elif SIZEOF_MP_LIMB_T != 4
#error "Size of mp_limb_t not supported by Saturation_Row::first_one(mp_limb_t w)."
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
#endif // !HAVE_DECL_FFS || SIZEOF_MP_LIMB_T != SIZEOF_INT

unsigned int
PPL::Saturation_Row::last_one(mp_limb_t w) {
  unsigned int r = 0;
#if SIZEOF_MP_LIMB_T == 8
  if (w & 0xffffffff00000000) {
    w >>= 32;
    r += 32;
  }
#elif SIZEOF_MP_LIMB_T != 4
#error "Size of mp_limb_t not supported by Saturation_Row::last_one(mp_limb_t w)."
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

unsigned long
PPL::Saturation_Row::first() const {
  const mp_size_t vec_size = vec->_mp_size;
  assert(vec_size >= 0);
  mp_size_t li = 0;
  mp_srcptr p = vec->_mp_d;
  for (; li < vec_size; ++li, ++p) {
    const mp_limb_t limb = *p;
    if (limb != 0)
      return li*BITS_PER_GMP_LIMB + first_one(limb);
  }
  return ULONG_MAX;
}

unsigned long
PPL::Saturation_Row::next(unsigned long position) const {
  ++position;

  // The alternative implementation using the mpz_scan1() function
  // of GMP was measured to be slower that ours.  Here it is, in
  // case mpz_scan1() is improved.
  //
  // unsigned long r = mpz_scan1(vec, position);
  // return (r == ULONG_MAX) ? -1 : r;

  mp_size_t li = position / BITS_PER_GMP_LIMB;
  const mp_size_t vec_size = vec->_mp_size;
  assert(vec_size >= 0);
  if (li >= vec_size)
    return ULONG_MAX;

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
  return ULONG_MAX;
}

unsigned long
PPL::Saturation_Row::last() const {
  mp_size_t li = vec->_mp_size;
  assert(li >= 0);
  if (li == 0)
    return ULONG_MAX;
  --li;
  const mp_srcptr p = vec->_mp_d + li;
  const mp_limb_t limb = *p;
  assert(limb != 0);
  return li*BITS_PER_GMP_LIMB + last_one(limb);
}

unsigned long
PPL::Saturation_Row::prev(unsigned long position) const {
  if (position == 0)
    return ULONG_MAX;

  --position;

  const mp_size_t vec_size = vec->_mp_size;
  assert(vec_size > 0);
  mp_size_t li = position / BITS_PER_GMP_LIMB;

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
  return ULONG_MAX;
}

bool
PPL::Saturation_Row::operator[](const unsigned long k) const {
  const mp_size_t vec_size = vec->_mp_size;
  assert(vec_size >= 0);

  unsigned long i = k / GMP_NUMB_BITS;
  if (i >= static_cast<unsigned long>(vec_size))
    return false;

  mp_limb_t limb = *(vec->_mp_d + i);
  return (limb >> (k % GMP_NUMB_BITS)) & 1;
}

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
int
PPL::compare(const Saturation_Row& x, const Saturation_Row& y) {
  const mp_size_t x_size = x.vec->_mp_size;
  assert(x_size >= 0);
  const mp_size_t y_size = y.vec->_mp_size;
  assert(y_size >= 0);
  mp_size_t size = (x_size > y_size ? y_size : x_size);
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  while (size > 0) {
    const mp_limb_t xl = *xp;
    const mp_limb_t yl = *yp;
    if (xl != yl) {
      // Get the one's where they are different.
      const mp_limb_t diff = xl ^ yl;
      // First bit that is different.
      const mp_limb_t mask = diff & ~(diff-1);
      return (xl & mask) ? 1 : -1;
    }
    ++xp;
    ++yp;
    --size;
  }
  return x_size == y_size ? 0 : (x_size > y_size ? 1 : -1);
}

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
bool
PPL::subset_or_equal(const Saturation_Row& x, const Saturation_Row& y) {
  mp_size_t x_size = x.vec->_mp_size;
  assert(x_size >= 0);
  mp_size_t y_size = y.vec->_mp_size;
  assert(y_size >= 0);
  if (x_size > y_size)
    return false;
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  while (x_size > 0) {
    if (*xp & ~*yp)
      return false;
    ++xp;
    ++yp;
    --x_size;
  }
  return true;
}

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
bool
PPL::subset_or_equal(const Saturation_Row& x, const Saturation_Row& y,
		     bool& strict_subset) {
  mp_size_t x_size = x.vec->_mp_size;
  assert(x_size >= 0);
  mp_size_t y_size = y.vec->_mp_size;
  assert(y_size >= 0);
  if (x_size > y_size)
    return false;
  strict_subset = (x_size < y_size);
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  while (x_size > 0) {
    const mp_limb_t xl = *xp;
    const mp_limb_t yl = *yp;
    if (xl & ~yl)
      return false;
    if (!strict_subset && xl != yl)
      strict_subset = true;
    ++xp;
    ++yp;
    --x_size;
  }
  return true;
}

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
bool
PPL::strict_subset(const Saturation_Row& x, const Saturation_Row& y) {
  mp_size_t x_size = x.vec->_mp_size;
  assert(x_size >= 0);
  mp_size_t y_size = y.vec->_mp_size;
  assert(y_size >= 0);
  if (x_size > y_size)
    return false;
  bool different = (x_size < y_size);
  mp_srcptr xp = x.vec->_mp_d;
  mp_srcptr yp = y.vec->_mp_d;
  while (x_size > 0) {
    const mp_limb_t xl = *xp;
    const mp_limb_t yl = *yp;
    if (xl & ~yl)
      return false;
    if (!different && xl != yl)
      different = true;
    ++xp;
    ++yp;
    --x_size;
  }
  return different;
}

/*! \relates Saturation_Row */
bool
PPL::operator==(const Saturation_Row& x, const Saturation_Row& y) {
  const mp_size_t x_vec_size = x.vec->_mp_size;
  assert(x_vec_size >= 0);
  const mp_size_t y_vec_size = y.vec->_mp_size;
  assert(y_vec_size >= 0);

  if (x_vec_size != y_vec_size)
    return false;

  return mpn_cmp(x.vec->_mp_d, y.vec->_mp_d, x_vec_size) == 0;
}

/*! \relates Saturation_Row */
bool
PPL::operator!=(const Saturation_Row& x, const Saturation_Row& y) {
  const mp_size_t x_vec_size = x.vec->_mp_size;
  assert(x_vec_size >= 0);
  const mp_size_t y_vec_size = y.vec->_mp_size;
  assert(y_vec_size >= 0);

  if (x_vec_size != y_vec_size)
    return true;

  return mpn_cmp(x.vec->_mp_d, y.vec->_mp_d, x_vec_size) != 0;
}

bool
PPL::Saturation_Row::OK() const {
  // FIXME: this must be completed.
  const mp_size_t vec_size = vec->_mp_size;
  return vec_size >= 0
    && (vec_size == 0 || mpz_getlimbn(vec, vec_size-1) != 0);
}
