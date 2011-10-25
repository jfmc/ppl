/* Row_Flags class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#ifndef PPL_Row_Flags_inlines_hh
#define PPL_Row_Flags_inlines_hh 1

#include "math_utilities.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Row_Flags::Row_Flags()
  : bits(0) {
}

inline
Row_Flags::Row_Flags(base_type n)
  : bits(n) {
}

inline Row_Flags::base_type
Row_Flags::get_bits() const {
  return bits;
}

inline void
Row_Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Row_Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Row_Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline bool
Row_Flags::operator==(const Row_Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Row_Flags::operator!=(const Row_Flags& y) const {
  return !operator==(y);
}

} // namespace Parma_Polyhedra_Library


#endif // !defined(PPL_Row_Flags_inlines_hh)
