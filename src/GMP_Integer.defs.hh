/* GMP_Integer class declaration.
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

#ifndef PPL_GMP_Integer_defs_hh
#define PPL_GMP_Integer_defs_hh 1

#include "GMP_Integer.types.hh"
#include <cstddef>

namespace Parma_Polyhedra_Library {

inline void negate(GMP_Integer& x);
inline void gcd_assign(GMP_Integer& x, const GMP_Integer& y);
inline void gcd_assign(GMP_Integer& x,
		       const GMP_Integer& y, const GMP_Integer& z);
inline void lcm_assign(GMP_Integer& x, const GMP_Integer& y);
inline void lcm_assign(GMP_Integer& x,
		       const GMP_Integer& y, const GMP_Integer& z);
inline void add_mul_assign(GMP_Integer& x,
			   const GMP_Integer& y, const GMP_Integer& z);
inline void sub_mul_assign(GMP_Integer& x,
			   const GMP_Integer& y, const GMP_Integer& z);
inline void exact_div_assign(GMP_Integer& x, const GMP_Integer& y);
inline void exact_div_assign(GMP_Integer& x,
			     const GMP_Integer& y, const GMP_Integer& z);
inline void sqrt_assign(GMP_Integer& x);
inline void sqrt_assign(GMP_Integer& x, const GMP_Integer& y);
inline int cmp(const GMP_Integer& x, const GMP_Integer& y);

inline const mpz_class& raw_value(const GMP_Integer& x);
inline mpz_class& raw_value(GMP_Integer& x);

//! Returns the total size in bytes of the memory occupied by \p x.
inline size_t total_memory_in_bytes(const GMP_Integer& x);

//! Returns the size in bytes of the memory managed by \p x.
inline size_t external_memory_in_bytes(const GMP_Integer& x);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::GMP_Integer& x,
	  Parma_Polyhedra_Library::GMP_Integer& y);

} // namespace std

#include "GMP_Integer.inlines.hh"

#endif // !defined(PPL_GMP_Integer_defs_hh)
