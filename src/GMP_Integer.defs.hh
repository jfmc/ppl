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

//! \name Functions Operating on Unbounded Integer Coefficients
//@{

//! Assigns to \p x its negation.
inline void negate(GMP_Integer& x);

//! Assigns to \p x the greatest common divisor of \p x and \p y.
inline void gcd_assign(GMP_Integer& x, const GMP_Integer& y);
//! Assigns to \p x the greatest common divisor of \p y and \p z.
inline void gcd_assign(GMP_Integer& x,
		       const GMP_Integer& y, const GMP_Integer& z);

//! Extended GCD which assigns to one of the input parameters.
/*!
  Assigns to \p x the greatest common divisor of \p x and \p y, and to
  \p s and \p t the values such that \p x * \p s + \p y * \p t = \p x.
*/
inline void gcdext_assign(GMP_Integer& x, const GMP_Integer& y,
			  GMP_Integer& s, GMP_Integer& t);
//! Extended GCD.
/*!
  Assigns to \p x the greatest common divisor of \p y and \p z, and to
  \p s and \p t the values such that \p y * \p s + \p z * \p t = \p x.
*/
inline void gcdext_assign(GMP_Integer& x,
			  const GMP_Integer& y, const GMP_Integer& z,
			  GMP_Integer& s, GMP_Integer& t);

//! Assigns to \p x the least common multiple of \p x and \p y.
inline void lcm_assign(GMP_Integer& x, const GMP_Integer& y);
//! Assigns to \p x the least common multiple of \p y and \p z.
inline void lcm_assign(GMP_Integer& x,
		       const GMP_Integer& y, const GMP_Integer& z);

//! Assigns to \p x the value <CODE>x + y * z</CODE>.
inline void add_mul_assign(GMP_Integer& x,
			   const GMP_Integer& y, const GMP_Integer& z);
//! Assigns to \p x the value <CODE>x - y * z</CODE>.
inline void sub_mul_assign(GMP_Integer& x,
			   const GMP_Integer& y, const GMP_Integer& z);

//! Assigns to \p x the quotient of the integer division of \p x by \p y.
inline void exact_div_assign(GMP_Integer& x, const GMP_Integer& y);
//! Assigns to \p x the quotient of the integer division of \p y by \p z.
inline void exact_div_assign(GMP_Integer& x,
			     const GMP_Integer& y, const GMP_Integer& z);

//! Assigns to \p x its integer square root.
inline void sqrt_assign(GMP_Integer& x);
//! Assigns to \p x the integer square root of \p y.
inline void sqrt_assign(GMP_Integer& x, const GMP_Integer& y);

//! \brief
//! Returns a negative, zero or positive value depending on whether
//! \p x is lower than, equal to or greater than \p y, respectively.
inline int cmp(const GMP_Integer& x, const GMP_Integer& y);

//! Returns a const reference to \p x.
inline const mpz_class& raw_value(const GMP_Integer& x);
//! Returns a reference to \p x.
inline mpz_class& raw_value(GMP_Integer& x);

//! Returns the total size in bytes of the memory occupied by \p x.
inline size_t total_memory_in_bytes(const GMP_Integer& x);

//! Returns the size in bytes of the memory managed by \p x.
inline size_t external_memory_in_bytes(const GMP_Integer& x);

//@} // Functions Operating on Unbounded Integer Coefficients

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
void swap(Parma_Polyhedra_Library::GMP_Integer& x,
	  Parma_Polyhedra_Library::GMP_Integer& y);

} // namespace std

#include "GMP_Integer.inlines.hh"

#endif // !defined(PPL_GMP_Integer_defs_hh)
