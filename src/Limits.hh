/* Limits for native integer types.
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

#ifndef PPL_Limits_hh
#define PPL_Limits_hh 1

#include <limits.h>

namespace Parma_Polyhedra_Library {

/*
  The only reason to use these definitions instead of std::numeric_limits
  is a missing optimization in gcc 3.4.1.
 */

template <typename T>
struct Limits;

#define signed_limits(type, prefix) \
template <> \
struct Limits<type> { \
	static const type min = prefix ## _MIN; \
	static const type max = prefix ## _MAX; \
}

#define unsigned_limits(type, prefix) \
template <> \
struct Limits<type> { \
	static const type min = 0; \
	static const type max = prefix ## _MAX; \
}

signed_limits(signed char, SCHAR);
signed_limits(short, SHRT);
signed_limits(int, INT);
signed_limits(long, LONG);
signed_limits(long long, LONG_LONG);

unsigned_limits(unsigned char, UCHAR);
unsigned_limits(unsigned short, USHRT);
unsigned_limits(unsigned int, UINT);
unsigned_limits(unsigned long, ULONG);
unsigned_limits(unsigned long long, ULONG_LONG);

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Limits_hh)
