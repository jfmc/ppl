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

#include <climits>

// C99 defines LLONG_MIN, LLONG_MAX and ULLONG_MAX, but this part of
// C99 is not yet included into the C++ standard.
// GCC defines LONG_LONG_MIN, LONG_LONG_MAX and ULONG_LONG_MAX.
// Some compilers (such as Comeau C++ up to and including version 4.3.3)
// define nothing.  In this last case we make a reasonable guess.
#ifndef LLONG_MIN
#if defined(LONG_LONG_MIN)
#define LLONG_MIN LONG_LONG_MIN
#elif SIZEOF_LONG_LONG == 8
#define LLONG_MIN 0x8000000000000000LL
#endif
#endif

#ifndef LLONG_MAX
#if defined(LONG_LONG_MAX)
#define LLONG_MAX LONG_LONG_MAX
#elif SIZEOF_LONG_LONG == 8
#define LLONG_MAX 0x7fffffffffffffffLL
#endif
#endif


#ifndef ULLONG_MAX
#if defined(ULONG_LONG_MAX)
#define ULLONG_MAX ULONG_LONG_MAX
#elif SIZEOF_LONG_LONG == 8
#define ULLONG_MAX 0xffffffffffffffffULL
#endif
#endif


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
signed_limits(long long, LLONG);

unsigned_limits(unsigned char, UCHAR);
unsigned_limits(unsigned short, USHRT);
unsigned_limits(unsigned int, UINT);
unsigned_limits(unsigned long, ULONG);
unsigned_limits(unsigned long long, ULLONG);

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Limits_hh)
