/* Architecture-dependent, floating-point number types.
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

#ifndef PPL_float_types_hh
#define PPL_float_types_hh

#include <stdint.h>

#define FLOAT32_TYPE float
#define FLOAT64_TYPE double
#define FLOAT96_TYPE long double

#ifdef FLOAT32_TYPE
typedef FLOAT32_TYPE float32_t;
#endif

#ifdef FLOAT64_TYPE
typedef FLOAT64_TYPE float64_t;
#endif

#ifdef FLOAT96_TYPE
typedef FLOAT96_TYPE float96_t;
#endif

#ifdef FLOAT128_TYPE
typedef FLOAT128_TYPE float96_t;
#endif

#endif // !defined(PPL_float_types_hh)
