/* Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Integer_types_hh
#define PPL_Integer_types_hh 1

#include "Integer_traits_template.hh"

// Kludge
#include <stdint.h>

#ifdef NATIVE_INTEGERS
#include "Native_Integer.types.hh"
#endif

#ifdef CHECKED_INTEGERS
#include "Checked_Integer.types.hh"
#endif

#ifdef GMP_INTEGERS
#include "GMP_Integer.types.hh"
#endif

namespace Parma_Polyhedra_Library {

typedef COEFFICIENT_TYPE Integer;

typedef Integer_traits_template<Integer> Integer_traits;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Integer_types_hh)
