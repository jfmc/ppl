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
#ifdef NEW_CHECKED_CODE
#include "Checked_Number.types.hh"

namespace Parma_Polyhedra_Library {

template <>
struct Integer_traits_template<Checked_Number<int8_t> > {
  typedef Checked_Number<int8_t> const_reference;
};

template <>
struct Integer_traits_template<Checked_Number<int16_t> > {
  typedef Checked_Number<int16_t> const_reference;
};

template <>
struct Integer_traits_template<Checked_Number<int32_t> > {
  typedef Checked_Number<int32_t> const_reference;
};

template <>
struct Integer_traits_template<Checked_Number<int64_t> > {
  typedef const Checked_Number<int64_t>& const_reference;
};

} // namespace Parma_Polyhedra_Library

#else
#include "Checked_Integer.types.hh"

#endif
#endif

#ifdef GMP_INTEGERS
#include "GMP_Integer.types.hh"
#endif

namespace Parma_Polyhedra_Library {

typedef COEFFICIENT_TYPE Integer;

typedef Integer_traits_template<Integer> Integer_traits;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Integer_types_hh)
