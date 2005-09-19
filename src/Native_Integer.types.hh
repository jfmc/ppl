/* Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Native_Integer_types_hh
#define PPL_Native_Integer_types_hh 1

#include "Coefficient_traits_template.hh"

namespace Parma_Polyhedra_Library {

template <typename T>
class Native_Integer;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits partial specialization for 8 bits unchecked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Native_Integer<int8_t> > {
  //! The type used for references to const native integers.
  typedef Native_Integer<int8_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits partial specialization for 16 bits unchecked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Native_Integer<int16_t> > {
  //! The type used for references to const native integers.
  typedef Native_Integer<int16_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits partial specialization for 32 bits unchecked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Native_Integer<int32_t> > {
  //! The type used for references to const native integers.
  typedef Native_Integer<int32_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits partial specialization for 64 bits unchecked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Native_Integer<int64_t> > {
  //! The type used for references to const native integers.
  typedef const Native_Integer<int64_t>& const_reference;
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Native_Integer_types_hh)
