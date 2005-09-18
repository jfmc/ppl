/* Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Checked_Number_types_hh
#define PPL_Checked_Number_types_hh 1

#include "Coefficient_traits_template.hh"

namespace Parma_Polyhedra_Library {

struct Checked_Number_Default_Policy;

template <typename T, typename Policy = Checked_Number_Default_Policy>
class Checked_Number;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits specialization for 8 bits checked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Checked_Number<int8_t> > {
  //! The type used for references to const 8 bit checked integers.
  typedef Checked_Number<int8_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits specialization for 16 bits checked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Checked_Number<int16_t> > {
  //! The type used for references to const 16 bit checked integers.
  typedef Checked_Number<int16_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits specialization for 32 bits checked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Checked_Number<int32_t> > {
  //! The type used for references to const 32 bit checked integers.
  typedef Checked_Number<int32_t> const_reference;
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Coefficient traits specialization for 64 bits checked integers.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <>
struct Coefficient_traits_template<Checked_Number<int64_t> > {
  //! The type used for references to const 64 bit checked integers.
  typedef const Checked_Number<int64_t>& const_reference;
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Checked_Number_types_hh)
