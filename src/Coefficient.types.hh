/* Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Coefficient_types_hh
#define PPL_Coefficient_types_hh 1

#include "Coefficient_traits_template.hh"

#ifdef NATIVE_INTEGERS
#include "Native_Integer.types.hh"
#endif

#ifdef CHECKED_INTEGERS
#include "Checked_Number.types.hh"

namespace Parma_Polyhedra_Library {

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
#endif

#ifdef GMP_INTEGERS
#include "GMP_Integer.types.hh"
#endif

namespace Parma_Polyhedra_Library {

//! An alias for easily naming the type of PPL coefficients.
/*!
  Objects of type Coefficient are used to implement the integral valued
  coefficients occurring in linear expressions, constraints, generators,
  intervals, bounding boxes and so on. Depending on the chosen
  configuration options (see file <CODE>README.configure</CODE>),
  a Coefficient may actually be:
    - The GMP_Integer type, which in turn is an alias for the
      <CODE>mpz_class</CODE> type implemented by the C++ interface
      of the GMP library (this is the default configuration);
    - An instance of the Checked_Number class template, implementing
      overflow detection on top of a native integral type
      (available template instances include checked integers having
      8, 16, 32 or 64 bits);
    - An instance of the Native_Integer class template, simply wrapping
      a native integral types with no overflow detection
      (available template instances include native integers having
      8, 16, 32 or 64 bits).
*/
typedef COEFFICIENT_TYPE Coefficient;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! An alias for easily naming the coefficient traits.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
typedef Coefficient_traits_template<Coefficient> Coefficient_traits;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Coefficient_types_hh)
