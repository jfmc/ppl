/* Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_GMP_Integer_types_hh
#define PPL_GMP_Integer_types_hh 1

#include <gmpxx.h>

namespace Parma_Polyhedra_Library {

//! Unbounded integers are implemented using the GMP library.
/*!
  GMP_Integer is an alias for the <CODE>mpz_class</CODE> type
  defined in the C++ interface of the GMP library.
  For more information, see <CODE>http://www.swox.com/gmp/</CODE>
*/
typedef mpz_class GMP_Integer;

//! Traits for the unbounded integer coefficients.
template <>
struct Coefficient_traits_template<GMP_Integer> {
  //! The type used for references to const unbounded integers.
  typedef const GMP_Integer& const_reference;
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_GMP_Integer_types_hh)
