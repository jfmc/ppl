/* Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_globals_types_hh
#define PPL_globals_types_hh 1

#include <cstddef>

namespace Parma_Polyhedra_Library {

//! An unsigned integral type for representing space dimensions.
/*! \ingroup PPL_CXX_interface */
typedef size_t dimension_type;

//! An unsigned integral type for representing memory size in bytes.
/*! \ingroup PPL_CXX_interface */
typedef size_t memory_size_type;

//! Kinds of degenerate abstract elements.
/*! \ingroup PPL_CXX_interface */
enum Degenerate_Element {
  //! The universe element, i.e., the whole vector space.
  UNIVERSE,
  //! The empty element, i.e., the empty set.
  EMPTY
};

//! Relation symbols.
/*! \ingroup PPL_CXX_interface */
enum Relation_Symbol {
  //! Less than.
  LESS_THAN,
  //! Less than or equal to.
  LESS_OR_EQUAL,
  //! Equal to.
  EQUAL,
  //! Greater than or equal to.
  GREATER_OR_EQUAL,
  //! Greater than.
  GREATER_THAN,
  //! Not equal to.
  NOT_EQUAL
};

//! Complexity pseudo-classes.
/*! \ingroup PPL_CXX_interface */
enum Complexity_Class {
  //! Worst-case polynomial complexity.
  POLYNOMIAL_COMPLEXITY,
  //! Worst-case exponential complexity but typically polynomial behavior.
  SIMPLEX_COMPLEXITY,
  //! Any complexity.
  ANY_COMPLEXITY
};

//! Possible optimization modes.
/*! \ingroup PPL_CXX_interface */
enum Optimization_Mode {
  //! Minimization is requested.
  MINIMIZATION,
  //! Maximization is requested.
  MAXIMIZATION
};

//! Widths of bounded integer types.
/*! \ingroup PPL_CXX_interface */
enum Bounded_Integer_Type_Width {
  //! \hideinitializer 8 bits.
  BITS_8 = 8,

  //! \hideinitializer 16 bits.
  BITS_16 = 16,

  //! \hideinitializer 32 bits.
  BITS_32 = 32,

  //! \hideinitializer 64 bits.
  BITS_64 = 64,

  //! \hideinitializer 128 bits.
  BITS_128 = 128,
};

//! Signedness of bounded integer types.
/*! \ingroup PPL_CXX_interface */
enum Bounded_Integer_Type_Signedness {
  //! Unsigned integer.
  UNSIGNED,

  //! Signed integer represented by the two's complement of the absolute value.
  SIGNED_2_COMPLEMENT
};

//! Overflow behavior of bounded integer types.
/*! \ingroup PPL_CXX_interface */
enum Bounded_Integer_Type_Overflow {
  /*! \brief
    On overflow, wrapping takes place.

    FIXME: formally specify what "wrapping takes place" means.
  */
  OVERFLOW_WRAPS,

  /*! \brief
    On overflow, the result is undefined.

    This simply means that the result of the operation resulting in an
    overflow can take any value.

    \note
    Even though something more serious can happen in the system
    being analyzed ---due to, e.g., C's undefined behavior---, here we
    are only concerned with the results of arithmetic operations.
    It is the responsibility of the analyzer to ensure that other
    manifestations of undefined behavior are conservatively approximated.
  */
  OVERFLOW_UNDEFINED,

  /*! \brief
    Overflow is impossible.

    This is for the analysis of languages where overflow is trapped
    before it affects the state, for which, thus, any indication that
    an overflow may have affected the state is necessarily due to
    the imprecision of the analysis.
  */
  OVERFLOW_IMPOSSIBLE
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_globals_types_hh)
