/* Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Native_Integer_types_hh
#define PPL_Native_Integer_types_hh 1

namespace Parma_Polyhedra_Library {

template <typename T>
class Native_Integer;

//! Coefficient traits partial specialization for unchecked native integers.
template <typename T>
struct Coefficient_traits_template<Native_Integer<T> > {
  //! The type used for references to const native integers.
  typedef Native_Integer<T> const_reference;
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Native_Integer_types_hh)
