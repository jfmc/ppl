/* Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Polyhedron_types_hh
#define PPL_Polyhedron_types_hh 1

//! The entire library is confined into this namespace.
namespace Parma_Polyhedra_Library {

class Polyhedron;

//! All input output operators are confined into this namespace.
/*!
  This is done so that the library's input output operators
  do not to interfere with those the user might want to define.
*/
namespace IO_Operators {
}

}

//! The standard C++ namespace.
/*!
  The Parma Polyhedra Library conforms to the C++ standard and,
  in particular, as far as reserved names are concerned (17.4.3.1,
  [lib.reserved.names]).  The PPL, however, defines several
  template specializations for the standard library templates
  swap() and iter_swap() (25.2.2, [lib.alg.swap]).
*/
namespace std {
}

#endif // !defined(PPL_Polyhedron_types_hh)
