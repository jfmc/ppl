/* Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Polyhedron_types_hh
#define PPL_Polyhedron_types_hh 1

//! The entire library is confined to this namespace.
namespace Parma_Polyhedra_Library {

class Polyhedron;

//! All input/output operators are confined to this namespace.
/*!
  This is done so that the library's input/output operators
  do not interfere with those the user might want to define.
  In fact, it is highly unlikely that any pre-defined I/O
  operator will suit the needs of a client application.
  On the other hand, those applications for which the PPL
  I/O operator are enough can easily obtain access to them.
  For example, a directive like
  \code
    using namespace Parma_Polyhedra_Library::IO_Operators;
  \endcode
  would suffice for most uses.
  In more complex situations, such as
  \code
    const Constraint_System& cs = ...;
    copy(cs.begin(), cs.end(),
	 ostream_iterator<Constraint>(cout, "\n"));
  \endcode
  the Parma_Polyhedra_Library namespace must be suitably extended.
  This can be done as follows:
  \code
    namespace Parma_Polyhedra_Library {
      // Import all the output operators into the main PPL namespace.
      using IO_Operators::operator<<;
    }
  \endcode
*/
namespace IO_Operators {
}

} // namespace Parma_Polyhedra_Library


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
