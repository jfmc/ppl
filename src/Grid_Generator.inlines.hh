/* Grid Generator class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Grid_Generator_inlines_hh
#define PPL_Grid_Generator_inlines_hh 1

#include "Grid_Generator.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Grid_Generator::Grid_Generator(Generator g)
#if 0 // FIX
  : Generator(g,
	      g.type(),
	      g.topology() /* FIX */) {
#endif
  : Generator(g) {
}

inline Grid_Generator&
Grid_Generator::operator=(const Grid_Generator& g) {
  Generator::operator=(g);
  return *this;
}

inline Grid_Generator&
Grid_Generator::operator=(const Generator& g) {
  Generator::operator=(g);
  return *this;
}

/*! \relates Grid_Generator */
inline bool
operator==(const Grid_Generator& x, const Grid_Generator& y) {
  return x.is_equivalent_to(y);
}

/*! \relates Grid_Generator */
inline bool
operator!=(const Grid_Generator& x, const Grid_Generator& y) {
  return !(x == y);
}

inline Grid_Generator
Grid_Generator::line(const Linear_Expression& e) {
  // FIX creates a temp?
  return static_cast<Grid_Generator>(Generator::line(e));
}

inline Grid_Generator
Grid_Generator::parameter(const Linear_Expression& e) {
  // FIX creates a temp?
  return static_cast<Grid_Generator>(Generator::ray(e));
}

inline Grid_Generator
Grid_Generator::point(const Linear_Expression& e,
		      Coefficient_traits::const_reference d) {
  // FIX creates a temp?
  return static_cast<Grid_Generator>(Generator::point(e, d));
}

/*! \relates Grid_Generator */
inline Grid_Generator
grid_line(const Linear_Expression& e) {
  return Grid_Generator::line(e);
}

/*! \relates Grid_Generator */
inline Grid_Generator
parameter(const Linear_Expression& e) {
  return Grid_Generator::parameter(e);
}

/*! \relates Grid_Generator */
inline Grid_Generator
grid_point(const Linear_Expression& e, Coefficient_traits::const_reference d) {
  return Grid_Generator::point(e, d);
}

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Grid_Generator */
inline std::ostream&
operator<<(std::ostream& s, const Grid_Generator& g) {
  return operator<<(s, dynamic_cast<const Generator&>(g));
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Grid_Generator */
inline void
swap(Parma_Polyhedra_Library::Grid_Generator& x,
     Parma_Polyhedra_Library::Grid_Generator& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Grid_Generator_inlines_hh)
