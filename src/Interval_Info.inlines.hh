/* Interval_Info class implementation: inline functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Interval_Info_inlines_hh
#define PPL_Interval_Info_inlines_hh 1

namespace Parma_Polyhedra_Library {

template <typename Policy>
inline void
Interval_Info_Null<Policy>::swap(Interval_Info_Null<Policy>& y) {
}

template <typename T, typename Policy>
inline void
Interval_Info_Bitset<T, Policy>::swap(Interval_Info_Bitset<T, Policy>& y) {
  std::swap(bitset, y.bitset);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Interval_Info */
template <typename Policy>
inline void
swap(Parma_Polyhedra_Library::Interval_Info_Null<Policy>& x,
     Parma_Polyhedra_Library::Interval_Info_Null<Policy>& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::Interval_Info */
template <typename T, typename Policy>
inline void
swap(Parma_Polyhedra_Library::Interval_Info_Bitset<T, Policy>& x,
     Parma_Polyhedra_Library::Interval_Info_Bitset<T, Policy>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Interval_Info_inlines_hh)
