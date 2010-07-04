/* Constraint class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Constraint_inlines_hh
#define PPL_Constraint_inlines_hh 1

namespace Parma_Polyhedra_Library {

template <typename Target>
template <typename Underlying>
typename Underlying_To_Exposed<Target, Underlying>::Type*
Proxy<Target>::exposed(Underlying* ptr) {
  return reinterpret_cast<typename Underlying_To_Exposed<Target, Underlying>::Type*>(ptr);
}

#if 0
class Proxy {
public:
  template <typename Underlying>
  static const typename Underlying_To_Exposed<Target, Underlying>::Type* exposed(const Underlying* ptr) {
    return reinterpret_cast<const typename Underlying_To_Exposed<Target, Underlying>::Type*>(ptr);
  }
  template <typename Exposed>
  static typename Exposed_To_Underlying<Target, Exposed>::Type* underlying(Exposed* ptr) {
    return reinterpret_cast<typename Exposed_To_Underlying<Target, Exposed>::Type*>(ptr);
  }
  template <typename Exposed>
  static const typename Exposed_To_Underlying<Target, Exposed>::Type* underlying(const Exposed* ptr) {
    return reinterpret_cast<const typename Exposed_To_Underlying<Target, Exposed>::Type*>(ptr);
  }
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Constraint_inlines_hh)
