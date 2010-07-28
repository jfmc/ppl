/* Concrete_Expression class implementation: inline functions.
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

#ifndef PPL_Concrete_Expression_inlines_hh
#define PPL_Concrete_Expression_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Concrete_Expression_Type::Concrete_Expression_Type(Implementation i)
  : impl(i) {
}

inline Concrete_Expression_Type
Concrete_Expression_Type
::bounded_integer(const Bounded_Integer_Type_Width width,
                  const Bounded_Integer_Type_Representation representation,
                  const Bounded_Integer_Type_Overflow overflow) {
  Implementation impl;
  impl.bounded_integer = 1;
  impl.bounded_integer_type_width = width;
  impl.bounded_integer_type_representation = representation;
  impl.bounded_integer_type_overflow = overflow;
  // Arbitrary choice to ensure determinism.
  impl.floating_point_format = IEEE754_HALF;
  return Concrete_Expression_Type(impl);
}

inline Concrete_Expression_Type
Concrete_Expression_Type
::floating_point(const Floating_Point_Format format) {
  Implementation impl;
  impl.bounded_integer = 0;
  impl.floating_point_format = format;
  // Arbitrary choices to ensure determinism.
  impl.bounded_integer_type_width = BITS_128;
  impl.bounded_integer_type_representation =  SIGNED_2_COMPLEMENT;
  impl.bounded_integer_type_overflow = OVERFLOW_IMPOSSIBLE;
  return Concrete_Expression_Type(impl);
}

inline bool
Concrete_Expression_Type::is_bounded_integer() const {
  return impl.bounded_integer != 0;
}

inline bool
Concrete_Expression_Type::is_floating_point() const {
  return impl.bounded_integer == 0;
}

inline Bounded_Integer_Type_Width
Concrete_Expression_Type::bounded_integer_type_width() const {
  return impl.bounded_integer_type_width;
}

inline Bounded_Integer_Type_Representation
Concrete_Expression_Type::bounded_integer_type_representation() const {
  return impl.bounded_integer_type_representation;
}

inline Bounded_Integer_Type_Overflow
Concrete_Expression_Type::bounded_integer_type_overflow() const {
  return impl.bounded_integer_type_overflow;
}

inline Floating_Point_Format
Concrete_Expression_Type::floating_point_format() const {
  return impl.floating_point_format;
}

template <typename Target>
template <template <typename T> class Derived>
inline bool
Concrete_Expression_Common<Target>::is() const {
  return Concrete_Expression<Target>::kind() == Derived<Target>::KIND;
}

template <typename Target>
template <template <typename T> class Derived>
inline Derived<Target>*
Concrete_Expression_Common<Target>::as() {
  PPL_ASSERT(is<Derived>());
  return static_cast<Derived<Target>*>(this);
}

template <typename Target>
template <template <typename T> class Derived>
inline const Derived<Target>*
Concrete_Expression_Common<Target>::as() const {
  PPL_ASSERT(is<Derived>());
  return static_cast<const Derived<Target>*>(this);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Concrete_Expression_inlines_hh)
