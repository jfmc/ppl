/* Linear_Row class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Linear_Row_inlines_hh
#define PPL_Linear_Row_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Linear_Row.defs.hh"

#include "globals.defs.hh"
#include "assert.hh"
#include "math_utilities.defs.hh"
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Linear_Row::Flags::Flags()
  : bits(0) {
  // Note that the constructed type has its validity bit unset.
}

inline
Linear_Row::Flags::Flags(const Topology t)
  : bits(t << nnc_bit) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
}

inline
Linear_Row::Flags::Flags(const Topology t, const Kind k)
  : bits((k << rpi_bit) | (t << nnc_bit)) {
#ifndef NDEBUG
  set_bits((1 << rpi_validity_bit)
	   | (1 << nnc_validity_bit));
#endif
}

inline bool
Linear_Row::Flags::is_ray_or_point_or_inequality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return test_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline void
Linear_Row::Flags::set_is_ray_or_point_or_inequality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  set_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Linear_Row::Flags::is_line_or_equality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return !is_ray_or_point_or_inequality();
}

inline void
Linear_Row::Flags::set_is_line_or_equality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  reset_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Linear_Row::Flags::is_not_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return test_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline bool
Linear_Row::Flags::is_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return !is_not_necessarily_closed();
}

inline void
Linear_Row::Flags::set_topology(Topology x) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  if (x == NOT_NECESSARILY_CLOSED)
    set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
  else
    reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Linear_Row::Flags::set_not_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Linear_Row::Flags::set_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline Topology
Linear_Row::Flags::topology() const {
  return is_necessarily_closed() ? NECESSARILY_CLOSED : NOT_NECESSARILY_CLOSED;
}

inline bool
Linear_Row::Flags::operator==(const Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Linear_Row::Flags::operator!=(const Flags& y) const {
  return !operator==(y);
}

inline Linear_Row::Flags::base_type
Linear_Row::Flags::get_bits() const {
  return bits;
}

inline void
Linear_Row::Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Linear_Row::Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Linear_Row::Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline const Linear_Row::Flags
Linear_Row::flags() const {
  return flags_;
}

inline void
Linear_Row::set_flags(Flags f) {
  flags_ = f;
}

inline bool
Linear_Row::is_necessarily_closed() const {
  return flags().is_necessarily_closed();
}

inline bool
Linear_Row::is_not_necessarily_closed() const {
  return flags().is_not_necessarily_closed();
}

inline dimension_type
Linear_Row::max_space_dimension() {
  // The first coefficient holds the inhomogeneous term or the divisor.
  // In NNC rows, the last coefficient is for the epsilon dimension.
  return Dense_Row::max_size() - 2;
}

inline dimension_type
Linear_Row::max_num_columns() {
  return Dense_Row::max_size();
}

inline dimension_type
Linear_Row::space_dimension() const {
  const dimension_type sz = get_row().size();
  return (sz == 0)
    ? 0
    : sz - (is_necessarily_closed() ? 1 : 2);
}

inline
Linear_Row::Linear_Row()
  : Linear_Expression() {
}

inline
Linear_Row::Linear_Row(const dimension_type sz, const dimension_type /* capacity */,
                       const Flags f)
  : Linear_Expression(), flags_(f) {
  get_row().resize(sz);
}

inline
Linear_Row::Linear_Row(const dimension_type sz, const Flags f)
  : Linear_Expression(), flags_(f) {
  get_row().resize(sz);
}

inline
Linear_Row::Linear_Row(const Linear_Row& y)
  : Linear_Expression(y), flags_(y.flags_) {
}

inline
Linear_Row::Linear_Row(const Linear_Row& y,
                       const dimension_type size)
  : Linear_Expression(y, size),
    flags_(y.flags_) {
}

inline
Linear_Row::Linear_Row(const Linear_Row& y,
		       const dimension_type sz, const dimension_type /* capacity */)
  : Linear_Expression(y, sz),
    flags_(y.flags_) {
}

inline
Linear_Row::~Linear_Row() {
}

inline void
Linear_Row::swap(Linear_Row& y) {
  Linear_Expression::swap(y);
  std::swap(flags_, y.flags_);
}

inline void
Linear_Row::swap(dimension_type i, dimension_type j) {
  get_row().swap(i, j);
}

inline bool
Linear_Row::is_line_or_equality() const {
  return flags().is_line_or_equality();
}

inline bool
Linear_Row::is_ray_or_point_or_inequality() const {
  return flags().is_ray_or_point_or_inequality();
}

inline Topology
Linear_Row::topology() const {
  return flags().topology();
}

inline void
Linear_Row::set_is_line_or_equality() {
  flags_.set_is_line_or_equality();
}

inline void
Linear_Row::set_is_ray_or_point_or_inequality() {
  flags_.set_is_ray_or_point_or_inequality();
}

inline void
Linear_Row::set_topology(Topology x) {
  if (topology() == x)
    return;
  if (topology() == NECESSARILY_CLOSED)
    // Add a column for the epsilon dimension.
    get_row().resize(get_row().size() + 1);
  else {
    PPL_ASSERT(get_row().size() > 0);
    get_row().resize(get_row().size() - 1);
  }
  flags_.set_topology(x);
}

inline void
Linear_Row::mark_as_necessarily_closed() {
  PPL_ASSERT(is_not_necessarily_closed());
  flags_.set_topology(NECESSARILY_CLOSED);
}

inline void
Linear_Row::mark_as_not_necessarily_closed() {
  PPL_ASSERT(is_necessarily_closed());
  flags_.set_topology(NOT_NECESSARILY_CLOSED);
}

inline void
Linear_Row::set_necessarily_closed() {
  set_topology(NECESSARILY_CLOSED);
}

inline void
Linear_Row::set_not_necessarily_closed() {
  set_topology(NOT_NECESSARILY_CLOSED);
}

inline Coefficient_traits::const_reference
Linear_Row::inhomogeneous_term() const {
  return get_row()[0];
}

inline Coefficient_traits::const_reference
Linear_Row::coefficient(const dimension_type k) const {
  return get_row()[k+1];
}

inline void
Linear_Row::strong_normalize() {
  get_row().normalize();
  sign_normalize();
}

/*! \relates Linear_Row */
inline bool
operator==(const Linear_Row& x, const Linear_Row& y) {
  return x.flags() == y.flags() && x.get_row() == y.get_row();
}

/*! \relates Linear_Row */
inline bool
operator!=(const Linear_Row& x, const Linear_Row& y) {
  return !(x == y);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Linear_Row */
inline void
swap(Parma_Polyhedra_Library::Linear_Row& x,
     Parma_Polyhedra_Library::Linear_Row& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Linear_Row_inlines_hh)
