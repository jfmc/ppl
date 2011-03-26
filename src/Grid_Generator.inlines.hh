/* Grid Generator class implementation: inline functions.
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

#ifndef PPL_Grid_Generator_inlines_hh
#define PPL_Grid_Generator_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Grid_Generator.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Grid_Generator::Flags::Flags()
  : bits(0) {
  // Note that the constructed type has its validity bit unset.
}

inline
Grid_Generator::Flags::Flags(const Topology t)
  : bits(t << nnc_bit) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
}

inline
Grid_Generator::Flags::Flags(const Topology t, const Kind k)
  : bits((k << rpi_bit) | (t << nnc_bit)) {
#ifndef NDEBUG
  set_bits((1 << rpi_validity_bit)
           | (1 << nnc_validity_bit));
#endif
}

inline bool
Grid_Generator::Flags::is_ray_or_point_or_inequality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return test_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline void
Grid_Generator::Flags::set_is_ray_or_point_or_inequality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  set_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Grid_Generator::Flags::is_line_or_equality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return !is_ray_or_point_or_inequality();
}

inline void
Grid_Generator::Flags::set_is_line_or_equality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  reset_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Grid_Generator::Flags::is_not_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return test_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline bool
Grid_Generator::Flags::is_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return !is_not_necessarily_closed();
}

inline void
Grid_Generator::Flags::set_topology(Topology x) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  if (x == NOT_NECESSARILY_CLOSED)
    set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
  else
    reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Grid_Generator::Flags::set_not_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Grid_Generator::Flags::set_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline Topology
Grid_Generator::Flags::topology() const {
  return is_necessarily_closed() ? NECESSARILY_CLOSED : NOT_NECESSARILY_CLOSED;
}

inline bool
Grid_Generator::Flags::operator==(const Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Grid_Generator::Flags::operator!=(const Flags& y) const {
  return !operator==(y);
}

inline Grid_Generator::Flags::base_type
Grid_Generator::Flags::get_bits() const {
  return bits;
}

inline void
Grid_Generator::Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Grid_Generator::Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Grid_Generator::Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline const Grid_Generator::Flags
Grid_Generator::flags() const {
  return flags_;
}

inline void
Grid_Generator::set_flags(Flags f) {
  flags_ = f;
}

inline bool
Grid_Generator::is_necessarily_closed() const {
  return flags().is_necessarily_closed();
}

inline bool
Grid_Generator::is_not_necessarily_closed() const {
  return flags().is_not_necessarily_closed();
}

inline bool
Grid_Generator::is_line_or_equality() const {
  return flags().is_line_or_equality();
}

inline bool
Grid_Generator::is_ray_or_point_or_inequality() const {
  return flags().is_ray_or_point_or_inequality();
}

inline Topology
Grid_Generator::topology() const {
  return flags().topology();
}

inline void
Grid_Generator::set_is_line_or_equality() {
  flags_.set_is_line_or_equality();
}

inline void
Grid_Generator::set_is_ray_or_point_or_inequality() {
  flags_.set_is_ray_or_point_or_inequality();
}

inline void
Grid_Generator::set_topology(Topology x) {
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
Grid_Generator::mark_as_necessarily_closed() {
  PPL_ASSERT(is_not_necessarily_closed());
  flags_.set_topology(NECESSARILY_CLOSED);
}

inline void
Grid_Generator::mark_as_not_necessarily_closed() {
  PPL_ASSERT(is_necessarily_closed());
  flags_.set_topology(NOT_NECESSARILY_CLOSED);
}

inline void
Grid_Generator::set_necessarily_closed() {
  set_topology(NECESSARILY_CLOSED);
}

inline void
Grid_Generator::set_not_necessarily_closed() {
  set_topology(NOT_NECESSARILY_CLOSED);
}

inline
Grid_Generator::Grid_Generator(Linear_Expression& e, Type type) {
  get_row().swap(e.get_row());
  set_flags(Flags(NECESSARILY_CLOSED, (type == LINE
                                       ? LINE_OR_EQUALITY
                                       : RAY_OR_POINT_OR_INEQUALITY)));
  PPL_ASSERT(OK());
}

inline
Grid_Generator::Grid_Generator()
  : Linear_Row() {
  set_flags(Flags(NECESSARILY_CLOSED, LINE_OR_EQUALITY));
}

inline
Grid_Generator::Grid_Generator(const Grid_Generator& g)
  : Linear_Row(g), flags_(g.flags_) {
}

inline
Grid_Generator::Grid_Generator(dimension_type size, Flags f)
  : Linear_Row(size), flags_(f) {
}

inline
Grid_Generator::Grid_Generator(const Grid_Generator& g, dimension_type size,
                               dimension_type capacity)
  : Linear_Row(g, size, capacity), flags_(g.flags_) {
}

inline void
Grid_Generator::swap(dimension_type i, dimension_type j) {
  Linear_Row::swap(i, j);
}

inline
Grid_Generator::~Grid_Generator() {
}

inline dimension_type
Grid_Generator::max_space_dimension() {
  return Linear_Row::max_space_dimension() - 1;
}

inline dimension_type
Grid_Generator::space_dimension() const {
  return Linear_Expression::space_dimension() - 1;
}

inline void
Grid_Generator::set_space_dimension(dimension_type space_dim) {
  const dimension_type old_space_dim = space_dimension();
  if (space_dim > old_space_dim) {
    get_row().resize(space_dim + 2);
    Linear_Row::swap(space_dim + 1, old_space_dim + 1);
  } else {
    Linear_Row::swap(space_dim + 1, old_space_dim + 1);
    get_row().resize(space_dim + 2);
  }
  PPL_ASSERT(space_dimension() == space_dim);
}

inline Grid_Generator::Type
Grid_Generator::type() const {
  if (is_line())
    return LINE;
  return is_point() ? POINT : PARAMETER;
}

inline bool
Grid_Generator::is_line() const {
  return is_line_or_equality();
}

inline bool
Grid_Generator::is_parameter() const {
  return is_parameter_or_point() && is_line_or_parameter();
}

inline bool
Grid_Generator::is_line_or_parameter() const {
  return get_row()[0] == 0;
}

inline bool
Grid_Generator::is_point() const {
  return !is_line_or_parameter();
}

inline bool
Grid_Generator::is_parameter_or_point() const {
  return is_ray_or_point_or_inequality();
}

inline void
Grid_Generator::set_divisor(Coefficient_traits::const_reference d) {
  PPL_ASSERT(!is_line());
  Linear_Row& x = *this;
  if (is_line_or_parameter())
    x.get_row()[get_row().size() - 1] = d;
  else
    x.get_row()[0] = d;
}

inline Coefficient_traits::const_reference
Grid_Generator::divisor() const {
  if (is_line())
    throw_invalid_argument("divisor()", "*this is a line");
  const Linear_Row& x = *this;
  if (is_line_or_parameter())
    return x.get_row()[get_row().size() - 1];
  else
    return x.get_row()[0];
}

inline bool
Grid_Generator::is_equal_at_dimension(dimension_type dim,
				      const Grid_Generator& y) const {
  const Grid_Generator& x = *this;
  return x.get_row()[dim] * y.divisor() == y.get_row()[dim] * x.divisor();
}

inline void
Grid_Generator::set_is_line() {
  set_is_line_or_equality();
}

inline void
Grid_Generator::set_is_parameter_or_point() {
  set_is_ray_or_point_or_inequality();
}

inline Grid_Generator&
Grid_Generator::operator=(const Grid_Generator& g) {
  Linear_Row::operator=(g);
  flags_ = g.flags_;
  return *this;
}

inline void
Grid_Generator::negate(dimension_type first, dimension_type last) {
  Linear_Row& x = *this;
  for ( ; first < last; ++first)
    neg_assign(x.get_row()[first]);
}

inline Coefficient_traits::const_reference
Grid_Generator::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return Linear_Row::coefficient(v.id());
}

inline memory_size_type
Grid_Generator::total_memory_in_bytes() const {
  return Linear_Row::total_memory_in_bytes();
}

inline memory_size_type
Grid_Generator::external_memory_in_bytes() const {
  return Linear_Row::external_memory_in_bytes();
}

inline const Grid_Generator&
Grid_Generator::zero_dim_point() {
  PPL_ASSERT(zero_dim_point_p != 0);
  return *zero_dim_point_p;
}

inline void
Grid_Generator::strong_normalize() {
  PPL_ASSERT(!is_parameter());
  get_row().normalize();
  sign_normalize();
}

inline void
Grid_Generator::swap(Grid_Generator& y) {
  Linear_Row::swap(y);
  std::swap(flags_, y.flags_);
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

/*! \relates Grid_Generator */
inline Grid_Generator
grid_line(const Linear_Expression& e) {
  return Grid_Generator::grid_line(e);
}

/*! \relates Grid_Generator */
inline Grid_Generator
parameter(const Linear_Expression& e,
	  Coefficient_traits::const_reference d) {
  return Grid_Generator::parameter(e, d);
}

/*! \relates Grid_Generator */
inline Grid_Generator
grid_point(const Linear_Expression& e,
	   Coefficient_traits::const_reference d) {
  return Grid_Generator::grid_point(e, d);
}

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
