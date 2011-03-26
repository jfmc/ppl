/* Constraint class implementation: inline functions.
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

#ifndef PPL_Constraint_inlines_hh
#define PPL_Constraint_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Constraint.defs.hh"

#include "Linear_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

inline
Constraint::Flags::Flags()
  : bits(0) {
  // Note that the constructed type has its validity bit unset.
}

inline
Constraint::Flags::Flags(const Topology t)
  : bits(t << nnc_bit) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
}

inline
Constraint::Flags::Flags(const Topology t, const Kind k)
  : bits((k << rpi_bit) | (t << nnc_bit)) {
#ifndef NDEBUG
  set_bits((1 << rpi_validity_bit)
           | (1 << nnc_validity_bit));
#endif
}

inline bool
Constraint::Flags::is_ray_or_point_or_inequality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return test_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline void
Constraint::Flags::set_is_ray_or_point_or_inequality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  set_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Constraint::Flags::is_line_or_equality() const {
  PPL_ASSERT(test_bits(1 << rpi_validity_bit));
  return !is_ray_or_point_or_inequality();
}

inline void
Constraint::Flags::set_is_line_or_equality() {
#ifndef NDEBUG
  set_bits(1 << rpi_validity_bit);
#endif
  reset_bits(RAY_OR_POINT_OR_INEQUALITY << rpi_bit);
}

inline bool
Constraint::Flags::is_not_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return test_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline bool
Constraint::Flags::is_necessarily_closed() const {
  PPL_ASSERT(test_bits(1 << nnc_validity_bit));
  return !is_not_necessarily_closed();
}

inline void
Constraint::Flags::set_topology(Topology x) {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  if (x == NOT_NECESSARILY_CLOSED)
    set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
  else
    reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Constraint::Flags::set_not_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  set_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline void
Constraint::Flags::set_necessarily_closed() {
#ifndef NDEBUG
  set_bits(1 << nnc_validity_bit);
#endif
  reset_bits(NOT_NECESSARILY_CLOSED << nnc_bit);
}

inline Topology
Constraint::Flags::topology() const {
  return is_necessarily_closed() ? NECESSARILY_CLOSED : NOT_NECESSARILY_CLOSED;
}

inline bool
Constraint::Flags::operator==(const Flags& y) const {
  base_type mask = low_bits_mask<base_type>(first_free_bit);
  return (get_bits() & mask) == (y.get_bits() & mask);
}

inline bool
Constraint::Flags::operator!=(const Flags& y) const {
  return !operator==(y);
}

inline Constraint::Flags::base_type
Constraint::Flags::get_bits() const {
  return bits;
}

inline void
Constraint::Flags::set_bits(const base_type mask) {
  bits |= mask;
}

inline void
Constraint::Flags::reset_bits(const base_type mask) {
  bits &= ~mask;
}

inline bool
Constraint::Flags::test_bits(const base_type mask) const {
  return (bits & mask) == mask;
}

inline const Constraint::Flags
Constraint::flags() const {
  return flags_;
}

inline void
Constraint::set_flags(Flags f) {
  flags_ = f;
}

inline bool
Constraint::is_necessarily_closed() const {
  return flags().is_necessarily_closed();
}

inline bool
Constraint::is_not_necessarily_closed() const {
  return flags().is_not_necessarily_closed();
}

inline dimension_type
Constraint::space_dimension() const {
  const dimension_type sz = get_row().size();
  return (sz == 0)
    ? 0
    : sz - (is_necessarily_closed() ? 1 : 2);
}

inline bool
Constraint::is_line_or_equality() const {
  return flags().is_line_or_equality();
}

inline bool
Constraint::is_ray_or_point_or_inequality() const {
  return flags().is_ray_or_point_or_inequality();
}

inline Topology
Constraint::topology() const {
  return flags().topology();
}

inline void
Constraint::set_is_line_or_equality() {
  flags_.set_is_line_or_equality();
}

inline void
Constraint::set_is_ray_or_point_or_inequality() {
  flags_.set_is_ray_or_point_or_inequality();
}

inline void
Constraint::set_topology(Topology x) {
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
Constraint::mark_as_necessarily_closed() {
  PPL_ASSERT(is_not_necessarily_closed());
  flags_.set_topology(NECESSARILY_CLOSED);
}

inline void
Constraint::mark_as_not_necessarily_closed() {
  PPL_ASSERT(is_necessarily_closed());
  flags_.set_topology(NOT_NECESSARILY_CLOSED);
}

inline void
Constraint::set_necessarily_closed() {
  set_topology(NECESSARILY_CLOSED);
}

inline void
Constraint::set_not_necessarily_closed() {
  set_topology(NOT_NECESSARILY_CLOSED);
}

inline
Constraint::Constraint(dimension_type sz, Flags flags)
  : Linear_Row(sz), flags_(flags) {
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(dimension_type sz, dimension_type capacity, Flags flags)
  : Linear_Row(sz, capacity), flags_(flags) {
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(Linear_Expression& e, Type type, Topology topology) {
  PPL_ASSERT(type != STRICT_INEQUALITY || topology == NOT_NECESSARILY_CLOSED);
  get_row().swap(e.get_row());
  set_flags(Flags(topology, (type == EQUALITY
                            ? LINE_OR_EQUALITY
                            : RAY_OR_POINT_OR_INEQUALITY)));
  strong_normalize();
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(const Constraint& c)
  : Linear_Row(c), flags_(c.flags_) {
  // NOTE: This does not call PPL_ASSERT(OK()) because this is called by OK().
}

inline
Constraint::Constraint(const Constraint& c, const dimension_type sz)
  : Linear_Row(c, sz, sz), flags_(c.flags_) {
  PPL_ASSERT(OK());
}

inline
Constraint::Constraint(const Constraint& c, const dimension_type sz,
                       const dimension_type capacity)
  : Linear_Row(c, sz, capacity), flags_(c.flags_) {
  PPL_ASSERT(OK());
}

inline
Constraint::~Constraint() {
}

inline Constraint&
Constraint::operator=(const Constraint& c) {
  static_cast<Linear_Row&>(*this) = static_cast<const Linear_Row&>(c);
  flags_ = c.flags_;
  PPL_ASSERT(OK());
  
  return *this;
}

inline dimension_type
Constraint::max_space_dimension() {
  return Linear_Row::max_space_dimension();
}

inline void
Constraint::set_space_dimension(dimension_type space_dim) {
  if (topology() == NECESSARILY_CLOSED) {
    get_row().resize(space_dim + 1);
  } else {
    const dimension_type old_space_dim = space_dimension();
    if (space_dim > old_space_dim) {
      get_row().resize(space_dim + 2);
      Linear_Row::swap(space_dim + 1, old_space_dim + 1);
    } else {
      Linear_Row::swap(space_dim + 1, old_space_dim + 1);
      get_row().resize(space_dim + 2);
    }
  }
  PPL_ASSERT(space_dimension() == space_dim);
}

inline bool
Constraint::is_equality() const {
  return is_line_or_equality();
}

inline bool
Constraint::is_inequality() const {
  return is_ray_or_point_or_inequality();
}

inline Constraint::Type
Constraint::type() const {
  if (is_equality())
    return EQUALITY;
  if (is_necessarily_closed())
    return NONSTRICT_INEQUALITY;
  else
    return (get_row()[get_row().size() - 1] < 0)
      ? STRICT_INEQUALITY
      : NONSTRICT_INEQUALITY;
}

inline bool
Constraint::is_nonstrict_inequality() const {
  return type() == NONSTRICT_INEQUALITY;
}

inline bool
Constraint::is_strict_inequality() const {
  return type() == STRICT_INEQUALITY;
}

inline void
Constraint::set_is_equality() {
  set_is_line_or_equality();
}

inline void
Constraint::set_is_inequality() {
  set_is_ray_or_point_or_inequality();
}

inline Coefficient_traits::const_reference
Constraint::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return Linear_Row::coefficient(v.id());
}

inline Coefficient_traits::const_reference
Constraint::inhomogeneous_term() const {
  return Linear_Row::inhomogeneous_term();
}

inline memory_size_type
Constraint::external_memory_in_bytes() const {
  return Linear_Row::external_memory_in_bytes();
}

inline memory_size_type
Constraint::total_memory_in_bytes() const {
  return Linear_Row::total_memory_in_bytes();
}

inline void
Constraint::strong_normalize() {
  get_row().normalize();
  sign_normalize();
}

/*! \relates Constraint */
inline bool
operator==(const Constraint& x, const Constraint& y) {
  return x.is_equivalent_to(y);
}

/*! \relates Constraint */
inline bool
operator!=(const Constraint& x, const Constraint& y) {
  return !x.is_equivalent_to(y);
}

/*! \relates Constraint */
inline Constraint
operator==(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff = e1 - e2;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(const Variable v1, const Variable v2) {
  Linear_Expression diff
    = (v1.space_dimension() < v2.space_dimension()) ? v1-v2 : v2-v1;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff = e1 - e2;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Variable v1, const Variable v2) {
  Linear_Expression diff = v1-v2;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Linear_Expression& e1, const Linear_Expression& e2) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  const dimension_type e1_dim = e1.space_dimension();
  const dimension_type e2_dim = e2.space_dimension();
  if (e1_dim > e2_dim)
    diff -= Variable(e1_dim);
  else
    diff -= Variable(e2_dim);
  diff += e1;
  diff -= e2;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Variable v1, const Variable v2) {
  Linear_Expression diff = v1-v2;
  diff -= Variable(std::max(v1.space_dimension(), v2.space_dimension()));
  return Constraint(diff,
                    Constraint::STRICT_INEQUALITY,
                    NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff = n - e;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff = n - e;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += n;
  diff -= e;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator==(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff = e - n;
  return Constraint(diff, Constraint::EQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>=(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff = e - n;
  return Constraint(diff, Constraint::NONSTRICT_INEQUALITY, NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator>(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  Linear_Expression diff;
  // Setting the epsilon coefficient to -1.
  // NOTE: this also enforces normalization.
  diff -= Variable(e.space_dimension());
  diff += e;
  diff -= n;

  return Constraint(diff, Constraint::STRICT_INEQUALITY, NOT_NECESSARILY_CLOSED);
}

/*! \relates Constraint */
inline Constraint
operator<=(const Linear_Expression& e1, const Linear_Expression& e2) {
  return e2 >= e1;
}

/*! \relates Constraint */
inline Constraint
operator<=(const Variable v1, const Variable v2) {
  return v2 >= v1;
}

/*! \relates Constraint */
inline Constraint
operator<=(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  return e >= n;
}

/*! \relates Constraint */
inline Constraint
operator<=(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n >= e;
}

/*! \relates Constraint */
inline Constraint
operator<(const Linear_Expression& e1, const Linear_Expression& e2) {
  return e2 > e1;
}

/*! \relates Constraint */
inline Constraint
operator<(const Variable v1, const Variable v2) {
  return v2 > v1;
}

/*! \relates Constraint */
inline Constraint
operator<(Coefficient_traits::const_reference n, const Linear_Expression& e) {
  return e > n;
}

/*! \relates Constraint */
inline Constraint
operator<(const Linear_Expression& e, Coefficient_traits::const_reference n) {
  return n > e;
}

inline const Constraint&
Constraint::zero_dim_false() {
  PPL_ASSERT(zero_dim_false_p != 0);
  return *zero_dim_false_p;
}

inline const Constraint&
Constraint::zero_dim_positivity() {
  PPL_ASSERT(zero_dim_positivity_p != 0);
  return *zero_dim_positivity_p;
}

inline const Constraint&
Constraint::epsilon_geq_zero() {
  PPL_ASSERT(epsilon_geq_zero_p != 0);
  return *epsilon_geq_zero_p;
}

inline const Constraint&
Constraint::epsilon_leq_one() {
  PPL_ASSERT(epsilon_leq_one_p != 0);
  return *epsilon_leq_one_p;
}

inline void
Constraint::swap(Constraint& y) {
  Linear_Row::swap(y);
  std::swap(flags_, y.flags_);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Constraint */
template <>
inline void
swap(Parma_Polyhedra_Library::Constraint& x,
     Parma_Polyhedra_Library::Constraint& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Constraint_inlines_hh)
