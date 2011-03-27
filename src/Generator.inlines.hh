/* Generator class implementation: inline functions.
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

#ifndef PPL_Generator_inlines_hh
#define PPL_Generator_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Generator.defs.hh"

namespace Parma_Polyhedra_Library {

inline bool
Generator::is_necessarily_closed() const {
  return (topology() == NECESSARILY_CLOSED);
}

inline bool
Generator::is_not_necessarily_closed() const {
  return (topology() == NOT_NECESSARILY_CLOSED);
}

inline dimension_type
Generator::space_dimension() const {
  const dimension_type sz = get_row().size();
  return (sz == 0)
    ? 0
    : sz - (is_necessarily_closed() ? 1 : 2);
}

inline bool
Generator::is_line_or_equality() const {
  return (kind_ == LINE_OR_EQUALITY);
}

inline bool
Generator::is_ray_or_point_or_inequality() const {
  return (kind_ == RAY_OR_POINT_OR_INEQUALITY);
}

inline Topology
Generator::topology() const {
  return topology_;
}

inline void
Generator::set_is_line_or_equality() {
  kind_ = LINE_OR_EQUALITY;
}

inline void
Generator::set_is_ray_or_point_or_inequality() {
  kind_ = RAY_OR_POINT_OR_INEQUALITY;
}

inline void
Generator::set_topology(Topology x) {
  if (topology() == x)
    return;
  if (topology() == NECESSARILY_CLOSED)
    // Add a column for the epsilon dimension.
    get_row().resize(get_row().size() + 1);
  else {
    PPL_ASSERT(get_row().size() > 0);
    get_row().resize(get_row().size() - 1);
  }
  topology_ = x;
}

inline void
Generator::mark_as_necessarily_closed() {
  PPL_ASSERT(is_not_necessarily_closed());
  topology_ = NECESSARILY_CLOSED;
}

inline void
Generator::mark_as_not_necessarily_closed() {
  PPL_ASSERT(is_necessarily_closed());
  topology_ = NOT_NECESSARILY_CLOSED;
}

inline void
Generator::set_necessarily_closed() {
  set_topology(NECESSARILY_CLOSED);
}

inline void
Generator::set_not_necessarily_closed() {
  set_topology(NOT_NECESSARILY_CLOSED);
}

inline
Generator::Generator()
  : Linear_Expression(), kind_(LINE_OR_EQUALITY), topology_(NECESSARILY_CLOSED) {
}

inline
Generator::Generator(dimension_type num_columns)
  : Linear_Expression(),
    kind_(RAY_OR_POINT_OR_INEQUALITY),
    topology_(NOT_NECESSARILY_CLOSED) {
  PPL_ASSERT(num_columns != 0);
  Linear_Expression::set_space_dimension(num_columns - 1);
}

inline
Generator::Generator(dimension_type num_columns, Kind kind, Topology topology)
  : Linear_Expression(), kind_(kind), topology_(topology) {
  PPL_ASSERT(num_columns != 0);
  Linear_Expression::set_space_dimension(num_columns - 1);
}

inline
Generator::Generator(dimension_type num_columns,
                     dimension_type /* num_reserved_columns */)
  : Linear_Expression(),
    kind_(RAY_OR_POINT_OR_INEQUALITY),
    topology_(NOT_NECESSARILY_CLOSED) {
  PPL_ASSERT(num_columns != 0);
  Linear_Expression::set_space_dimension(num_columns - 1);
}

inline
Generator::Generator(dimension_type num_columns,
                     dimension_type /* num_reserved_columns */,
                     Kind kind, Topology topology)
  : Linear_Expression(),
    kind_(kind),
    topology_(topology) {
  PPL_ASSERT(num_columns != 0);
  Linear_Expression::set_space_dimension(num_columns - 1);
}

inline
Generator::Generator(Linear_Expression& e, Type type, Topology topology)
  : topology_(topology) {
  PPL_ASSERT(type != CLOSURE_POINT || topology == NOT_NECESSARILY_CLOSED);
  get_row().swap(e.get_row());
  if (type == LINE)
    kind_ = LINE_OR_EQUALITY;
  else
    kind_ = RAY_OR_POINT_OR_INEQUALITY;
  strong_normalize();
}

inline
Generator::Generator(const Generator& g)
  : Linear_Expression(static_cast<const Linear_Expression&>(g)), kind_(g.kind_), topology_(g.topology_) {
}

inline
Generator::Generator(const Generator& g, dimension_type dimension)
  : Linear_Expression(static_cast<const Linear_Expression&>(g), dimension), kind_(g.kind_), topology_(g.topology_) {
}

inline
Generator::Generator(const Generator& g, dimension_type num_columns,
                     dimension_type /* num_reserved_columns */)
  : Linear_Expression(static_cast<const Linear_Expression&>(g), num_columns), kind_(g.kind_), topology_(g.topology_) {
}

inline
Generator::~Generator() {
}

inline Generator&
Generator::operator=(const Generator& g) {
  Linear_Expression::operator=(g);
  kind_ = g.kind_;
  topology_ = g.topology_;
  return *this;
}

inline Linear_Expression&
Generator::expression() {
  return *this;
}

inline const Linear_Expression&
Generator::expression() const {
  return *this;
}

inline dimension_type
Generator::max_space_dimension() {
  return Linear_Expression::max_space_dimension();
}

inline void
Generator::set_space_dimension(dimension_type space_dim) {
  if (topology() == NECESSARILY_CLOSED) {
    get_row().resize(space_dim + 1);
  } else {
    const dimension_type old_space_dim = space_dimension();
    if (space_dim > old_space_dim) {
      get_row().resize(space_dim + 2);
      get_row().swap(space_dim + 1, old_space_dim + 1);
    } else {
      get_row().swap(space_dim + 1, old_space_dim + 1);
      get_row().resize(space_dim + 2);
    }
  }
  PPL_ASSERT(space_dimension() == space_dim);
}

inline bool
Generator::is_line() const {
  return is_line_or_equality();
}

inline bool
Generator::is_ray_or_point() const {
  return is_ray_or_point_or_inequality();
}

inline bool
Generator::is_line_or_ray() const {
  return get_row()[0] == 0;
}

inline bool
Generator::is_ray() const {
  return is_ray_or_point() && is_line_or_ray();
}

inline Generator::Type
Generator::type() const {
  if (is_line())
    return LINE;
  if (is_line_or_ray())
    return RAY;
  if (is_necessarily_closed())
    return POINT;
  else {
    // Checking the value of the epsilon coefficient.
    const Generator& g = *this;
    return (g.get_row()[get_row().size() - 1] == 0) ? CLOSURE_POINT : POINT;
  }
}

inline bool
Generator::is_point() const {
  return type() == POINT;
}

inline bool
Generator::is_closure_point() const {
  return type() == CLOSURE_POINT;
}

inline void
Generator::set_is_line() {
  set_is_line_or_equality();
}

inline void
Generator::set_is_ray_or_point() {
  set_is_ray_or_point_or_inequality();
}

inline Coefficient_traits::const_reference
Generator::coefficient(const Variable v) const {
  if (v.space_dimension() > space_dimension())
    throw_dimension_incompatible("coefficient(v)", "v", v);
  return Linear_Expression::coefficient(v);
}

inline Coefficient_traits::const_reference
Generator::divisor() const {
  Coefficient_traits::const_reference d = Linear_Expression::inhomogeneous_term();
  if (!is_ray_or_point() || d == 0)
    throw_invalid_argument("divisor()",
			   "*this is neither a point nor a closure point");
  return d;
}

inline memory_size_type
Generator::external_memory_in_bytes() const {
  return Linear_Expression::external_memory_in_bytes();
}

inline memory_size_type
Generator::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

inline void
Generator::strong_normalize() {
  get_row().normalize();
  sign_normalize();
}

inline const Generator&
Generator::zero_dim_point() {
  PPL_ASSERT(zero_dim_point_p != 0);
  return *zero_dim_point_p;
}

inline const Generator&
Generator::zero_dim_closure_point() {
  PPL_ASSERT(zero_dim_closure_point_p != 0);
  return *zero_dim_closure_point_p;
}

/*! \relates Generator */
inline Generator
line(const Linear_Expression& e) {
  return Generator::line(e);
}

/*! \relates Generator */
inline Generator
ray(const Linear_Expression& e) {
  return Generator::ray(e);
}

/*! \relates Generator */
inline Generator
point(const Linear_Expression& e, Coefficient_traits::const_reference d) {
  return Generator::point(e, d);
}

/*! \relates Generator */
inline Generator
closure_point(const Linear_Expression& e,
	      Coefficient_traits::const_reference d) {
  return Generator::closure_point(e, d);
}

/*! \relates Generator */
inline bool
operator==(const Generator& x, const Generator& y) {
  return x.is_equivalent_to(y);
}

/*! \relates Generator */
inline bool
operator!=(const Generator& x, const Generator& y) {
  return !x.is_equivalent_to(y);
}

inline void
Generator::ascii_dump(std::ostream& s) const {
  s << "size " << get_row().size() << " ";
  for (dimension_type j = 0; j < get_row().size(); ++j)
    s << get_row()[j] << ' ';
  switch (type()) {
  case Generator::LINE:
    s << "L ";
    break;
  case Generator::RAY:
    s << "R ";
    break;
  case Generator::POINT:
    s << "P ";
    break;
  case Generator::CLOSURE_POINT:
    s << "C ";
    break;
  }
  if (is_necessarily_closed())
    s << "(C)";
  else
    s << "(NNC)";
  s << "\n";
}

inline bool
Generator::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str))
    return false;
  if (str != "size")
    return false;

  dimension_type sz;
  if (!(s >> sz))
    return false;
  get_row().resize(sz);

  for (dimension_type j = 0; j < get_row().size(); ++j)
    if (!(s >> get_row()[j]))
      return false;

  if (!(s >> str))
    return false;
  if (str == "L")
    set_is_line();
  else if (str == "R" || str == "P" || str == "C")
    set_is_ray_or_point();
  else
    return false;

  std::string str2;

  if (!(s >> str2))
    return false;
  if (str2 == "(C)") {
    if (is_not_necessarily_closed())
      // TODO: Avoid using the mark_as_*() methods if possible.
      mark_as_necessarily_closed();
  } else
    if (str2 == "(NNC)") {
      if (is_necessarily_closed())
        // TODO: Avoid using the mark_as_*() methods if possible.
        mark_as_not_necessarily_closed();
    } else
      return false;

  // Checking for equality of actual and declared types.
  switch (type()) {
  case Generator::LINE:
    if (str != "L")
  return false;
    break;
  case Generator::RAY:
    if (str != "R")
  return false;
    break;
  case Generator::POINT:
    if (str != "P")
  return false;
    break;
  case Generator::CLOSURE_POINT:
    if (str != "C")
  return false;
    break;
  }

  return true;
}

inline void
Generator::swap(Generator& y) {
  Linear_Expression::swap(y);
  std::swap(kind_, y.kind_);
  std::swap(topology_, y.topology_);
}

inline void
Generator::swap(dimension_type i, dimension_type j) {
  get_row().swap(i, j);
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \relates Generator */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename Specialization, typename Temp, typename To>
inline bool
l_m_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
		    const Generator& x,
		    const Generator& y,
		    const Rounding_Dir dir,
		    Temp& tmp0,
		    Temp& tmp1,
		    Temp& tmp2) {
  // Generator kind compatibility check: we only compute distances
  // between (closure) points.
  if (x.is_line_or_ray() || y.is_line_or_ray())
    return false;
  const dimension_type x_space_dim = x.space_dimension();
  // Dimension-compatibility check.
  if (x_space_dim != y.space_dimension())
    return false;

  // All zero-dim generators have distance zero.
  if (x_space_dim == 0) {
    assign_r(r, 0, ROUND_NOT_NEEDED);
    return true;
  }

  PPL_DIRTY_TEMP0(mpq_class, x_coord);
  PPL_DIRTY_TEMP0(mpq_class, y_coord);
  PPL_DIRTY_TEMP0(mpq_class, x_div);
  PPL_DIRTY_TEMP0(mpq_class, y_div);
  assign_r(x_div, x.divisor(), ROUND_NOT_NEEDED);
  assign_r(y_div, y.divisor(), ROUND_NOT_NEEDED);

  assign_r(tmp0, 0, ROUND_NOT_NEEDED);
  for (dimension_type i = x_space_dim; i-- > 0; ) {
    assign_r(x_coord, x.coefficient(Variable(i)), ROUND_NOT_NEEDED);
    div_assign_r(x_coord, x_coord, x_div, ROUND_NOT_NEEDED);
    assign_r(y_coord, y.coefficient(Variable(i)), ROUND_NOT_NEEDED);
    div_assign_r(y_coord, y_coord, y_div, ROUND_NOT_NEEDED);
    const Temp* tmp1p;
    const Temp* tmp2p;

    if (x_coord > y_coord) {
      maybe_assign(tmp1p, tmp1, x_coord, dir);
      maybe_assign(tmp2p, tmp2, y_coord, inverse(dir));
    }
    else {
      maybe_assign(tmp1p, tmp1, y_coord, dir);
      maybe_assign(tmp2p, tmp2, x_coord, inverse(dir));
    }
    sub_assign_r(tmp1, *tmp1p, *tmp2p, dir);
    PPL_ASSERT(sgn(tmp1) >= 0);
    Specialization::combine(tmp0, tmp1, dir);
  }
  Specialization::finalize(tmp0, dir);
  assign_r(r, tmp0, dir);
  return true;
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Generator& x,
			    const Generator& y,
			    const Rounding_Dir dir,
			    Temp& tmp0,
			    Temp& tmp1,
			    Temp& tmp2) {
  return l_m_distance_assign<Rectilinear_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Generator& x,
			    const Generator& y,
			    const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  PPL_DIRTY_TEMP(Checked_Temp, tmp0);
  PPL_DIRTY_TEMP(Checked_Temp, tmp1);
  PPL_DIRTY_TEMP(Checked_Temp, tmp2);
  return rectilinear_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename To>
inline bool
rectilinear_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			    const Generator& x,
			    const Generator& y,
			    const Rounding_Dir dir) {
  return rectilinear_distance_assign<To, To>(r, x, y, dir);
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Generator& x,
			  const Generator& y,
			  const Rounding_Dir dir,
			  Temp& tmp0,
			  Temp& tmp1,
			  Temp& tmp2) {
  return l_m_distance_assign<Euclidean_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Generator& x,
			  const Generator& y,
			  const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  PPL_DIRTY_TEMP(Checked_Temp, tmp0);
  PPL_DIRTY_TEMP(Checked_Temp, tmp1);
  PPL_DIRTY_TEMP(Checked_Temp, tmp2);
  return euclidean_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename To>
inline bool
euclidean_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			  const Generator& x,
			  const Generator& y,
			  const Rounding_Dir dir) {
  return euclidean_distance_assign<To, To>(r, x, y, dir);
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Generator& x,
			   const Generator& y,
			   const Rounding_Dir dir,
			   Temp& tmp0,
			   Temp& tmp1,
			   Temp& tmp2) {
  return l_m_distance_assign<L_Infinity_Distance_Specialization<Temp> >
    (r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename Temp, typename To>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Generator& x,
			   const Generator& y,
			   const Rounding_Dir dir) {
  typedef Checked_Number<Temp, Extended_Number_Policy> Checked_Temp;
  PPL_DIRTY_TEMP(Checked_Temp, tmp0);
  PPL_DIRTY_TEMP(Checked_Temp, tmp1);
  PPL_DIRTY_TEMP(Checked_Temp, tmp2);
  return l_infinity_distance_assign(r, x, y, dir, tmp0, tmp1, tmp2);
}

/*! \relates Generator */
template <typename To>
inline bool
l_infinity_distance_assign(Checked_Number<To, Extended_Number_Policy>& r,
			   const Generator& x,
			   const Generator& y,
			   const Rounding_Dir dir) {
  return l_infinity_distance_assign<To, To>(r, x, y, dir);
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::Generator */
inline void
swap(Parma_Polyhedra_Library::Generator& x,
     Parma_Polyhedra_Library::Generator& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Generator_inlines_hh)
