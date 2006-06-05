/* Direct_Product class implementation: inline functions.
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

#ifndef PPL_Direct_Product_inlines_hh
#define PPL_Direct_Product_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::max_space_dimension() {
  return std::min(D1::max_space_dimension(), D2::max_space_dimension());
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(dimension_type num_dimensions,
				       const Degenerate_Element kind)
  : d1(num_dimensions, kind), d2(num_dimensions, kind) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Congruence_System& ccgs)
  : d1(ccgs), d2(ccgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Congruence_System& cgs)
  : d1(const_cast<const Congruence_System&>(cgs)), d2(cgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Constraint_System& ccs)
  : d1(ccs), d2(ccs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Constraint_System& cs)
  : d1(const_cast<const Constraint_System&>(cs)), d2(cs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Grid_Generator_System& gs)
  : d1(gs), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Grid_Generator_System& gs)
  : d1(const_cast<const Grid_Generator_System&>(gs)), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Direct_Product& y)
  : d1(y.d1), d2(y.d2) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::~Direct_Product() {
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::external_memory_in_bytes() const {
  return d1.external_memory_in_bytes() + d2.external_memory_in_bytes();
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::space_dimension() const {
  assert(d1.space_dimension() == d2.space_dimension());
  return d1.space_dimension();
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::upper_bound_assign(const Direct_Product& y) {
  d1.upper_bound_assign(y.d1);
  d2.upper_bound_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::swap(Direct_Product& y) {
  std::swap(d1, y.d1);
  std::swap(d2, y.d2);
}

// FIX .templates.hh (some should be inlined)

template <typename D1, typename D2>
void
Direct_Product<D1, D2>::add_constraint(const Constraint& c) {
  d1.add_constraint(c);
  d2.add_constraint(c);
}

template <typename D1, typename D2>
void
Direct_Product<D1, D2>::add_congruence(const Congruence& cg) {
  d1.add_congruence(cg);
  d2.add_congruence(cg);
}

template <typename D1, typename D2>
Direct_Product<D1, D2>&
Direct_Product<D1, D2>::operator=(const Direct_Product& y) {
  d1 = y.d1;
  d2 = y.d2;
  return *this;
}

template <typename D1, typename D2>
const D1&
Direct_Product<D1, D2>::domain1() const {
  return d1;
}

template <typename D1, typename D2>
const D2&
Direct_Product<D1, D2>::domain2() const {
  return d2;
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::reduce() {
  bool modified = reduce_domain1_with_domain2();
  if (reduce_domain2_with_domain1()) {
    modified = true;
    while (reduce_domain1_with_domain2() && reduce_domain1_with_domain2());
  }
  return modified;
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::reduce_domain1_with_domain2() {
  return false;
#if 0
  // Skeleton attempt at simple reduction for Polhderon-Grid.

  // Reduce ph d1 with gr d2 by moving ph c's to nearest grid point
  // (any grid point, inside or outside the ph).

  // Always either ==, >= or >.
  // FIX include >
  // for each axis

  // FIX using dim 1 (A)

  //    flatten points to axis

  D2 d2_copy = d2;
  //d2_copy.remove_higher_space_dimensions(1);
  // FIX need to leave single space dim

  //    for each relational c in ph

  if (d1.has_pending_constraints() && !d1.process_pending_constraints())
    // d1 found empty.
    return false;

  TEMP_INTEGER(temp);
  bool modified = false;
  Constraint_System& cs = const_cast<Constraint_System&>(d1.constraints());
  for (Constraint_System::const_iterator i = cs.begin(),
         cs_end = cs.end(); i != cs_end; ++i) {
    const Constraint& c = *i;

    c.ascii_dump();

  //       if the constraint affects the axis

    if (c.is_equality() || c.coefficient(Variable(0)) == 0)
      continue;

  //          incr/decr const term to nearest grid point (depending on direction of relation)

    // FIX assume >=

    std::cout << "adjust c" << std::endl;

    // FIX include c divisors

    // FIX will signs of cterms be the same?

    Constraint& writable_c = const_cast<Constraint&>(c);
    Congruence_System& cgs = const_cast<Congruence_System&>(d2_copy.minimized_congruences());
    std::cout << "   using cg:" << std::endl;
    cgs.begin()->ascii_dump();
    // FIX include cg cterm <> 0

    // Substitute into cg the FIX extreme value of the single
    // dimension of c.
    temp = (- writable_c[0] *
	    (cgs.begin()->coefficient(Variable(0))))
      + (cgs.begin()->inhomogeneous_term());
    // Find the distance to the next module closest to the origin.
    temp %= (cgs.begin()->modulus() * writable_c[1]);
    writable_c[0] -= temp;
    if (temp < 0)
      // Raise to next module.
      writable_c[0] -= cgs.begin()->modulus();

    writable_c.strong_normalize();

    std::cout << "c after" << std::endl;
    c.ascii_dump();

    // FIX now need to adjust the entire cs? clear minimal form?
    d1.clear_constraints_minimized();
    d1.clear_generators_up_to_date();
    modified = true;
  }

  return modified;
#endif
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::reduce_domain2_with_domain1() {
  return false;
}

PPL_OUTPUT_2_PARAM_TEMPLATE_DEFINITIONS(D1, D2, Direct_Product)

template <typename D1, typename D2>
void
Direct_Product<D1, D2>::ascii_dump(std::ostream& s) const {
  s << "Domain 1:\n";
  d1.ascii_dump(s);
  s << "Domain 2:\n";
  d2.ascii_dump(s);
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::ascii_load(std::istream& s) {
  std::string str;
  return ((s >> str) && str == "Domain"
	  && (s >> str) && str == "1:"
	  && d1.ascii_load(s)
	  && (s >> str) && str == "Domain"
	  && (s >> str) && str == "2:"
	  && d2.ascii_load(s));
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::OK() const {
  return d1.OK() && d2.OK();
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
bool operator==(const Direct_Product<D1, D2>& x,
		const Direct_Product<D1, D2>& y) {
  return x.d1 == y.d1 && x.d2 == y.d2;
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
bool operator!=(const Direct_Product<D1, D2>& x,
		const Direct_Product<D1, D2>& y) {
  return !(x == y);
}

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
std::ostream&
IO_Operators::operator<<(std::ostream& s, const Direct_Product<D1, D2>& dp) {
  return s << "Domain 1:\n"
	   << dp.d1
	   << "Domain 2:\n"
	   << dp.d2;
}

} // namespace Parma_Polyhedra_Library

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline void
std::swap(Parma_Polyhedra_Library::Direct_Product<D1, D2>& x,
	  Parma_Polyhedra_Library::Direct_Product<D1, D2>& y) {
  x.swap(y);
}

#endif // !defined(PPL_Direct_Product_inlines_hh)
