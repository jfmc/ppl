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

#ifndef PPL_Direct_Product_templates_hh
#define PPL_Direct_Product_templates_hh 1

#include "Interval.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Grid_Generator_System.defs.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

// FIX Direct_Product.cc for full specializations

template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::ascii_load(std::istream& s) {
  std::string str;
  return ((s >> str) && str == "Domain"
	  && (s >> str) && str == "1:"
	  && d1.ascii_load(s)
	  && (s >> str) && str == "Domain"
	  && (s >> str) && str == "2:"
	  && d2.ascii_load(s));
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(const Grid_Generator_System& gs)
  : d2(gs) {
}

template <>
inline
Direct_Product<NNC_Polyhedron, Grid>::Direct_Product(Grid_Generator_System& gs)
  : d2(gs) {
}

template <>
inline const Congruence_System&
Direct_Product<NNC_Polyhedron, Grid>::congruences() const {
  return d2.congruences();
}

template <>
inline void
Direct_Product<NNC_Polyhedron, Grid>::add_grid_generator(const Grid_Generator& g) {
  d2.add_grid_generator(g);
}

template <>
inline void
Direct_Product<C_Polyhedron, Grid>::add_grid_generator(const Grid_Generator& g) {
  d2.add_grid_generator(g);
}

template <>
inline bool
Direct_Product<NNC_Polyhedron, Grid>::add_grid_generator_and_minimize(const Grid_Generator& g) {
  return d2.add_grid_generator_and_minimize(g);
}

template <>
inline bool
Direct_Product<C_Polyhedron, Grid>::add_grid_generator_and_minimize(const Grid_Generator& g) {
  return d2.add_grid_generator_and_minimize(g);
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::empty_reduce_d1_with_d2() {
  d2.minimized_congruences();
  if (d2.is_empty()) {
    if (d1.is_empty())
      return false;
    d1.add_constraint(Constraint::zero_dim_false());
    return true;
  }
  return false;
}

template <typename D1, typename D2>
bool
Direct_Product<D1, D2>::empty_reduce_d2_with_d1() {
  d1.minimized_constraints();
  if (d1.is_empty()) {
    if (d2.is_empty())
      return false;
    d2.add_congruence(Congruence::zero_dim_false());
    return true;
  }
  return true;
}

template <>
inline bool
Direct_Product<NNC_Polyhedron, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}

template <>
inline bool
Direct_Product<C_Polyhedron, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}

#if 0
template <>
inline bool
Direct_Product<BD_Shape<T>, Grid>::reduce_domain1_with_domain2() {
  return empty_reduce_d1_with_d2();
}
#endif

template <>
inline bool
Direct_Product<NNC_Polyhedron, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}

template <>
inline bool
Direct_Product<C_Polyhedron, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}

#if 0
template <typename T>
inline bool
Direct_Product<BD_Shape<T>, Grid>::reduce_domain2_with_domain1() {
  return empty_reduce_d2_with_d1();
}
#endif

#if 0
template <typename D1, typename D2>
inline bool
Direct_Product<D1, D2>::reduce_ph_with_gr() {
  // Skeleton attempt at simple reduction.

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
}
#endif

template <>
inline bool
Direct_Product<Parma_Polyhedra_Library::NNC_Polyhedron,
	       Parma_Polyhedra_Library::Grid>::reduce() {
  bool modified = reduce_domain1_with_domain2();
  if (reduce_domain2_with_domain1()) {
    modified = true;
    while (reduce_domain1_with_domain2() && reduce_domain1_with_domain2());
  }
#if 0
  if (reduce_ph_with_gr()) {
    modified = true;
    while (reduce_ph_with_gr() && reduce_ph_with_gr());
  }
#endif
  return modified;
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

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Direct_Product_templates_hh)
