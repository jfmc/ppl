/* Utilities for termination analysis: template functions.
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

#ifndef PPL_termination_templates_hh
#define PPL_termination_templates_hh 1

#include "Constraint_System.defs.hh"
#include "C_Polyhedron.defs.hh"
#include "BD_Shape.defs.hh"
#include "Octagonal_Shape.defs.hh"

#define PRINT_DEBUG_INFO 0

#if PRINT_DEBUG_INFO
#include <iostream>
#endif

namespace Parma_Polyhedra_Library {

namespace Implementation {

namespace Termination {

#if PRINT_DEBUG_INFO
static dimension_type output_function_MS_n;
static dimension_type output_function_MS_m;
/* Encodes which object are we printing:

   0 means input constraint system;
   1 means first output constraint system;
   2 means second output constraint system;
   3 means only output constraint system
     (i.e., when first and second are the same);
   4 means mu space.
*/
static int output_function_MS_which = -1;

/*
  Debuggin output function.  See the documentation of
  fill_constraint_systems_MS() for the allocation of variable indices.
*/
inline void
output_function_MS(std::ostream& s, const Variable& v) {
  dimension_type id = v.id();
  switch (output_function_MS_which) {
  case 0:
    if (id < output_function_MS_n)
      s << "x'" << id + 1;
    else if (id < 2*output_function_MS_n)
      s << "x" << id - output_function_MS_n + 1;
    else
      s << "WHAT?";
    break;
  case 1:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "WHAT?";
    else if (id <= output_function_MS_n + output_function_MS_m)
      s << "y" << id - output_function_MS_n;
    else
      s << "WHAT?";
    break;
  case 2:
  case 4:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "mu0";
    else if (output_function_MS_which == 2
	     && id <= output_function_MS_n + output_function_MS_m + 2)
      s << "z" << id - output_function_MS_n;
    else
      s << "WHAT?";
    break;
  case 3:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "mu0";
    else if (id <= output_function_MS_n + output_function_MS_m)
      s << "y" << id - output_function_MS_n;
    else if (id <= output_function_MS_n + 2*output_function_MS_m + 2)
      s << "z" << id - (output_function_MS_n + output_function_MS_m);
    else
      s << "WHAT?";
    break;
  default:
    abort();
    break;
  }
}
#endif

void
fill_constraint_systems_MS(const Constraint_System& cs,
			   const dimension_type n,
			   const dimension_type m,
			   Constraint_System& cs_out1,
			   Constraint_System& cs_out2);

void
fill_constraint_system_PR(const Constraint_System& cs,
			  const dimension_type n,
			  const dimension_type m,
			  Constraint_System& cs_out);

template <typename PSET>
void
assign_all_inequalities_approximation(const PSET& pset,
				      Constraint_System& cs) {
  cs = pset.minimized_constraints();
  if (cs.has_strict_inequalities() || cs.has_equalities() > 0) {
    Constraint_System tmp_cs;
    for (Constraint_System::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      const Constraint& c = *i;
      if (c.is_equality()) {
	// Insert the two corresponding opposing inequalities.
	tmp_cs.insert(Linear_Expression(c) >= 0);
	tmp_cs.insert(Linear_Expression(c) <= 0);
      }
      else if (c.is_strict_inequality())
	// Insert the non-strict approximation.
	tmp_cs.insert(Linear_Expression(c) >= 0);
      else
	// Insert as is.
	tmp_cs.insert(c);
    }
    cs = tmp_cs;
  }
}

template <>
void
assign_all_inequalities_approximation(const C_Polyhedron& ph,
				      Constraint_System& cs);

template <typename T>
void
assign_all_inequalities_approximation(const BD_Shape<T>& bds,
				      Constraint_System& cs) {
  cs = bds.minimized_constraints();
}

template <typename T>
void
assign_all_inequalities_approximation(const Octagonal_Shape<T>& ocs,
				      Constraint_System& cs) {
  cs = ocs.minimized_constraints();
}

template <typename PSET>
void
prepare_input_MS_PR(const PSET& pset, Constraint_System& cs,
		    dimension_type& n, dimension_type& m,
		    const char* function) {
  dimension_type space_dim = pset.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL" << function << "(pset, ...):\n"
      << "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  n = space_dim/2;
  assign_all_inequalities_approximation(pset, cs);
  m = std::distance(cs.begin(), cs.end());

#if PRINT_DEBUG_INFO
  output_function_MS_n = n;
  output_function_MS_m = m;
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs ***" << std::endl;
  output_function_MS_which = 0;
  using namespace IO_Operators;
  std::cout << cs << std::endl;

  Variable::set_output_function(p_default_output_function);
#endif
}

} // namespace Termination

} // namespace Implementation

template <typename PSET>
bool
termination_test_MS(const PSET& pset) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "termination_test_MS");

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_mip ***" << std::endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << std::endl;
  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  return mip.is_satisfiable();
}

template <typename PSET>
bool
one_affine_ranking_function_MS(const PSET& pset, Generator& mu) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "one_affine_ranking_function_MS");

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_mip ***" << std::endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << std::endl;
  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  if (mip.is_satisfiable()) {
    Generator fp = mip.feasible_point();
    assert(fp.is_point());
    Linear_Expression le;
    for (dimension_type i = n+1; i-- > 0; ) {
      Variable vi(i);
      le += vi*fp.coefficient(vi);
    }
    mu = point(le, fp.divisor());
    return true;
  }
  else
    return false;
}

template <typename PSET>
void
all_affine_ranking_functions_MS(const PSET& pset, C_Polyhedron& mu_space) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "all_affine_ranking_functions_MS");

  Constraint_System cs_out1;
  Constraint_System cs_out2;
  fill_constraint_systems_MS(cs, n, m, cs_out1, cs_out2);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_out1 ***" << std::endl;
  output_function_MS_which = 1;
  using namespace IO_Operators;
  std::cout << cs_out1 << std::endl;

  std::cout << "*** cs_out2 ***" << std::endl;
  output_function_MS_which = 2;
  using namespace IO_Operators;
  std::cout << cs_out2 << std::endl;
#endif

  C_Polyhedron ph1(cs_out1);
  C_Polyhedron ph2(cs_out2);
  ph1.remove_higher_space_dimensions(n);
  ph1.add_space_dimensions_and_embed(1);
  ph2.remove_higher_space_dimensions(n+1);

#if PRINT_DEBUG_INFO
  std::cout << "*** ph1 projected ***" << std::endl;
  output_function_MS_which = 4;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << std::endl;

  std::cout << "*** ph2 projected ***" << std::endl;
  std::cout << ph2.minimized_constraints() << std::endl;
#endif

  ph1.intersection_assign(ph2);

#if PRINT_DEBUG_INFO
  std::cout << "*** intersection ***" << std::endl;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << std::endl;
  Variable::set_output_function(p_default_output_function);
#endif

  mu_space.swap(ph1);
}

template <typename PSET>
bool
termination_test_PR(const PSET& pset) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "termination_test_PR");

  Constraint_System cs_mip;
  fill_constraint_system_PR(cs, n, m, cs_mip);

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  return mip.is_satisfiable();
}

template <typename PSET>
bool
one_affine_ranking_function_PR(const PSET& pset, Generator& mu) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "one_affine_ranking_function_PR");

  Constraint_System cs_mip;
  fill_constraint_system_PR(cs, n, m, cs_mip);

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  if (mip.is_satisfiable()) {
    Generator fp = mip.feasible_point();
    // FIXME: must project fp to obtain 3, then multiply by E'_C
    // and assign the result to mu.
    //return true;
    return false;
  }
  else
    return false;
}

template <typename PSET>
void
all_affine_ranking_functions_PR(const PSET& pset, C_Polyhedron& mu_space) {
  using namespace Implementation::Termination;
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "all_affine_ranking_functions_PR");

  Constraint_System cs_out;
  fill_constraint_system_PR(cs, n, m, cs_out);

  mu_space = C_Polyhedron(n+1, EMPTY);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_termination_templates_hh)
