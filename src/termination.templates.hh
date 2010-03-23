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

static dimension_type output_function_PR_s;
static dimension_type output_function_PR_r;

/*
  Debuggin output function.  See the documentation of
  fill_constraint_system_PR() for the allocation of variable indices.
*/
inline void
output_function_PR(std::ostream& s, const Variable& v) {
  dimension_type id = v.id();
  if (id < output_function_PR_s)
    s << "u3_" << id + 1;
  else if (id < output_function_PR_s + output_function_PR_r)
    s << "u2_" << id - output_function_PR_s + 1;
  else if (id < output_function_PR_s + 2*output_function_PR_r)
    s << "u1_" << id - (output_function_PR_s + output_function_PR_r) + 1;
  else
    s << "WHAT?";
}
#endif

void fill_constraint_systems_MS(const Constraint_System& cs,
				const dimension_type n,
				const dimension_type m,
				Constraint_System& cs_out1,
				Constraint_System& cs_out2);

void fill_constraint_system_PR(const Constraint_System& cs,
			       const dimension_type n,
			       const dimension_type m,
			       dimension_type& r,
			       Constraint_System& cs_out,
			       Linear_Expression& le_out);

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

void
shift_unprimed_variables(Constraint_System& cs);

template <typename PSET>
void
assign_all_inequalities_approximation(const PSET& pset_before,
				      const PSET& pset_after,
				      Constraint_System& cs) {
  assign_all_inequalities_approximation(pset_before, cs);
  shift_unprimed_variables(cs);
  Constraint_System cs_after;
  assign_all_inequalities_approximation(pset_after, cs_after);
  // FIXME: provide an "append" for constraint systems.
  for (Constraint_System::const_iterator i = cs_after.begin(),
	 cs_after_end = cs_after.end(); i != cs_after_end; ++i)
    cs.insert(*i);
}

bool
termination_test_MS(const Constraint_System& cs);

bool
one_affine_ranking_function_MS(const Constraint_System& cs,
			       Generator& mu);

void
all_affine_ranking_functions_MS(const Constraint_System& cs,
				C_Polyhedron& mu_space);

bool
termination_test_PR(const Constraint_System& cs_before,
		    const Constraint_System& cs_after);

bool
one_affine_ranking_function_PR(const Constraint_System& cs_before,
			       const Constraint_System& cs_after,
			       Generator& mu);

void
all_affine_ranking_functions_PR(const Constraint_System& cs_before,
				const Constraint_System& cs_after,
				NNC_Polyhedron& mu_space);

} // namespace Termination

} // namespace Implementation

template <typename PSET>
bool
termination_test_MS(const PSET& pset) {
  const dimension_type space_dim = pset.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::termination_test_MS(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset, cs);
  return termination_test_MS(cs);
}

template <typename PSET>
bool
termination_test_MS_2(const PSET& pset_before, const PSET& pset_after) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::termination_test_MS_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset_before, pset_after, cs);
  return termination_test_MS(cs);
}

template <typename PSET>
bool
one_affine_ranking_function_MS(const PSET& pset, Generator& mu) {
  const dimension_type space_dim = pset.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::one_affine_ranking_function_MS(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset, cs);
  return one_affine_ranking_function_MS(cs, mu);
}

template <typename PSET>
bool
one_affine_ranking_function_MS_2(const PSET& pset_before,
				 const PSET& pset_after,
				 Generator& mu) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::one_affine_ranking_function_MS_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset_before, pset_after, cs);
  return one_affine_ranking_function_MS(cs, mu);
}

template <typename PSET>
void
all_affine_ranking_functions_MS(const PSET& pset, C_Polyhedron& mu_space) {
  const dimension_type space_dim = pset.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::all_affine_ranking_functions_MS(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset, cs);
  all_affine_ranking_functions_MS(cs, mu_space);
}

template <typename PSET>
void
all_affine_ranking_functions_MS_2(const PSET& pset_before,
				  const PSET& pset_after,
				  C_Polyhedron& mu_space) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::all_affine_ranking_functions_MS_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs;
  assign_all_inequalities_approximation(pset_before, pset_after, cs);
  all_affine_ranking_functions_MS(cs, mu_space);
}

template <typename PSET>
bool
termination_test_PR_2(const PSET& pset_before, const PSET& pset_after) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::termination_test_PR_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs_before;
  Constraint_System cs_after;
  assign_all_inequalities_approximation(pset_before, cs_before);
  assign_all_inequalities_approximation(pset_after, cs_after);
  return termination_test_PR(cs_before, cs_after);
}

template <typename PSET>
bool
termination_test_PR(const PSET& pset_after) {
  const dimension_type space_dim = pset_after.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::termination_test_PR(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  // FIXME: this may be inefficient.
  PSET pset_before(pset_after);
  Variables_Set primed_variables(Variable(0), Variable(space_dim/2 - 1));
  pset_before.remove_space_dimensions(primed_variables);
  return termination_test_PR_2(pset_before, pset_after);
}

template <typename PSET>
bool
one_affine_ranking_function_PR_2(const PSET& pset_before,
				 const PSET& pset_after,
				 Generator& mu) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::one_affine_ranking_function_PR_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs_before;
  Constraint_System cs_after;
  assign_all_inequalities_approximation(pset_before, cs_before);
  assign_all_inequalities_approximation(pset_after, cs_after);
  return one_affine_ranking_function_PR(cs_before, cs_after, mu);
}

template <typename PSET>
bool
one_affine_ranking_function_PR(const PSET& pset_after, Generator& mu) {
  const dimension_type space_dim = pset_after.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::one_affine_ranking_function_PR(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  // FIXME: this may be inefficient.
  PSET pset_before(pset_after);
  Variables_Set primed_variables(Variable(0), Variable(space_dim/2 - 1));
  pset_before.remove_space_dimensions(primed_variables);
  return one_affine_ranking_function_PR_2(pset_before, pset_after, mu);
}

template <typename PSET>
void
all_affine_ranking_functions_PR_2(const PSET& pset_before,
				  const PSET& pset_after,
				  NNC_Polyhedron& mu_space) {
  const dimension_type before_space_dim = pset_before.space_dimension();
  const dimension_type after_space_dim = pset_after.space_dimension();
  if (after_space_dim != 2*before_space_dim) {
    std::ostringstream s;
    s << "PPL::all_affine_ranking_functions_MS_2(pset_before, pset_after):\n"
         "pset_before.space_dimension() == " << before_space_dim
      << ", pset_after.space_dimension() == " << after_space_dim
      << ";\nthe latter should be twice the former.";
    throw std::invalid_argument(s.str());
  }

  using namespace Implementation::Termination;
  Constraint_System cs_before;
  Constraint_System cs_after;
  assign_all_inequalities_approximation(pset_before, cs_before);
  assign_all_inequalities_approximation(pset_after, cs_after);
  all_affine_ranking_functions_PR(cs_before, cs_after, mu_space);
}

template <typename PSET>
void
all_affine_ranking_functions_PR(const PSET& pset_after,
				NNC_Polyhedron& mu_space) {
  const dimension_type space_dim = pset_after.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL::all_affine_ranking_functions_PR(pset):\n"
         "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  // FIXME: this may be inefficient.
  PSET pset_before(pset_after);
  Variables_Set primed_variables(Variable(0), Variable(space_dim/2 - 1));
  pset_before.remove_space_dimensions(primed_variables);
  all_affine_ranking_functions_PR_2(pset_before, pset_after, mu_space);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_termination_templates_hh)
