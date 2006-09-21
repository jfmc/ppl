/* MIP_Problem class implementation: non-inline template functions.
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

#ifndef PPL_MIP_Problem_templates_hh
#define PPL_MIP_Problem_templates_hh 1

#include "Variables_Set.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename In>
MIP_Problem::MIP_Problem(const dimension_type dim,
			 In first,
			 In last,
			 const Variables_Set& int_vars,
			 const Linear_Expression& obj,
			 const Optimization_Mode mode)
  : external_space_dim(dim),
    internal_space_dim(0),
    tableau(),
    working_cost(0, Row::Flags()),
    mapping(),
    base(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    input_obj_function(obj),
    opt_mode(mode),
    last_generator(point()) {
  if (!int_vars.empty())
    throw std::runtime_error("PPL internal error: unimplemented feature");

  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::MIP_Problem::"
			    "MIP_Problem(d, first, last, obj, m):\n"
			    "d exceeds the maximum allowed space dimension");
  // Check the objective function.
  if (obj.space_dimension() > dim)
    throw std::invalid_argument("PPL::MIP_Problem::"
				"MIP_Problem(d, first, last, obj, m):\n"
				"the space dimension of obj exceeds d.");
  // Check the constraints.
  for (In i = first; i != last; ++i) {
    if (i->is_strict_inequality())
      throw std::invalid_argument("PPL::MIP_Problem::"
				  "MIP_Problem(d, first, last, obj, m):\n"
				  "range [first, last) contains a strict "
				  "inequality constraint.");
    if (i->space_dimension() > dim)
      throw std::invalid_argument("PPL::MIP_Problem::"
				  "MIP_Problem(d, first, last, obj, m):\n"
				  "range [first, last) contains a constraint "
				  "having space dimension greater than d.");
    input_cs.push_back(*i);
  }
  assert(OK());
}

template <typename In>
MIP_Problem::MIP_Problem(const dimension_type dim,
			 In first,
			 In last,
			 Variables_Integrality integrality,
			 const Linear_Expression& obj,
			 const Optimization_Mode mode)
  : external_space_dim(dim),
    internal_space_dim(0),
    tableau(),
    working_cost(0, Row::Flags()),
    mapping(),
    base(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    input_obj_function(obj),
    opt_mode(mode),
    last_generator(point()) {
  if (integrality == ALL_INTEGER_VARIABLES)
    throw std::runtime_error("PPL internal error: unimplemented feature");

  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::MIP_Problem::"
			    "MIP_Problem(d, first, last, obj, m):\n"
			    "d exceeds the maximum allowed space dimension");
  // Check the objective function.
  if (obj.space_dimension() > dim)
    throw std::invalid_argument("PPL::MIP_Problem::"
				"MIP_Problem(d, first, last, obj, m):\n"
				"the space dimension of obj exceeds d.");
  // Check the constraints.
  for (In i = first; i != last; ++i) {
    if (i->is_strict_inequality())
      throw std::invalid_argument("PPL::MIP_Problem::"
				  "MIP_Problem(d, first, last, obj, m):\n"
				  "range [first, last) contains a strict "
				  "inequality constraint.");
    if (i->space_dimension() > dim)
      throw std::invalid_argument("PPL::MIP_Problem::"
				  "MIP_Problem(d, first, last, obj, m):\n"
				  "range [first, last) contains a constraint "
				  "having space dimension greater than d.");
    input_cs.push_back(*i);
  }
  assert(OK());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_MIP_Problem_templates_hh)
