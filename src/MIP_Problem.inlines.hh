/* MIP_Problem class implementation: inline functions.
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

#ifndef PPL_MIP_Problem_inlines_hh
#define PPL_MIP_Problem_inlines_hh 1

#include "Constraint.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

inline dimension_type
MIP_Problem::max_space_dimension() {
  return Constraint::max_space_dimension();
}

inline dimension_type
MIP_Problem::space_dimension() const {
  return external_space_dim;
}

inline
MIP_Problem::MIP_Problem(const dimension_type dim)
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
    input_obj_function(),
    opt_mode(MAXIMIZATION),
    last_generator(point()) {
  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::MIP_Problem::MIP_Problem(d, cs, obj, m):\n"
			    "d exceeds the maximum allowed space dimension");
  assert(OK());
}

inline
MIP_Problem::MIP_Problem(const dimension_type dim,
			 const Constraint_System& cs,
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
  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::MIP_Problem::MIP_Problem(d, cs, obj, m):\n"
			    "d exceeds the maximum allowed space dimension");
  // Check the objective function.
  if (obj.space_dimension() > dim)
    throw std::invalid_argument("PPL::MIP_Problem::"
				"MIP_Problem(d, cs, obj, m):\n"
				"the space dimension of obj exceeds d.");
  // Check the constraint system.
  if (cs.space_dimension() > dim)
    throw std::invalid_argument("PPL::MIP_Problem::"
				"MIP_Problem(d, cs, obj, m):\n"
				"the space dimension of cs exceeds d.");
  if (cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::MIP_Problem::"
				"MIP_Problem(d, cs, obj, m):\n"
				"cs contains strict inequalities.");
  // Actually copy the constraints.
  input_cs.insert(input_cs.end(), cs.begin(), cs.end());
  assert(OK());
}

inline
MIP_Problem::MIP_Problem(const MIP_Problem& y)
  : external_space_dim(y.external_space_dim),
    internal_space_dim(y.internal_space_dim),
    tableau(y.tableau),
    working_cost(y.working_cost),
    mapping(y.mapping),
    base(y.base),
    status(y.status),
    initialized(y.initialized),
    input_cs(y.input_cs),
    first_pending_constraint(y.first_pending_constraint),
    input_obj_function(y.input_obj_function),
    opt_mode(y.opt_mode),
    last_generator(y.last_generator) {
  assert(OK());
}

inline
MIP_Problem::~MIP_Problem() {
}

inline void
MIP_Problem::add_constraint(const Constraint& c) {
  if (space_dimension() < c.space_dimension())
    throw std::invalid_argument("PPL::MIP_Problem::add_constraint(c):\n"
				"*this and c are dimension-incompatible.");
  if (c.is_strict_inequality())
    throw std::invalid_argument("PPL::MIP_Problem::add_constraint(c):\n"
				"c is a strict inequality.");
  input_cs.push_back(c);
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  assert(OK());
}

inline void
MIP_Problem::add_constraints(const Constraint_System& cs) {
  if (space_dimension() < cs.space_dimension())
    throw std::invalid_argument("PPL::MIP_Problem::add_constraints(cs):\n"
				"*this and cs are dimension-incompatible.");
  if (cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::MIP_Problem::add_constraints(cs):\n"
				"cs contains strict inequalities.");
  input_cs.insert(input_cs.end(), cs.begin(), cs.end());
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  assert(OK());
}

inline void
MIP_Problem::set_objective_function(const Linear_Expression& obj) {
  if (space_dimension() < obj.space_dimension())
    throw std::invalid_argument("PPL::MIP_Problem::"
				"set_objective_function(obj):\n"
				"*this and obj are dimension incompatible.");
  input_obj_function = obj;
  if (status == UNBOUNDED || status == OPTIMIZED)
    status = SATISFIABLE;
  assert(OK());
}

inline void
MIP_Problem::set_optimization_mode(const Optimization_Mode mode) {
  if (opt_mode != mode) {
    opt_mode = mode;
    if (status == UNBOUNDED || status == OPTIMIZED)
      status = SATISFIABLE;
    assert(OK());
  }
}

inline const Linear_Expression&
MIP_Problem::objective_function() const {
  return input_obj_function;
}

inline Optimization_Mode
MIP_Problem::optimization_mode() const {
  return opt_mode;
}

inline const Generator&
MIP_Problem::feasible_point() const {
  if (is_satisfiable())
    return last_generator;
  else
    throw std::domain_error("PPL::MIP_Problem::feasible_point():\n"
			    "*this is not satisfiable.");
}

inline const Generator&
MIP_Problem::optimizing_point() const {
  if (solve() == OPTIMIZED_MIP_PROBLEM)
    return last_generator;
  else
    throw std::domain_error("PPL::MIP_Problem::optimizing_point():\n"
			    "*this doesn't have an optimizing point.");
}

inline MIP_Problem_Status
MIP_Problem::solve() const {
  if (is_satisfiable()) {
    MIP_Problem& x = const_cast<MIP_Problem&>(*this);
    x.second_phase();
    if (x.status == UNBOUNDED)
      return UNBOUNDED_MIP_PROBLEM;
    else {
      assert(x.status == OPTIMIZED);
      return OPTIMIZED_MIP_PROBLEM;
    }
  }
  return UNFEASIBLE_MIP_PROBLEM;
}

inline void
MIP_Problem::optimal_value(Coefficient& num, Coefficient& den) const {
  const Generator& g = optimizing_point();
  evaluate_objective_function(g, num, den);
}

inline MIP_Problem::const_iterator
MIP_Problem::constraints_begin() const {
  return input_cs.begin();
};

inline MIP_Problem::const_iterator
MIP_Problem::constraints_end() const {
  return input_cs.end();
};

inline void
MIP_Problem::swap(MIP_Problem& y) {
  std::swap(external_space_dim, y.external_space_dim);
  std::swap(internal_space_dim, y.internal_space_dim);
  std::swap(tableau, y.tableau);
  std::swap(working_cost, y.working_cost);
  std::swap(mapping, y.mapping);
  std::swap(initialized, y.initialized);
  std::swap(base, y.base);
  std::swap(status, y.status);
  std::swap(input_cs, y.input_cs);
  std::swap(first_pending_constraint, y.first_pending_constraint);
  std::swap(input_obj_function, y.input_obj_function);
  std::swap(opt_mode, y.opt_mode);
  std::swap(last_generator, y.last_generator);
}

inline MIP_Problem&
MIP_Problem::operator=(const MIP_Problem& y) {
  MIP_Problem tmp(y);
  swap(tmp);
  return *this;
}

inline void
MIP_Problem::clear() {
  MIP_Problem tmp;
  swap(tmp);
}

inline void
MIP_Problem::add_space_dimensions_and_embed(const dimension_type m) {
  // The space dimension of the resulting MIP problem should not
  // overflow the maximum allowed space dimension.
  if (m > max_space_dimension() - space_dimension())
    throw std::length_error("PPL::MIP_Problem::"
			    "add_space_dimensions_and_embed(m):\n"
			    "adding m new space dimensions exceeds "
			    "the maximum allowed space dimension");
  external_space_dim += m;
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  assert(OK());
}

inline memory_size_type
MIP_Problem::external_memory_in_bytes() const {
  memory_size_type n
    = tableau.external_memory_in_bytes()
    + working_cost.external_memory_in_bytes()
    + input_obj_function.external_memory_in_bytes()
    + last_generator.external_memory_in_bytes();
  // Adding the external memory for `input_cs'.
  n += input_cs.capacity() * sizeof(Constraint);
  for (const_iterator i = input_cs.begin(),
	 i_end = input_cs.end(); i != i_end; ++i)
    n += (i->external_memory_in_bytes());
  // Adding the external memory for `base'.
  n += base.capacity() * sizeof(dimension_type);
  // Adding the external memory for `mapping'.
  n += mapping.capacity() * sizeof(std::pair<dimension_type, dimension_type>);
  return n;
}

inline memory_size_type
MIP_Problem::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::MIP_Problem */
inline void
swap(Parma_Polyhedra_Library::MIP_Problem& x,
     Parma_Polyhedra_Library::MIP_Problem& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_MIP_Problem_inlines_hh)
