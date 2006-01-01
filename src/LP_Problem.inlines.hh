/* LP_Problem class implementation: inline functions.
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

#ifndef PPL_LP_Problem_inlines_hh
#define PPL_LP_Problem_inlines_hh 1

#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include <stdexcept>

namespace Parma_Polyhedra_Library {

inline
LP_Problem::LP_Problem()
  : tableau(), working_cost(0, Row::Flags()),
    base(),  dim_map(), status(OPTIMIZED),
    input_cs(), input_obj_function(), opt_mode(MAXIMIZATION),
    last_generator(point()) {
  assert(OK());
}

inline
LP_Problem::LP_Problem(const Constraint_System& cs,
		       const Linear_Expression& obj,
		       const Optimization_Mode mode)
  : tableau(), working_cost(0, Row::Flags()),
    base(), dim_map(), status(UNSOLVED),
    input_cs(!cs.has_strict_inequalities()
	     ? cs
	     : (throw std::invalid_argument("PPL::LP_Problem::"
			   "LP_Problem(cs, obj, m):\n"
			   "cs contains strict inequalities."),
		cs)),
    input_obj_function(obj.space_dimension() <= cs.space_dimension()
		       ? obj
		       : (throw std::invalid_argument("PPL::LP_Problem::"
			             "LP_Problem(cs, obj, m):\n"
				     "cs and obj have "
				     "incompatible space dimensions."),
			  obj)),
    opt_mode(mode),
    last_generator(point()) {
  assert(OK());
}

inline
LP_Problem::LP_Problem(const LP_Problem& y)
  : tableau(y.tableau), working_cost(y.working_cost),
    base(y.base), dim_map(y.dim_map), status(y.status),
    input_cs(y.input_cs), input_obj_function(y.input_obj_function),
    opt_mode(y.opt_mode), last_generator(y.last_generator) {
  assert(OK());
}

inline
LP_Problem::~LP_Problem() {
}

inline void
LP_Problem::add_constraint(const Constraint& c) {
  if (c.is_strict_inequality())
    throw std::invalid_argument("PPL::LP_Problem::add_constraint(c):\n"
				"c is a strict inequality.");
  input_cs.insert(c);
  if (status != UNSATISFIABLE)
    // FIXME: Here we should apply the incremental simplex algorithm: for
    // the moment we'll proceed computing a feasible base from the beginning.
    // As soon as possible the following line will be uncommented.
    // status = PARTIALLY_SATISFIABLE;
    status = UNSOLVED;
}

inline void
LP_Problem::add_constraints(const Constraint_System& cs) {
  if (cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::LP_Problem::add_constraints(cs):\n"
				"cs contains strict inequalities.");
  const dimension_type cs_num_rows = cs.num_rows();
  for (dimension_type i = cs_num_rows; i-- > 0; )
    input_cs.insert(cs[i]);
  if (status != UNSATISFIABLE)
    // FIXME: Here we should apply the incremental simplex algorithm: for
    // the moment we'll proceed computing a feaseble base from the beginning.
    // As soon as possible the following line will be uncommented.
    // status = PARTIALLY_SATISFIABLE;
    status = UNSOLVED;
  assert(OK());
}

inline void
LP_Problem::set_objective_function(const Linear_Expression& obj) {
  if (space_dimension() < obj.space_dimension())
    throw std::invalid_argument("PPL::LP_Problem::"
				"set_objective_function(obj):\n"
				"*this and obj are dimension incompatible.");
  switch (status) {
  case UNBOUNDED:
    status = SATISFIABLE;
    break;
  case OPTIMIZED:
    status = SATISFIABLE;
    break;
  default:
    break;
  }
  input_obj_function = obj;
  assert(OK());
}

inline void
LP_Problem::set_optimization_mode(Optimization_Mode mode) {
  if (opt_mode == mode)
    return;
  switch (status) {
  case UNBOUNDED:
    status = SATISFIABLE;
    break;
  case OPTIMIZED:
    status = SATISFIABLE;
    break;
  default:
    break;
  }
  opt_mode = mode;
  assert(OK());
}

inline const Linear_Expression&
LP_Problem::objective_function() const {
  return input_obj_function;
}

inline Optimization_Mode
LP_Problem::optimization_mode() const {
  return opt_mode;
}

inline const Generator&
LP_Problem::feasible_point() const {
  if (is_satisfiable()) {
    assert(OK());
    return last_generator;
  }
  throw std::domain_error("PPL::LP_Problem::feasible_point():\n"
			  "*this is not satisfiable.");
}

inline const Generator&
LP_Problem::optimizing_point() const {
  if (solve() == OPTIMIZED_LP_PROBLEM)
    return last_generator;
  throw std::domain_error("PPL::LP_Problem::optimizing_point():\n"
			  "*this doesn't have an optimizing point.");
}

inline const Constraint_System&
LP_Problem::constraints() const {
  return input_cs;
}

inline LP_Problem_Status
LP_Problem::solve() const {
  if (is_satisfiable()) {
    LP_Problem& x = const_cast<LP_Problem&>(*this);
    x.second_phase();
    if (x.status == UNBOUNDED)
      return UNBOUNDED_LP_PROBLEM;
    if (x.status == OPTIMIZED)
      return OPTIMIZED_LP_PROBLEM;
  }
  return UNFEASIBLE_LP_PROBLEM;
}

inline void
LP_Problem::optimal_value(Coefficient& num, Coefficient& den) const {
  const Generator& g_ref = optimizing_point();
  evaluate_objective_function(g_ref, num, den);
  assert(OK());
}

inline void
LP_Problem::swap(LP_Problem& y) {
  std::swap(tableau, y.tableau);
  std::swap(working_cost, y.working_cost);
  std::swap(base, y.base);
  std::swap(dim_map, y.dim_map);
  std::swap(status, y.status);
  std::swap(input_cs, y.input_cs);
  std::swap(input_obj_function, y.input_obj_function);
  std::swap(opt_mode, y.opt_mode);
  std::swap(last_generator, y.last_generator);
}

inline LP_Problem&
LP_Problem::operator=(const LP_Problem& y) {
  LP_Problem tmp(y);
  swap(tmp);
  return *this;
}

inline void
LP_Problem::clear() {
  LP_Problem tmp;
  swap(tmp);
}

inline dimension_type
LP_Problem::max_space_dimension() {
  // FIXME.
  assert(false);
  return Constraint_System::max_space_dimension();
}

inline dimension_type
LP_Problem::space_dimension() const {
  return input_cs.space_dimension();
}

inline memory_size_type
LP_Problem::external_memory_in_bytes() const {
  memory_size_type n
    = tableau.external_memory_in_bytes()
    + working_cost.external_memory_in_bytes()
    + input_cs.external_memory_in_bytes()
    + input_obj_function.external_memory_in_bytes()
    + last_generator.external_memory_in_bytes();
  // Adding the external memory for `base'.
  n += base.capacity() * sizeof(dimension_type);
  // Adding the external memory for `dim_map'.
  // CHECK ME: just a lower approximation?
  n += dim_map.size()
    * sizeof(std::map<dimension_type, dimension_type>::value_type);
  return n;
}

inline memory_size_type
LP_Problem::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

} // namespace Parma_Polyhedra_Library

namespace std {

/*! \relates Parma_Polyhedra_Library::LP_Problem */
inline void
swap(Parma_Polyhedra_Library::LP_Problem& x,
     Parma_Polyhedra_Library::LP_Problem& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_LP_Problem_inlines_hh)
