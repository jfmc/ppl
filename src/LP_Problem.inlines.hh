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
  : tableau(),
    working_cost(0, Row::Flags()),
    is_artificial(),
    mapping(),
    base(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    pending_input_cs(),
    input_obj_function(),
    opt_mode(MAXIMIZATION),
    last_generator(point())
{
  assert(OK());
}

inline
LP_Problem::LP_Problem(const Constraint_System& cs,
		       const Linear_Expression& obj,
		       const Optimization_Mode mode)
  : tableau(),
    working_cost(0, Row::Flags()),
    is_artificial(),
    mapping(),
    base(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    pending_input_cs(!cs.has_strict_inequalities()
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
    last_generator(point())
{
  assert(OK());
}

inline
LP_Problem::LP_Problem(const LP_Problem& y)
  : tableau(y.tableau),
    working_cost(y.working_cost),
    is_artificial(y.is_artificial),
    mapping(y.mapping),
    base(y.base),
    status(y.status),
    initialized(y.initialized),
    input_cs(y.input_cs),
    pending_input_cs(y.pending_input_cs),
    input_obj_function(y.input_obj_function),
    opt_mode(y.opt_mode),
    last_generator(y.last_generator)
{
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
  pending_input_cs.insert(c);
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  assert(OK());
}

inline void
LP_Problem::add_constraints(const Constraint_System& cs) {
  if (cs.has_strict_inequalities())
    throw std::invalid_argument("PPL::LP_Problem::add_constraints(cs):\n"
				"cs contains strict inequalities.");
  const dimension_type cs_num_rows = cs.num_rows();
  for (dimension_type i = cs_num_rows; i-- > 0; )
    pending_input_cs.insert(cs[i]);
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  assert(OK());
}

inline void
LP_Problem::set_objective_function(const Linear_Expression& obj) {
  if (space_dimension() < obj.space_dimension())
    throw std::invalid_argument("PPL::LP_Problem::"
				"set_objective_function(obj):\n"
				"*this and obj are dimension incompatible.");
  input_obj_function = obj;
  if (status == UNBOUNDED || status == OPTIMIZED)
    status = SATISFIABLE;
  assert(OK());
}

inline void
LP_Problem::set_optimization_mode(Optimization_Mode mode) {
  if (opt_mode != mode) {
    opt_mode = mode;
    if (status == UNBOUNDED || status == OPTIMIZED)
      status = SATISFIABLE;
    //  assert(OK());
  }
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
  if (is_satisfiable())
    return last_generator;
  else
    throw std::domain_error("PPL::LP_Problem::feasible_point():\n"
			    "*this is not satisfiable.");
}

inline const Generator&
LP_Problem::optimizing_point() const {
  if (solve() == OPTIMIZED_LP_PROBLEM)
    return last_generator;
  else
    throw std::domain_error("PPL::LP_Problem::optimizing_point():\n"
			    "*this doesn't have an optimizing point.");
}

inline Constraint_System
LP_Problem::constraints() const {
  // FIXME : avoid the copy if possible.
  Constraint_System cs(input_cs);
  for (dimension_type i = 0,
	 i_end = pending_input_cs.num_rows(); i != i_end; ++i)
    cs.insert(pending_input_cs[i]);
  return cs;
}

inline LP_Problem_Status
LP_Problem::solve() const {
  if (is_satisfiable()) {
    LP_Problem& x = const_cast<LP_Problem&>(*this);
    x.second_phase();
    if (x.status == UNBOUNDED)
      return UNBOUNDED_LP_PROBLEM;
    else {
      assert(x.status == OPTIMIZED);
      return OPTIMIZED_LP_PROBLEM;
    }
  }
  return UNFEASIBLE_LP_PROBLEM;
}

inline void
LP_Problem::optimal_value(Coefficient& num, Coefficient& den) const {
  const Generator& g = optimizing_point();
  evaluate_objective_function(g, num, den);
}

inline void
LP_Problem::swap(LP_Problem& y) {
  std::swap(tableau, y.tableau);
  std::swap(working_cost, y.working_cost);
  std::swap(is_artificial, y.is_artificial);
  std::swap(mapping, y.mapping);
  std::swap(base, y.base);
  std::swap(status, y.status);
  std::swap(input_cs, y.input_cs);
  std::swap(pending_input_cs, y.pending_input_cs);
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
  return Constraint_System::max_space_dimension();
}

inline dimension_type
LP_Problem::space_dimension() const {
  return std::max(input_cs.space_dimension(),
		  pending_input_cs.space_dimension());
}

inline memory_size_type
LP_Problem::external_memory_in_bytes() const {
  memory_size_type n
    = tableau.external_memory_in_bytes()
    + working_cost.external_memory_in_bytes()
    + input_cs.external_memory_in_bytes()
    + pending_input_cs.external_memory_in_bytes()
    + input_obj_function.external_memory_in_bytes()
    + last_generator.external_memory_in_bytes();
  // Adding the external memory for `base' and `is_artificial'.
  n += base.capacity() * sizeof(dimension_type);
  n += is_artificial.capacity() * sizeof(bool);
  // CHECKME: is it right this way of computing the memory used by `mapping'?
  n += mapping.capacity() * sizeof(std::pair<dimension_type, dimension_type>);
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
