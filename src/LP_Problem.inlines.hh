/* LP_Problem class implementation: inline functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace Parma_Polyhedra_Library {

inline
LP_Problem::LP_Problem()
  : tableau(),  base(),  dim_map(), status(PROBLEM_UNSOLVED),
    input_cs(), input_obj_function(), opt_mode(MAXIMIZATION),
    last_generator(point()){
  assert(OK());
}

inline
LP_Problem::LP_Problem(const Constraint_System& cs,
		       const Linear_Expression& obj,
		       const Optimization_Mode kind)
  :tableau(), base(), dim_map(), status(PROBLEM_UNSOLVED),
   input_cs(cs), input_obj_function(obj), opt_mode(kind),
   last_generator(point()){
  assert(OK());
}

inline
LP_Problem::LP_Problem(const LP_Problem& y)
  : tableau(y.tableau), base(y.base), dim_map(y.dim_map), status(y.status),
    input_cs(y.input_cs), input_obj_function(y.input_obj_function),
    opt_mode(y.opt_mode), last_generator(y.last_generator) {
  assert(OK());
}

inline
LP_Problem::~LP_Problem() {
}

inline void
LP_Problem::add_constraint(const Constraint& c) {
  input_cs.insert(c);
  if (status != PROBLEM_UNSATISFIABLE)
    // FIXME: Here we should apply the incremental simplex algorithm: for
    // the moment we'll proceed computing a feaseble base from the beginning.
    // As soon as possible the following line will be uncommented.
    // status = PROBLEM_PARTIALLY_SATISFIABLE;
    status = PROBLEM_UNSOLVED;
}

inline void
LP_Problem::add_constraints(const Constraint_System& cs) {
  const dimension_type cs_num_rows = cs.num_rows();
  for (dimension_type i = cs_num_rows; i-- > 0; )
    input_cs.insert(cs[i]);
  if (status != PROBLEM_UNSATISFIABLE)
    // FIXME: Here we should apply the incremental simplex algorithm: for
    // the moment we'll proceed computing a feaseble base from the beginning.
    // As soon as possible the following line will be uncommented.
    // status = PROBLEM_PARTIALLY_SATISFIABLE;
    status = PROBLEM_UNSOLVED;
  assert(OK());
}

inline void
LP_Problem::set_objective_function(const Linear_Expression& obj) {
  switch(status){
  case PROBLEM_UNBOUNDED:
    status = PROBLEM_SATISFIABLE;
    break;
  case PROBLEM_OPTIMIZED:
    status = PROBLEM_SATISFIABLE;
    break;
  default:
    break;
  }
  input_obj_function = obj;
  assert(OK());
}

inline void
LP_Problem::set_optimization_mode(Optimization_Mode new_mode){
  if (opt_mode == new_mode)
    return;
  switch(status){
  case PROBLEM_UNBOUNDED:
    status = PROBLEM_SATISFIABLE;
    break;
  case PROBLEM_OPTIMIZED:
    status = PROBLEM_SATISFIABLE;
    break;
  default:
    break;
  }
  opt_mode = new_mode;
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
LP_Problem::feasible_point() {
  switch(status) {
  case PROBLEM_UNSOLVED:
    {
      if(is_satisfiable()){
	assert(OK());
	return last_generator;
      };
      throw std::domain_error("*this is not satisfiable.");
      break;
    }
  case PROBLEM_UNSATISFIABLE:
    throw std::domain_error("*this is not satisfiable.");
    break;
  case PROBLEM_SATISFIABLE:
    return last_generator;
    break;
  case PROBLEM_UNBOUNDED:
    return last_generator;
    break;
  case PROBLEM_PARTIALLY_SATISFIABLE:
    {
      if(is_satisfiable()){
	assert(OK());
	return last_generator;
      }
    }
    throw std::domain_error("*this is not satisfiable.");
    break;
  case PROBLEM_OPTIMIZED:
    return last_generator;
    break;
}
// If we have reached this point something went wrong.
throw std::runtime_error("PPL internal error");
}

inline const Generator&
LP_Problem::optimizing_point() {
  switch(status) {
  case PROBLEM_UNSOLVED:
    if(solve() == SOLVED_PROBLEM)
      return last_generator;
    throw std::domain_error("*this doesn't have an optimizing point.");
    break;
  case PROBLEM_UNSATISFIABLE:
    throw std::domain_error("*this doesn't have an optimizing point.");
    break;
  case PROBLEM_SATISFIABLE:
    {
    if(solve() == SOLVED_PROBLEM)
      return last_generator;
    break;
    }
  case PROBLEM_UNBOUNDED:
    throw std::domain_error("*this doesn't have an optimizing point.");
    break;
  case PROBLEM_PARTIALLY_SATISFIABLE:
    {
      if(solve() == SOLVED_PROBLEM)
	return last_generator;
      throw std::domain_error("*this doesn't have an optimizing point.");
      break;
    }
  case PROBLEM_OPTIMIZED:
    return last_generator;
    break;
  }
  // If we have reached this point something went wrong.
  throw std::runtime_error("PPL internal error");
};

inline const Constraint_System&
LP_Problem::constraints() const{
  return input_cs;
}

inline Simplex_Status
LP_Problem::solve() {
  switch(status){
  case PROBLEM_UNSATISFIABLE:
    return UNFEASIBLE_PROBLEM;
  case PROBLEM_SATISFIABLE:
    {
      Simplex_Status new_status = second_phase();
      assert(OK());
      return new_status;
      break;
    }
  default:
    {
      if(is_satisfiable()){
	assert(OK());
	Simplex_Status new_status = second_phase();
	assert(OK());
	return new_status;
      }
      return UNFEASIBLE_PROBLEM;
    }
  }
  // If we have reached this point something went wrong.
   throw std::runtime_error("PPL internal error");
}

inline void
LP_Problem::optimal_value(Coefficient& num, Coefficient& den){
  const Generator& g_ref = optimizing_point();
  evaluate_objective_function(g_ref, num, den);
  assert(OK());
}

inline bool
LP_Problem::OK() const {
  // FIXME: still to be completed...
  if (status == PROBLEM_SATISFIABLE || status == PROBLEM_OPTIMIZED
      || status == PROBLEM_UNBOUNDED) {
    // The dimension of the last computed generator must be equal to
    // the input `Constraint_System' one.
    if(last_generator.space_dimension() != input_cs.space_dimension())
      return false;
    if(!input_cs.satisfies_all_constraints(last_generator))
      return false;
  }
  return true;
}

inline void
LP_Problem::swap(LP_Problem& y) {
  std::swap(tableau, y.tableau);
  std::swap(base, y.base);
  std::swap(working_cost, y.working_cost);
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
  // FIXME.
  assert(false);
  return tableau.external_memory_in_bytes()
    + working_cost.total_memory_in_bytes()
    // + base.external_memory_in_bytes()
    // + dim_map.external_memory_in_bytes()
    + input_cs.external_memory_in_bytes()
    + input_obj_function.external_memory_in_bytes()
    + last_generator.external_memory_in_bytes()
   + sizeof(opt_mode);
}

inline memory_size_type
LP_Problem::total_memory_in_bytes() const {
  // FIXME.
  assert(false);
  return tableau.total_memory_in_bytes()
    + working_cost.total_memory_in_bytes()
    // + base.total_memory_in_bytes()
    // + dim_map.total_memory_in_bytes()
    + input_cs.total_memory_in_bytes()
    + input_obj_function.total_memory_in_bytes()
    + last_generator.external_memory_in_bytes()
    + sizeof(opt_mode);
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
