/* PIP_Problem class implementation: non-inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <ppl-config.h>
#include "PIP_Problem.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

/*! \relates Parma_Polyhedra_Library::PIP_Problem */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const PIP_Problem& /*p*/) {
 return s;
}

PPL::PIP_Problem::PIP_Problem(dimension_type dim)
  : external_space_dim(dim),
    internal_space_dim(0),
    status(PARTIALLY_SATISFIABLE),
    current_solution(0),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    parameters(),
    initial_context() {
  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::PIP_Problem:: PIP_Problem(dim):\n"
                            "dim exceeds the maximum allowed"
                            "space dimension.");
  PPL_ASSERT(OK());
}

PPL::PIP_Problem::PIP_Problem(const PIP_Problem &y)
  : external_space_dim(y.external_space_dim),
    internal_space_dim(y.internal_space_dim),
    status(y.status),
    current_solution(y.current_solution),
    initialized(y.initialized),
    input_cs(y.input_cs),
    first_pending_constraint(y.first_pending_constraint),
    parameters(y.parameters),
    initial_context(y.initial_context) {
  PPL_ASSERT(OK());
}

PPL::PIP_Problem_Status
PPL::PIP_Problem::solve() const {
  switch (status) {
  case UNSATISFIABLE:
    PPL_ASSERT(OK());
    return UNFEASIBLE_PIP_PROBLEM;
  case OPTIMIZED:
    PPL_ASSERT(OK());
    return OPTIMIZED_PIP_PROBLEM;
  case SATISFIABLE:
    // Intentionally fall through
  case PARTIALLY_SATISFIABLE:
    {
      PIP_Problem& x = const_cast<PIP_Problem&>(*this);
      PIP_Problem_Status return_value;

      if (current_solution == 0)
        x.current_solution = new PIP_Solution_Node(&x);
      if (input_cs.empty()) {
        // no constraints: solution = {0}
        return OPTIMIZED_PIP_PROBLEM;
      }

      //look for constraints with only parameter coefficients
      Constraint_Sequence::const_iterator c;
      Constraint_Sequence::const_iterator c_end = input_cs.end();
      Variables_Set::iterator param_begin = parameters.begin();
      Variables_Set::iterator param_end = parameters.end();
      Variables_Set::iterator pi;
      dimension_type i;

      // resize context matrix properly
      dimension_type num_params = parameters.size()+1;
      dimension_type num_cols = initial_context.num_columns();
      if (num_cols < num_params)
        x.initial_context.add_zero_columns(num_params-num_cols);

      for (c = input_cs.begin()+first_pending_constraint; c != c_end; ++c) {
        dimension_type width = c->space_dimension();
        if (external_space_dim < width)
          x.external_space_dim = width;
        for (i = 0; i < width; ++i) {
          if (c->coefficient(Variable(i)) != 0 && parameters.count(i) == 0)
            /* nonzero variable coefficient, constraint not to be inserted
              in context */
            break;
        }
        if (i == width) {
          // At this point, the constraint must be translated into context row
          Row row(num_params, Row::Flags());
          for (pi = param_begin, i = 1; pi != param_end; ++pi, ++i)
            row[i] = c->coefficient(Variable(*pi));
          row[0] = c->inhomogeneous_term();
          x.initial_context.add_row(row);
        }
      }

      x.current_solution->update_tableau(external_space_dim,
                                         first_pending_constraint,
                                         input_cs,
                                         parameters);
      x.internal_space_dim = external_space_dim;
      x.first_pending_constraint = input_cs.size();

      return_value = x.current_solution->solve(x.current_solution,
                                               initial_context);

      switch (return_value) {
      case UNFEASIBLE_PIP_PROBLEM:
        x.status = UNSATISFIABLE;
        break;
      case OPTIMIZED_PIP_PROBLEM:
        x.status = OPTIMIZED;
        break;
      }
      PPL_ASSERT(OK());
      return return_value;
    }
  }
  // We should not be here!
  throw std::runtime_error("PPL internal error");
}

PPL::PIP_Tree
PPL::PIP_Problem::solution() const {
  return current_solution;
}

PPL::PIP_Tree
PPL::PIP_Problem::optimizing_solution() const {
  return current_solution;
}

bool
PPL::PIP_Problem::OK() const {
  //FIXME
  return true;
}

void
PPL::PIP_Problem::ascii_dump(std::ostream& s) const {
  using namespace IO_Operators;
  s << "\nexternal_space_dim: " << external_space_dim << " \n";
  s << "\ninternal_space_dim: " << internal_space_dim << " \n";

  const dimension_type input_cs_size = input_cs.size();

  s << "\ninput_cs( " << input_cs_size << " )\n";
  for (dimension_type i = 0; i < input_cs_size; ++i)
    input_cs[i].ascii_dump(s);

  s << "\nfirst_pending_constraint: " <<  first_pending_constraint
    << std::endl;

  s << "\ninitialized: " << (initialized ? "YES" : "NO") << "\n";

  s << "\nstatus: ";
  switch (status) {
  case UNSATISFIABLE:
    s << "UNSATISFIABLE";
    break;
  case SATISFIABLE:
    s << "SATISFIABLE";
    break;
  case OPTIMIZED:
    s << "OPTIMIZED";
    break;
  case PARTIALLY_SATISFIABLE:
    s << "PARTIALLY_SATISFIABLE";
    break;
  }
  s << "\n";

  s << "\nparameters";
  parameters.ascii_dump(s);

  s << "\ninitial_context";
  initial_context.ascii_dump(s);
}

PPL_OUTPUT_DEFINITIONS(PIP_Problem)

bool
PPL::PIP_Problem::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "external_space_dim:")
    return false;

  if (!(s >> external_space_dim))
    return false;

  if (!(s >> str) || str != "internal_space_dim:")
    return false;

  if (!(s >> internal_space_dim))
    return false;

  if (!(s >> str) || str != "input_cs(")
    return false;

  dimension_type input_cs_size;

  if (!(s >> input_cs_size))
    return false;

  if (!(s >> str) || str != ")")
    return false;

  Constraint c(Constraint::zero_dim_positivity());
  for (dimension_type i = 0; i < input_cs_size; ++i) {
    if (!c.ascii_load(s))
      return false;
    input_cs.push_back(c);
  }

  if (!(s >> str) || str != "first_pending_constraint:")
    return false;

  if (!(s >> first_pending_constraint))
    return false;

  if (!(s >> str) || str != "initialized:")
    return false;
  if (!(s >> str))
    return false;
  if (str == "YES")
    initialized = true;
  else if (str == "NO")
    initialized = false;
  else
    return false;

  if (!(s >> str) || str != "status:")
    return false;

  if (!(s >> str))
    return false;

  if (str == "UNSATISFIABLE")
    status = UNSATISFIABLE;
  else if (str == "SATISFIABLE")
    status = SATISFIABLE;
  else if (str == "OPTIMIZED")
    status = OPTIMIZED;
  else if (str == "PARTIALLY_SATISFIABLE")
    status = PARTIALLY_SATISFIABLE;
  else
    return false;

  if (!(s >> str) || str != "parameters")
    return false;

  if (!parameters.ascii_load(s))
    return false;

  if (!(s >> str) || str != "initial_context")
    return false;

  if (!initial_context.ascii_load(s))
    return false;

  PPL_ASSERT(OK());
  return true;
}

void
PPL::PIP_Problem
::add_space_dimensions_and_embed(const dimension_type m_pip_vars,
                                 const dimension_type m_pip_params) {
  // The space dimension of the resulting PIP problem should not
  // overflow the maximum allowed space dimension.
  dimension_type available = max_space_dimension() - space_dimension();
  bool should_throw = (m_pip_vars > available);
  if (!should_throw) {
    available -= m_pip_vars;
    should_throw = (m_pip_params > available);
  }
  if (should_throw)
    throw std::length_error("PPL::PIP_Problem::"
			    "add_space_dimensions_and_embed(m_v, m_p):\n"
			    "adding m_v+m_p new space dimensions exceeds "
			    "the maximum allowed space dimension.");
  // First add PIP variables ...
  external_space_dim += m_pip_vars;
  // ... then add PIP parameters.
  for (dimension_type i = m_pip_params; i-- > 0; ) {
    parameters.insert(Variable(external_space_dim));
    ++external_space_dim;
  }
  // Update problem status.
  if (status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
  PPL_ASSERT(OK());
}
