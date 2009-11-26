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
  // FIXME: to be implemented.
  return s;
}

PPL::PIP_Problem::PIP_Problem(const dimension_type dim)
  : external_space_dim(dim),
    internal_space_dim(0),
    status(PARTIALLY_SATISFIABLE),
    current_solution(0),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    parameters(),
    initial_context(),
    big_parameter_dimension(not_a_dimension()) {
  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::PIP_Problem::PIP_Problem(dim):\n"
                            "dim exceeds the maximum allowed "
                            "space dimension.");
  control_parameters_init();
  PPL_ASSERT(OK());
}

PPL::PIP_Problem::PIP_Problem(const PIP_Problem &y)
  : external_space_dim(y.external_space_dim),
    internal_space_dim(y.internal_space_dim),
    status(y.status),
    current_solution(0),
    initialized(y.initialized),
    input_cs(y.input_cs),
    first_pending_constraint(y.first_pending_constraint),
    parameters(y.parameters),
    initial_context(y.initial_context),
    big_parameter_dimension(y.big_parameter_dimension) {
  if (y.current_solution != 0)
    current_solution = y.current_solution->clone();
  control_parameters_copy(y);
  PPL_ASSERT(OK());
}

PPL::PIP_Problem::~PIP_Problem() {
  if (current_solution != 0)
    delete current_solution;
}

void
PPL::PIP_Problem::control_parameters_init() {
  control_parameters[CUTTING_STRATEGY] = CUTTING_STRATEGY_FIRST;
}

void
PPL::PIP_Problem::control_parameters_copy(const PIP_Problem& y) {
  for (dimension_type i = CONTROL_PARAMETER_NAME_SIZE; i-- > 0; )
    control_parameters[i] = y.control_parameters[i];
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
  case PARTIALLY_SATISFIABLE:
    {
      PIP_Problem& x = const_cast<PIP_Problem&>(*this);
      PIP_Problem_Status return_value;

      if (current_solution == 0)
        x.current_solution = new PIP_Solution_Node();
      if (input_cs.empty()) {
        // No constraints: solution = {0}.
        x.status = OPTIMIZED;
        return OPTIMIZED_PIP_PROBLEM;
      }

      // Look for constraints with only parameter coefficients.
      const Constraint_Sequence::const_iterator c_begin = input_cs.begin();
      const Constraint_Sequence::const_iterator c_end = input_cs.end();
      Constraint_Sequence::const_iterator c;
      const Variables_Set::iterator param_begin = parameters.begin();
      const Variables_Set::iterator param_end = parameters.end();
      Variables_Set::iterator pi;
      dimension_type i;

      // Resize context matrix properly.
      dimension_type num_params = parameters.size() + 1;
      dimension_type num_cols = initial_context.num_columns();
      if (num_cols < num_params)
        x.initial_context.add_zero_columns(num_params - num_cols);

      for (c = c_begin + first_pending_constraint; c != c_end; ++c) {
        const dimension_type c_space_dim = c->space_dimension();
        if (external_space_dim < c_space_dim)
          x.external_space_dim = c_space_dim;
        bool has_nonzero_variable_coefficient = false;
        for (i = 0; i < c_space_dim; ++i) {
          if (c->coefficient(Variable(i)) != 0 && parameters.count(i) == 0) {
            /* Constraint should not be inserted in context. */
            has_nonzero_variable_coefficient = true;
            break;
          }
        }
        if (!has_nonzero_variable_coefficient) {
          // At this point, the constraint must be translated into context row.
          Row row(num_params, Row::Flags());
          for (pi = param_begin, i = 1; pi != param_end; ++pi, ++i)
            row[i] = c->coefficient(Variable(*pi));
          row[0] = c->inhomogeneous_term();
          if (c->is_strict_inequality())
            --row[0];
          x.initial_context.add_row(row);
          if (c->is_equality()) {
            for (i = 0; i < num_params; ++i)
              row[i] = -row[i];
            x.initial_context.add_row(row);
          }
        }
      }

      x.current_solution->update_tableau(*this,
                                         external_space_dim,
                                         first_pending_constraint,
                                         input_cs,
                                         parameters);
      x.internal_space_dim = external_space_dim;
      x.first_pending_constraint = input_cs.size();

      return_value = x.current_solution->solve(x.current_solution,
                                               *this,
                                               initial_context, parameters,
                                               external_space_dim);

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
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  if (external_space_dim < internal_space_dim) {
#ifndef NDEBUG
      cerr << "The internal space dimension of the PIP_Problem is "
	   << "greater than its external space dimension."
	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }

  // Constraint system should be OK.
  const dimension_type input_cs_num_rows = input_cs.size();
  for (dimension_type i = input_cs_num_rows; i-- > 0; )
    if (!input_cs[i].OK())
      return false;

  // Constraint system should be space dimension compatible.
  for (dimension_type i = input_cs_num_rows; i-- > 0; ) {
    if (input_cs[i].space_dimension() > external_space_dim) {
#ifndef NDEBUG
      cerr << "The space dimension of the PIP_Problem is smaller than "
           << "the space dimension of one of its constraints."
	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
  }

  // Test validity of control parameter values.
  Control_Parameter_Value strategy = control_parameters[CUTTING_STRATEGY];
  if (strategy != CUTTING_STRATEGY_FIRST
      && strategy != CUTTING_STRATEGY_DEEPEST) {
#ifndef NDEBUG
    cerr << "Invalid value for the CUTTING_STRATEGY control parameter."
	 << endl;
    ascii_dump(cerr);
#endif
    return false;
  }

  if (big_parameter_dimension != not_a_dimension()
      && parameters.count(big_parameter_dimension) == 0) {
#ifndef NDEBUG
    cerr << "The current value for the big parameter is not a parameter "
         << "dimension."
	 << endl;
    ascii_dump(cerr);
#endif
    return false;
  }

  if (!parameters.OK())
    return false;
  if (!initial_context.OK())
    return false;

  // All checks passed.
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

  s << "\ninitial_context\n";
  initial_context.ascii_dump(s);

  s << "\ncontrol_parameters\n";
  for (dimension_type i = 0; i < CONTROL_PARAMETER_NAME_SIZE; ++i) {
    Control_Parameter_Value value = control_parameters[i];
    switch (value) {
    case CUTTING_STRATEGY_FIRST:
      s << "CUTTING_STRATEGY_FIRST";
      break;
    case CUTTING_STRATEGY_DEEPEST:
      s << "CUTTING_STRATEGY_DEEPEST";
      break;
    default:
      s << "Invalid control parameter value";
    }
    s << "\n";
  }

  s << "\nbig_parameter_dimension: "
    << big_parameter_dimension
    << "\n";
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

  if (!(s >> str) || str != "control_parameters")
    return false;

  for (dimension_type i = 0; i < CONTROL_PARAMETER_NAME_SIZE; ++i) {
    if (!(s >> str))
      return false;
    Control_Parameter_Value value;
    if (str == "CUTTING_STRATEGY_FIRST")
      value = CUTTING_STRATEGY_FIRST;
    if (str == "CUTTING_STRATEGY_DEEPEST")
      value = CUTTING_STRATEGY_DEEPEST;
    else
      return false;
    control_parameters[i] = value;
  }

  if (!(s >> str) || str != "big_parameter_dimension:")
    return false;
  if (!(s >> big_parameter_dimension))
    return false;

  PPL_ASSERT(OK());
  return true;
}

void
PPL::PIP_Problem::clear() {
  external_space_dim = 0;
  internal_space_dim = 0;
  status = PARTIALLY_SATISFIABLE;
  if (current_solution != 0) {
    delete current_solution;
    current_solution = 0;
  }
  initialized = false;
  input_cs.clear();
  first_pending_constraint = 0;
  parameters.clear();
  initial_context.clear();
  control_parameters_init();
  big_parameter_dimension = not_a_dimension();
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

void
PPL::PIP_Problem
::add_to_parameter_space_dimensions(const Variables_Set& p_vars) {
  if (p_vars.space_dimension() > external_space_dim)
    throw std::invalid_argument("PPL::PIP_Problem::"
				"add_to_parameter_space_dimension(p_vars):\n"
				"*this and p_vars are dimension "
				"incompatible.");
  const dimension_type original_size = parameters.size();
  parameters.insert(p_vars.begin(), p_vars.end());
  // Do not allow to turn variables into parameters.
  Variables_Set::const_iterator p;
  Variables_Set::const_iterator end = p_vars.end();
  for (p = p_vars.begin(); p != end; ++p) {
    if (*p < internal_space_dim) {
      throw std::invalid_argument("PPL::PIP_Problem::"
				  "add_to_parameter_space_dimension(p_vars):"
				  "p_vars contain variable indices.");
    }
  }

  // If a new parameter was inserted, set the internal status to
  // PARTIALLY_SATISFIABLE.
  if (parameters.size() != original_size && status != UNSATISFIABLE)
    status = PARTIALLY_SATISFIABLE;
}

void
PPL::PIP_Problem::add_constraint(const Constraint& c) {
  if (c.space_dimension() > external_space_dim) {
    std::ostringstream s;
    s << "PPL::PIP_Problem::add_constraint(c):\n"
      << "dim == "<< external_space_dim << " and c.space_dimension() =="
      << " " << c.space_dimension() << " are dimension "
      "incompatible.";
    throw std::invalid_argument(s.str());
  }
  input_cs.push_back(c);
}

void
PPL::PIP_Problem::add_constraints(const Constraint_System &cs) {
  for (Constraint_System::const_iterator ci = cs.begin(),
         ci_end = cs.end(); ci != ci_end; ++ci)
    add_constraint(*ci);
}

bool
PPL::PIP_Problem::is_satisfiable() const {
  if (status == PARTIALLY_SATISFIABLE)
    solve();
  return status == OPTIMIZED;
}

void
PPL::PIP_Problem::set_control_parameter(Control_Parameter_Value value) {
  switch (value) {
  case CUTTING_STRATEGY_FIRST:
    // Intentionally fall through.
  case CUTTING_STRATEGY_DEEPEST:
    control_parameters[CUTTING_STRATEGY] = value;
    break;
  default:
    throw std::invalid_argument("PPL::PIP_Problem::set_control_parameter(v)"
                                ":\ninvalid value.");
  }
}

void
PPL::PIP_Problem::set_big_parameter_dimension(dimension_type x) {
  if (parameters.count(x) == 0)
    throw std::invalid_argument("PPL::PIP_Problem::"
                                "set_big_parameter_dimension(x):\n"
                                "dimension 'x' is not a parameter.");
  if (x < internal_space_dim)
    throw std::invalid_argument("PPL::PIP_Problem::"
                                "set_big_parameter_dimension(x):\n"
                                "only newly-added parameters can be"
                                "converted into the big parameter.");
  big_parameter_dimension = x;
}
