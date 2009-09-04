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

void
PPL::PIP_Problem::Rational_Matrix::normalize() {
  //FIXME
}

PPL::PIP_Problem::PIP_Problem(dimension_type dim)
  : external_space_dim(dim),
    internal_space_dim(0),
    tableau(),
    basis(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    parameters() {
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
    tableau(y.tableau),
    basis(y.basis),
    status(y.status),
    initialized(y.initialized),
    input_cs(y.input_cs),
    first_pending_constraint(y.first_pending_constraint),
    parameters(y.parameters) {
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

      x.update_tableau();
      //FIXME: implement the simplex algorithm

      return_value = OPTIMIZED_PIP_PROBLEM;

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

void
PPL::PIP_Problem::update_tableau() {
  dimension_type i;
  dimension_type n_params = parameters.size();
  dimension_type n_vars = external_space_dim - n_params;
  dimension_type n_vars_int = tableau.s.num_columns();
  dimension_type n_constr_int = tableau.s.num_rows();
  const_iterator cst;

  if (tableau.t.num_columns() == 0) {
    // Create the parameter column, corresponding to the constant term
    tableau.t.add_zero_columns(1);
  }

  for (i=internal_space_dim; i<external_space_dim; ++i) {
    // add new columns to the tableau
    if (parameters.count(i) == 1)
      tableau.t.add_zero_columns(1);
    else
      tableau.s.add_zero_columns(1);
  }
  internal_space_dim = external_space_dim;

  Coefficient denom_s = tableau.s.get_denominator();
  Coefficient denom_t = tableau.t.get_denominator();

  for (cst = constraints_begin() + first_pending_constraint;
       cst < constraints_end(); ++cst) {
    // FIXME: must handle nonbasic variables aswell
    int v = 0;
    int p = 1;
    Row var(n_vars, Row::Flags());
    Row param(n_params+1, Row::Flags());
    Coefficient cnst_term = -cst->inhomogeneous_term();
    if (cst->is_strict_inequality())
      // convert c > 0  <=>  c-1 >= 0
      cnst_term -= 1;
    param[0] = cnst_term * denom_t;
    for (i=0; i<internal_space_dim; i++) {
      if (parameters.count(i) == 1) {
        param[p] = cst->coefficient(Variable(i)) * denom_t;
        ++p;
      } else {
        var[v] = cst->coefficient(Variable(i)) * denom_s;
        ++v;
      }
    }
    //FIXME: must handle equality constraints
    tableau.s.add_row(var);
    tableau.t.add_row(param);
  }

  // update current basis with newly inserted variables
  dimension_type next_var = n_vars_int + n_constr_int;
  for (i=n_vars_int; i<n_vars; ++i)
    basis.insert(next_var++);
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

  s << "\nsimplex_tableau\n";
  tableau.s.ascii_dump(s);

  s << "\nparameter_tableau\n";
  tableau.t.ascii_dump(s);
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

  if (!(s >> str) || str != "simplex_tableau")
    return false;
  if (!tableau.s.ascii_load(s))
    return false;

  if (!(s >> str) || str != "parameter_tableau")
    return false;
  if (!tableau.t.ascii_load(s))
    return false;

  PPL_ASSERT(OK());
  return true;
}

void
PPL::PIP_Problem::Rational_Matrix::ascii_dump(std::ostream& s) const {
  s << "denominator " << denominator << "\n";
  Matrix::ascii_dump(s);
}

bool
PPL::PIP_Problem::Rational_Matrix::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "denominator")
    return false;
  Coefficient den;
  if (!(s >> den))
    return false;
  denominator = den;

  return Matrix::ascii_load(s);
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
