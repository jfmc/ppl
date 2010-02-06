/* Test the PIP_Problem class
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

#include "ppl_test.hh"

namespace {

void
display_solution(const PIP_Tree pip, const Variables_Set& parameters,
                 const Variables_Set& vars, dimension_type space_dimension,
                 int indent=0) {
  using namespace std;
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!pip) {
    nout << setw(indent*2) << "" << "_|_" << endl;
  }
  else {
    PIP_Tree_Node::Artificial_Parameter_Sequence::const_iterator i, i_end;
    i_end = pip->art_parameter_end();
    for (i = pip->art_parameter_begin(); i != i_end; ++i) {
      nout << setw(indent*2) << "" << "Parameter "
           << Linear_Expression(Variable(space_dimension++))
           << " = " << *i << endl;
    }
    const Constraint_System &constraints = pip->constraints();
    bool constraints_empty = constraints.empty();
    if (!constraints_empty) {
      nout << setw(indent*2) << "" << "if ";
      for (Constraint_System::const_iterator
             begin = constraints.begin(),
             end = constraints.end(),
             i = begin; i != end; ++i)
        nout << ((i == begin) ? "" : " and ") << *i;
      nout << " then" << endl;
    }
    const PIP_Decision_Node* dn = pip->as_decision();
    if (dn) {
      display_solution(dn->child_node(true), parameters, vars,
                       space_dimension, indent+1);
      nout << setw(indent*2) << "" << "else" << endl;
      display_solution(dn->child_node(false), parameters, vars,
                       space_dimension, indent+1);
    }
    else {
      const PIP_Solution_Node* sn = pip->as_solution();
      nout << setw(indent*2 + (constraints_empty ? 0 : 2)) << "" << "{";
      for (Variables_Set::const_iterator
             begin = vars.begin(),
             end = vars.end(),
             i = begin; i != end; ++i)
        nout << ((i == begin) ? "" : " ; ")
             << sn->parametric_values(Variable(*i), parameters);
      nout << "}" << endl;
      if (!constraints_empty) {
        nout << setw(indent*2) << "" << "else" << endl;
        nout << setw(indent*2+2) << "" << "_|_" << endl;
      }
    }
  }
}

bool
test01() {
  Variable X1(0);
  Variable X2(1);
  Variable I0(2);
  Variable J0(3);
  Variable N(4);
  Variables_Set params(I0, N);

  Constraint_System cs;
  cs.insert(-X1 + N - 1 >= 0);
  cs.insert(X1 - X2 >= 0);
  cs.insert(X1 + I0 == N);
  cs.insert(X2 + J0 - N - 1 >= 0);
  cs.insert(I0 >= 1);
  cs.insert(N >= 1);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(X1, X2),
                     cs.space_dimension());
  }

  return ok;
}

bool
test02() {
  Variable i(0);
  Variable j(1);
  Variable n(2);
  Variable m(3);
  Variables_Set params(n, m);

  Constraint_System cs;
  cs.insert(3*j >= -2*i+8);
  cs.insert(j <= 4*i - 4);
  cs.insert(j <= m);
  //cs.insert(j >= 0);
  cs.insert(i <= n);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     cs.space_dimension());
  }

  return ok;
}

bool
test03() {
  Variable i(0);
  Variable j(1);
  Variable k(2);
  Variable m(3);
  Variable n(4);
  Variables_Set params(k, n);

  Constraint_System cs;
  cs.insert(i <= m);
  cs.insert(j <= n);
  cs.insert(2*i+j == 2*m+n-k);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     cs.space_dimension());
  }

  return ok;
}

bool
test04() {
  Variable i(0);
  Variable j(1);
  Variable k(2);
  Variable m(3);
  Variable n(4);
  Variables_Set params(k, n);

  Constraint_System cs;
  cs.insert(i <= m);
  cs.insert(j <= n);
  cs.insert(2*i+j == 2*m+n-k);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     cs.space_dimension());
  }

  // Copy constructor is no longer buggy.
  {
    PIP_Problem pip_copy = pip;
    // Here we call the destructor of pip_copy
    // and we also destroy the (copied) solution tree of pip_copy.
    const PIP_Tree solution = pip_copy.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     cs.space_dimension());
  }

  return ok;
}

bool
test05() {
  Variable i(0);
  Variable j(1);
  Variable m(2);
  Variable n(3);
  Variables_Set params(m, n);

  Constraint_System cs;
  cs.insert(3*j >= -2*i+8);
  cs.insert(j <= 4*i - 4);
  cs.insert(i <= n);
  cs.insert(j <= m);
  cs.insert(n >= 3);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     cs.space_dimension());
  }

  return ok;
}

bool
test06() {
  Variable i(0);
  Variable n(1);
  Variables_Set params(n);

  Constraint_System cs;
  cs.insert(4*i + 2*n == 1);

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = (pip.solve() == UNFEASIBLE_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i),
                     cs.space_dimension());
  }

  return ok;
}

bool
test07() {
  Variable i(0);
  Variable j(1);
  Variable m(2);
  Variable n(3);
  Variables_Set params(m, n);

  PIP_Problem pip(4);
  pip.add_to_parameter_space_dimensions(params);

  pip.add_constraint(3*j >= -2*i+8);
  pip.add_constraint(j <= 4*i - 4);
  pip.add_constraint(i <= n);
  pip.add_constraint(n >= 3);
  bool ok = (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     pip.space_dimension());
  }

  pip.add_constraint(j <= m);
  ok &= (pip.solve() == OPTIMIZED_PIP_PROBLEM);
  if (ok) {
    const PIP_Tree solution = pip.solution();
    display_solution(solution, params, Variables_Set(i, j),
                     pip.space_dimension());
  }

  return ok;
}

bool
test08() {
  Variable i(0);
  Variable j(1);
  Variable m(2);
  Variable n(3);
  Variables_Set params(m, n);

  PIP_Problem pip(4);
  pip.add_to_parameter_space_dimensions(params);

  Constraint_System cs;
  cs.insert(3*j >= -2*i+8);
  cs.insert(j <= 4*i - 4);
  cs.insert(i <= n);
  cs.insert(n >= 3);
  cs.insert(j <= m);

  pip.add_constraints(cs);

  pip.set_control_parameter(PIP_Problem::PIVOT_ROW_STRATEGY_MAX_COLUMN);

  bool ok = pip.is_satisfiable();
  if (ok) {
    const PIP_Tree solution = pip.optimizing_solution();
    display_solution(solution, params, Variables_Set(i, j),
                     pip.space_dimension());
    pip.clear();
  }

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
END_MAIN
