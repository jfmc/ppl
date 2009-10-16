/* Test the PIP_Problem class
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

#include "ppl_test.hh"

namespace {

void
display_solution(const PIP_Tree pip, const Variables_Set& params,
                 const Variables_Set& vars, dimension_type space_dimension,
                 int indent=0) {
  using namespace std;
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!pip) {
    nout << setw(indent*2) << "" << "_|_" << endl;
  } else {
    Variables_Set parameters(params);
    dimension_type new_params
        = pip->insert_artificials(parameters, space_dimension);
    if (new_params > 0) {
      PIP_Tree_Node::Artificial_Parameter_Sequence::const_iterator i, i_end;
      i_end = pip->art_parameter_end();
      for (i=pip->art_parameter_begin(); i!=i_end; ++i) {
        nout << setw(indent*2) << "" << "Parameter "
             << Linear_Expression(Variable(space_dimension++))
             << " = " << *i << endl;
      }
    }
    const Constraint_System &constraints = pip->constraints();
    bool constraints_empty = constraints.empty();
    if (!constraints_empty) {
      nout << setw(indent*2) << "" << "if ";
      Constraint_System::const_iterator begin = constraints.begin();
      Constraint_System::const_iterator end = constraints.end();
      Constraint_System::const_iterator i;
      for (i = begin; i != end; ++i)
        nout << ((i==begin)?"":" and ") << *i;
      nout << " then" << endl;
    }
    const PIP_Decision_Node* dn = pip->as_decision();
    if (dn) {
      display_solution(dn->child_node(true), parameters, vars,
                       space_dimension, indent+1);
      nout << setw(indent*2) << "" << "else" << endl;
      display_solution(dn->child_node(false), parameters, vars,
                       space_dimension, indent+1);
    } else {
      const PIP_Solution_Node* sn = pip->as_solution();
      Variables_Set::const_iterator begin = vars.begin();
      Variables_Set::const_iterator end = vars.end();
      Variables_Set::const_iterator i;
      nout << setw(indent*2+(constraints_empty?0:2)) << "" << "{";
      for (i=begin; i!=end; ++i)
        nout << ((i==begin)?"":" ; ")
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
  cs.insert(X1 + I0 - N >= 0);
  cs.insert(-X1 - I0 + N >= 0);
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
  cs.insert(2*i+j <= 2*m+n-k);
  cs.insert(2*i+j >= 2*m+n-k);

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
  cs.insert(2*i+j <= 2*m+n-k);
  cs.insert(2*i+j >= 2*m+n-k);

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

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN
