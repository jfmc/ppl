/* Test Polyhedra_Powerset<PH>.
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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY 
#define NOISY 0
#endif

namespace {

Variable x(0);

void
test1() {
  Constraint_System cs = Constraint_System::zero_dim_empty();
  Polyhedra_Powerset<C_Polyhedron> ps(cs);
  if (!ps.OK())
    exit(1);
}

void
test2() {
  Constraint_System cs;
  Polyhedra_Powerset<NNC_Polyhedron> nnc_ps(1, EMPTY);

  cs.clear();
  cs.insert(x > 0);
  cs.insert(x <= 1);
  nnc_ps.add_disjunct(NNC_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x < 1);
  nnc_ps.add_disjunct(NNC_Polyhedron(cs));

  Polyhedra_Powerset<C_Polyhedron> c_ps(nnc_ps);  

  if (!c_ps.OK())
    exit(1);
}

void
test3() {
  Constraint_System cs;
  Polyhedra_Powerset<C_Polyhedron> c_ps(1, EMPTY);

  cs.clear();
  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  c_ps.add_constraint(x == 1);

  Polyhedra_Powerset<NNC_Polyhedron> nnc_ps(c_ps);  

  if (!nnc_ps.OK())
    exit(1);
}

void
test4() {
  Polyhedra_Powerset<C_Polyhedron> c_ps(1, EMPTY);
  Constraint_System cs;

  cs.insert(x >= 0);
  cs.insert(x <= 2);
  c_ps.add_disjunct(C_Polyhedron(cs));

  cs.clear();
  cs.insert(x >= 1);
  cs.insert(x <= 3);
  c_ps.add_disjunct(C_Polyhedron(cs));

  c_ps.concatenate_assign(c_ps);

  if (!c_ps.OK())
    exit(1);
}

} // namespace

int main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
