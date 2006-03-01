/* Test BD_Shape<T>::minimized_constraints().
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

#include "ppl_test.hh"

namespace {

bool
test01() {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 1);
  bd.add_constraint(B >= 0);
  bd.add_constraint(A - B >= -2);
  bd.add_constraint(A >= -3);
  bd.add_constraint(A <= 1);

  print_constraints(bd, "*** bd ***");

  const Constraint_System cs = bd.minimized_constraints();

  using namespace IO_Operators;
  nout << "*** bd.minimized_constraints() ***" << endl;

  dimension_type num_constraints = 0;
  for (Constraint_System::const_iterator i = cs.begin(),
	 iend = cs.end(); i != iend; ++i) {
    nout << *i << endl;
    ++num_constraints;
  }

  nout << "num_constraints == " << num_constraints << endl;

  C_Polyhedron ph_bd(cs);
  C_Polyhedron known_result(2);
  known_result.add_constraint(A == 1);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(B <= 3);

  bool ok = (num_constraints == 3 && known_result == ph_bd) ;

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
END_MAIN
