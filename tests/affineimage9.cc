/* Test Polyhedron::affine_image().
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

void
my_output_function(ostream& s, const Variable& v) {
  s << char('i' + v.id());
}

} // namespace

int
main() TRY {
  set_handlers();

  Variable i(0), j(1);

  // Install the alternate output function.
  Variable::set_output_function(my_output_function);

  NNC_Polyhedron p1(2);
  p1.add_constraint(j == 0);
  p1.add_constraint(i >= 0);

  NNC_Polyhedron p2(2);
  p2.add_constraint(j == 0);
  p2.add_constraint(-i > 0);

#if NOISY
  print_constraints(p1, "*** p1 ***");
  print_constraints(p2, "*** p2 ***");
#endif

  p1.affine_image(j, i+2);
  p2.affine_image(j, i);

#if NOISY
  print_constraints(p1, "*** p1.affine_image(j, i+2) ***");
  print_constraints(p2, "*** p2.affine_image(j, i) ***");
#endif

  p1.poly_hull_assign_and_minimize(p2);

  NNC_Polyhedron known_result(2);
  known_result.add_constraint(i - j >= -2);
  known_result.add_constraint(-i + j >= 0.);

  int retval = p1 == known_result ? 0 : 1;

#if NOISY
  print_constraints(p1, "*** p1.poly_hull_assign_and_minimize(p2) ***");
#endif

  return retval;
}
CATCH
