/* Test Polyhedron::bounded_BHRZ03_extrapolation_assign().
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

#define PROPAGATION_STEPS 1
#define NUMBER_OF_TOKENS 1

namespace {

void
my_output_function(ostream& s, const Variable& v) { 
  s << char('i' + v.id());
} 

} // namespace 

int
main() TRY {
  Variable i(0);
  Variable j(1);
  Variable k(2);
  Variable::set_output_function(my_output_function); 

  C_Polyhedron ph(3);
  ph.add_constraint(i == 1);
  ph.add_constraint(j == 1);
  ph.add_constraint(0 <= k);
  ph.add_constraint(k <= 1);

  C_Polyhedron old_ph(3, Polyhedron::EMPTY);

  // Propagation.
  for (int steps = 0; steps < PROPAGATION_STEPS; ++steps) {
    old_ph = ph;

    ph.affine_image(i, i+1);
    ph.affine_image(j, j+k);
    ph.affine_image(k, k-1);

    old_ph.poly_hull_assign(ph);

    ph = old_ph;
  }

  // Widening.
  Constraint_System up_to_constraints = ph.constraints();
#if NOISY
  print_constraints(up_to_constraints, "*** up_to_constraints ***");
#endif

  unsigned tokens = NUMBER_OF_TOKENS;

  for (int step = 1; ; ++step) {
#if NOISY
    cout << "\nAt step " << step << endl;
    print_constraints(ph);
#endif

    old_ph = ph;

    ph.affine_image(i, i+1);
    ph.affine_image(j, j+k);
    ph.affine_image(k, k-1);

    ph.poly_hull_assign(old_ph);
    if (old_ph.contains(ph))
      break;

    // Notice that neither the H79 nor the BHRZ03 limited
    // extrapolations (used as widenings here) allow to obtain the
    // desired postfixpoint for this example.  Both the H79 and the
    // BHRZ03 bounded extrapolation operators do achieve this result.
    ph.bounded_H79_extrapolation_assign(old_ph, up_to_constraints, &tokens);
  }

  C_Polyhedron known_result(3);
  known_result.add_constraint(-i - k >= -2);
  known_result.add_constraint(i - j + k >= 0);
  known_result.add_constraint(i - j + 2*k >= -1);
  known_result.add_constraint(i + k >= 1);
  known_result.add_constraint(i >= 1);
  known_result.add_constraint(i - j >= 0);

  bool ok = (ph == known_result);

#if NOISY
  print_constraints(ph, "\nPostfixpoint");
#endif

  return ok ? 0 : 1;
}
CATCH
