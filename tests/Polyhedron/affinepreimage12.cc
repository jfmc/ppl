/* Test Polyhedron::affine_preimage(): the examples from definitions.dox.
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

Variable A(0);
Variable B(1);

void
test1() {
  Generator_System gs;
  gs.insert(point());
  gs.insert(point(0*A + 3*B));
  gs.insert(point(3*A + 0*B));
  gs.insert(point(3*A + 3*B));

  C_Polyhedron ph(gs);

  C_Polyhedron known_result = ph;

  print_generators(ph, "--- ph before ph.affine_image(A, A + 2*B + 4) ---");

  ph.affine_image(A, A + 2*B + 4);

  print_generators(ph, "--- ph after ph.affine_image(A, A + 2*B + 4) ---");

  ph.affine_preimage(A, A + 2*B + 4);

  int ok = (ph == known_result) ? 1 : 0;

  print_generators(ph, "--- ph after ph.affine_preimage(A, A + 2*B + 4) ---");

  if (!ok)
    exit(1);
}

void
test2() {
  Generator_System gs;
  gs.insert(point());
  gs.insert(point(0*A + 3*B));
  gs.insert(point(3*A + 0*B));
  gs.insert(point(3*A + 3*B));

  C_Polyhedron ph(gs);

  print_generators(ph, "--- ph before ph.affine_image(A, B) ---");

  ph.affine_image(A, B);

  print_generators(ph, "--- ph after ph.affine_image(A, B) ---");

  ph.affine_preimage(A, B);

  C_Polyhedron known_result(2);
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(B <= 3);

  int ok = (ph == known_result) ? 1 : 0;

  print_generators(ph, "--- ph after ph.affine_preimage(A, B) ---");

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();

  return 0;
}
CATCH
