/* Test Grid::affine_image().
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

// Based on an example in a paper by Muller-Olm and Seidl in SAS 2005

bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Grid gr(2, EMPTY);
  gr.add_generator(grid_point(2*A + 0*B));

  print_congruences(gr, "*** gr ***");

  Grid gr0 = gr;  // first grid (using trivial transformation)

  Grid gr1 = gr;  // second grid - initial state

  gr1.generalized_affine_image(B, 18*A + B, 1, 0);
  gr1.generalized_affine_image(A, 15*A, 1, 0);
                  // second grid - 1 pass through procedure

  Grid gr2 = gr;  // third grid - initial state

  gr2.affine_image(B, 282*A + B);
  gr2.affine_image(A, 225*A);
                  // third grid - 2 passes through procedure

  gr.join_assign(gr1); // join of gr0 and gr1

  print_congruences(gr,
        "*** gr.join_assign(gr1) ***");

  gr.join_assign(gr2); // join of gr0, gr1 and gr2

  Grid known_gr(2);

  known_gr.add_congruence((A %= 2) / 28);
  known_gr.add_congruence((B %= 0) / 12);

  bool ok = (gr == known_gr);

  print_congruences(gr,
        "*** gr.join_assign(gr2) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
END_MAIN
