/* Test Polyhedron::shrink_bounding_box().
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "BBox.hh"

namespace {

bool
test01() {
  C_Polyhedron ph(1, EMPTY);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

  BBox known_box(ph.space_dimension());
  known_box.set_empty();

  bool ok (nbox == known_box);

  print_constraints(ph, "*** ph ***");
  nbox.print(nout, "*** nbox ***");
  known_box.print(nout, "*** known_box ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
