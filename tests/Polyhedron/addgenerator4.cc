/* Test Polyhedron::add_generator(): we add points and rays of
   a system of generators that is not necessarily closed to a
   necessarily closed polyhedron.
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

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(closure_point(3*A, 2));
  gs.insert(point(7*A, 4));
  gs.insert(ray(A - B));

  print_generators(gs, "*** gs ***");

  C_Polyhedron ph(2, EMPTY);

  for (Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i)
    if (!(*i).is_closure_point())
      ph.add_generator(*i);

  Generator_System gs_known;
  gs_known.insert(point(7*A + 0*B, 4));
  gs_known.insert(ray(A - B));
  C_Polyhedron known_result(gs_known);

  int retval = (ph == known_result) ? 0 : 1;

  print_generators(gs, "*** gs ***");
  print_generators(ph, "*** ph ***");

  return retval;
}
CATCH
