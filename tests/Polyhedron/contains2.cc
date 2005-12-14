/* Test Polyhedron::contains(const Polyhedron&).
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

  Variable x(0);
  Variable y(1);

  Constraint_System cs;
  cs.insert(x + 0*y >= 2);
  C_Polyhedron p_half_space(cs);

  print_constraints(p_half_space, "*** p_half_space constraints ***");
  print_generators(p_half_space, "*** p_half_space generators ***");

  Generator_System gs;
  gs.insert(point(2*x));
  gs.insert(line(x+y));
  C_Polyhedron p_line(gs);

  print_constraints(p_line, "*** p_line constraints ***");
  print_generators(p_line, "*** p_line generators ***");

  bool p_half_space_includes_p_line = p_half_space.contains(p_line);

  nout << "p_half_space ";
  if (p_half_space_includes_p_line)
    nout << "includes ";
  else
    nout << "does not include ";
  nout << "or is equal to p_line" << endl;

  bool p_line_includes_p_half_space = p_line.contains(p_half_space);

  nout << "p_line ";
  if (p_line_includes_p_half_space)
    nout << "includes ";
  else
    nout << "does not include ";
  nout << "or is equal to p_half_space" << endl;

  return (!p_line_includes_p_half_space
	  && !p_half_space_includes_p_line) ? 0 : 1;
}
CATCH
