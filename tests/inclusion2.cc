/* Test Polyhedron::operator<=().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x + 0*y >= 2);
  C_Polyhedron p_half_space(cs);

#if NOISY
  print_constraints(p_half_space, "*** p_half_space constraints ***");
  print_generators(p_half_space, "*** p_half_space generators ***");
#endif

  GenSys gs;
  gs.insert(point(2*x));
  gs.insert(line(x+y));
  C_Polyhedron p_line(gs);

#if NOISY
  print_constraints(p_line, "*** p_line constraints ***");
  print_generators(p_line, "*** p_line generators ***");
#endif

  bool p_half_space_includes_p_line = (p_half_space >= p_line);

#if NOISY
  cout << "p_half_space ";
  if (p_half_space_includes_p_line)
    cout << "includes ";
  else
    cout << "does not include ";
  cout << "or is equal to p_line" << endl;
#endif

  bool p_line_includes_p_half_space = (p_line >= p_half_space);

#if NOISY
  cout << "p_line ";
  if (p_line_includes_p_half_space)
    cout << "includes ";
  else
    cout << "does not include ";
  cout << "or is equal to p_half_space" << endl;
#endif

  return (!p_line_includes_p_half_space
	  && !p_half_space_includes_p_line) ? 0 : 1;
}
CATCH
