/* Different ways of creating an empty polyhedron.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_install.hh"
#include "print.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

int 
main() {
  Variable x(0);
  Variable y(1);
  Polyhedron ph(2);
  ph.insert(vertex(0*x + 0*y));
  ph.insert(vertex(0*x + 3*y));
  ph.insert(vertex(3*x + 0*y));
  ph.insert(vertex(3*x + 3*y));
  LinExpression coeff = x + 0*y + 4;

  Polyhedron p1(ph);
  p1.assign_variable(x, coeff);

  Polyhedron p2(ph);
  p2.substitute_variable(x, coeff);

  Polyhedron p1_known_result(2);
  p1_known_result.insert(vertex(4*x + 0*y));
  p1_known_result.insert(vertex(4*x + 3*y));
  p1_known_result.insert(vertex(7*x + 0*y));
  p1_known_result.insert(vertex(7*x + 3*y));

  Polyhedron p2_known_result(2);
  p2_known_result.insert(vertex(-4*x + 0*y));
  p2_known_result.insert(vertex(-4*x + 3*y));
  p2_known_result.insert(vertex(-1*x + 0*y));
  p2_known_result.insert(vertex(-1*x + 3*y));

  int retval = ((p1 == p1_known_result) && (p2 == p2_known_result)) ? 0 : 1;
  
  return retval;
}
