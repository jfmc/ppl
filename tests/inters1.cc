/* Intersection of an icosahedron with a column.
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

#include <config.h>
#include "ppl.hh"
#include "print.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
 
  Polyhedron icosahedron(3);
  icosahedron.insert(4*x - 2*y - z + 14 >= 0);
  icosahedron.insert(4*x + 2*y - z + 2 >= 0);
  icosahedron.insert(x + y - 1 >= 0);
  icosahedron.insert(x + y + 2*z - 5 >= 0); 
  icosahedron.insert(x + 1 >= 0); 
  icosahedron.insert(x + z - 1 >= 0); 
  icosahedron.insert(2*x + y -2*z + 7 >= 0); 
  icosahedron.insert(x - y + 2*z + 1 >= 0); 
  icosahedron.insert(x - y + 5 >= 0); 
  icosahedron.insert(2*x - y - 2*z + 13 >= 0); 
  icosahedron.insert(-2*x - y + 2*z + 1 >= 0); 
  icosahedron.insert(-x + y - 1 >= 0); 
  icosahedron.insert(-x + y -2*z + 7 >= 0); 
  icosahedron.insert(-4*x + 2*y + z - 4 >= 0); 
  icosahedron.insert(-2*x + y + 2*z - 5 >= 0); 
  icosahedron.insert(-x + 1 >= 0); 
  icosahedron.insert(-x - z + 5 >= 0); 
  icosahedron.insert(-4*x - 2*y + z + 8 >= 0); 
  icosahedron.insert(-x - y + 5 >= 0); 
  icosahedron.insert(-x - y -2*z +13 >= 0); 

  Polyhedron column(3);
  column.insert(y >= 2);
  column.insert(y <= 4);
  column.insert(x >= 0);
  column.insert(x <= 1);
  
  Polyhedron computed_result = icosahedron;
  computed_result.intersection_assign(column);

  Polyhedron known_result(3);
  known_result.insert(-4*x - 2*y + z >= -8);
  known_result.insert(-4*x + 2*y + z >= 4);
  known_result.insert(-2*x - y + 2*z >= -1);
  known_result.insert(-2*x + y + 2*z >= 5);
  known_result.insert(-x - y - 2*z >= -13);
  known_result.insert(-x - z >= -5);
  known_result.insert(-x >= -1);
  known_result.insert(-x + y - 2*z >= -7);
  known_result.insert(-y >= -4);
  known_result.insert(y >= 2);
  known_result.insert(x >= 0);

  int retval = (computed_result == known_result) ? 0 : 1;

#if NOISY
  print_constraints(icosahedron, "*** icosahedron ***");
  print_constraints(column, "*** column ***");
  print_constraints(computed_result, "*** computed_result ***");
#endif

  return retval;
}
