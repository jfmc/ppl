/* Simulate the initialization phase of the China analyzer.
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
#include "Partial_Function.defs.hh"

#if NOISY
static void
print_function(const PFunction& function, const string& intro = "",
	       ostream& s = cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}
#endif

TOctagon
init(dimension_type num_vars) {
  TOctagon oc(num_vars);
  if (num_vars > 0) {
    Constraint_System cs;
    for (dimension_type i = num_vars; i-- > 0; )
      cs.insert(Variable(i) >= 0);
    oc = TOctagon(cs);
#if NOISY
    print_constraints(oc, "*** oc ***");
#endif
  }
  return oc;
}

int
main() TRY {
  Partial_Function function;
  function.insert(0, 1);
  function.insert(1, 0);
  function.insert(3, 2);
  function.insert(5, 3);
  function.insert(7, 4);
  function.insert(9, 5);
#if NOISY
  print_function(function, "*** function ***");
#endif

  TOctagon known_result = init(6);

  for (dimension_type i = 10; i < 100; ++i) {
    TOctagon oc = init(i);
    oc.map_space_dimensions(function);
    if (oc != known_result) {
#if NOISY
      print_constraints(oc, "*** oc ***");
      print_constraints(known_result, "*** known_result ***");
#endif
      return 1;
    }
  }

  return 0;
}
CATCH

