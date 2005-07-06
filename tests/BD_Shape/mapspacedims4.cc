/* Test BD_Shape::map_space_dimensions().
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

#ifndef NOISY
#define NOISY 0
#endif
#include "PFunction.hh"

#if NOISY
void
print_function(const PFunction& function, const string& intro = "",
	       ostream& s = cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}
#endif

int
main() TRY {
  PFunction function;
  function.insert(0, 1);
  function.insert(1, 0);

  TBD_Shape bd(0);

#if NOISY
  print_constraints(bd, "*** bd ***");
  print_function(function, "*** function ***");
#endif

  bd.map_space_dimensions(function);

  TBD_Shape known_result(0);

#if NOISY
  print_constraints(bd, "*** bd.map_space_dimension(function) ***");
#endif

  int retval = (bd == known_result) ? 0 : 1;

  return retval;
}
CATCH
