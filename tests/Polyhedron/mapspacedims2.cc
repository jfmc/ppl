/* Test Polyhedron::map_space_dimensions().
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
#include "PFunction.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

#if NOISY
namespace {

void
print_function(const PFunction& function, const std::string& intro = "",
	       std::ostream& s = std::cout) {
  if (!intro.empty())
    s << intro << endl;
  function.print(s);
}

} // namespace
#endif


int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  PFunction rotate_right;
  rotate_right.insert(0, 1);
  rotate_right.insert(1, 2);
  rotate_right.insert(2, 0);

  PFunction rotate_left;
  rotate_left.insert(0, 2);
  rotate_left.insert(1, 0);
  rotate_left.insert(2, 1);

  C_Polyhedron ph(3);
  ph.add_constraint(-4*x - 2*y + z >= -8);
  ph.add_constraint(-4*x + 2*y + z >= 4);
  ph.add_constraint(-2*x - y + 2*z >= -1);
  ph.add_constraint(-2*x + y + 2*z >= 5);
  ph.add_constraint(-x - y - 2*z >= -13);
  ph.add_constraint(-x - z >= -5);
  ph.add_constraint(-x >= -1);
  ph.add_constraint(-x + y - 2*z >= -7);
  ph.add_constraint(-y >= -4);
  ph.add_constraint(y >= 2);
  ph.add_constraint(x >= 0);

#if NOISY
  print_constraints(ph, "*** ph ***");
  print_function(rotate_right, "*** rotate_right ***");
  print_function(rotate_left, "*** rotate_left ***");
#endif

  C_Polyhedron rs[4];
  rs[0] = ph;
#if NOISY
  print_constraints(rs[0], "*** rs[0] ***");
#endif
  for (int i = 1; i <= 3; ++i) {
    rs[i] = rs[i-1];
    rs[i].map_space_dimensions(rotate_right);
#if NOISY
    print_constraints(rs[i], "*** rs[i] ***");
#endif
  }

  C_Polyhedron ls[4];
  ls[3] = ph;
#if NOISY
  print_constraints(ls[3], "*** ls[3] ***");
#endif
  for (int i = 2; i >= 0; --i) {
    ls[i] = ls[i+1];
    // Force generators to be up-to-date, for a change.
    (void) ls[i].generators();
    ls[i].map_space_dimensions(rotate_left);
#if NOISY
    print_constraints(ls[i], "*** ls[i] ***");
#endif
  }

  for (int i = 0; i <= 3; ++i)
    if (rs[i] != ls[i]) {
#if NOISY
      cout << "rs[" << i << "] != ls[" << i << "]" << endl;
#endif
      return 1;
    }

  return 0;
}
CATCH
