/* Test Congruence_System::satisfies_all_congruences().
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

using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

class Test_Congruence_System : public Congruence_System {
public:
  Test_Congruence_System() : Congruence_System() {}
  Test_Congruence_System(Congruence_System cgs) : Congruence_System(cgs) {}
  Test_Congruence_System(Congruence cg) : Congruence_System(cg) {}
  bool
  satisfies_all_congruences(const Grid_Generator& g) const {
    return Congruence_System::satisfies_all_congruences(g);
  }
};

/* If PASS_EXPECTED is true:
     If the first generator in GS satifies CGS, then return true, else
     print an error message and return false.

   And, symmetrically, if PASS_EXPECTED is false:
     If the first generator in GS fails to satify CGS, then return
     false, else print an error message and return true.

   FIXME: If Generator::ascii_dump was public this could take a
          Generator.
*/

bool
fulfils(const Grid_Generator_System& gs,
	const Test_Congruence_System& cgs,
	bool pass_expected = false) {
  Grid_Generator_System::const_iterator gi = gs.begin();

  if (cgs.satisfies_all_congruences(*gi) == pass_expected)
    return pass_expected;

  nout << *gi << " should";
  pass_expected || nout << "fail to";
  nout << " satisfy " << cgs << "." << endl
       << "ASCII dump of system holding " << *gi << ":" << endl;
  gs.ascii_dump(nout);
  nout << "ASCII dump of " << cgs << ":" << endl;
  cgs.ascii_dump(nout);

  return !pass_expected;
}

#define satisfies fulfils

inline bool
fails_to_satisfy(const Grid_Generator_System& gs,
		 const Congruence_System& cgs) {
  if (fulfils(gs, cgs, true))
    return false;
  return true;
}

Variable A(0);
Variable B(1);

// Divisor of 1.

void
test1() {
  Test_Congruence_System cgs0((A - B %= 0) / 7);
  if (find_variation(cgs0))
    exit(1);

  Grid_Generator_System gs0;

  // Points.

  gs0.insert(grid_point());
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(A + B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(A + 2*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(5*A + 2*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(5*A - 2*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  // Parameters.

  gs0.clear();
  gs0.insert(parameter(3*A + 3*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(parameter(A + 14*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(parameter(-A + 13*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  // Lines.

  gs0.clear();
  gs0.insert(grid_line(13*A + 13*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_line(18*A + 14*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_line(14*A - 21*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  cgs0.clear();
  cgs0.insert((A %= 0) / 2);
  gs0.clear();
  gs0.insert(grid_line(3*A));
  if (satisfies(gs0, cgs0))
    exit(1);

  return;
}

// Divisors greater than 1.

void
test2() {
  Test_Congruence_System cgs0((A - B %= 0) / 7);
  if (find_variation(cgs0))
    exit(1);

  Grid_Generator_System gs0;

  // Points.

  gs0.clear();
  gs0.insert(grid_point(A + B, 3));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(A + 2*B, 3));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(5*A + 2*B, 5));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_point(5*A - 2*B, 7));
  if (satisfies(gs0, cgs0))
    exit(1);

  // Parameters.

  gs0.clear();
  gs0.insert(parameter(3*A + 3*B, 4));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(parameter(A + 14*B, 5));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(parameter(-A + 13*B, 2));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  // Lines.

  gs0.clear();
  gs0.insert(grid_line(13*A + 13*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_line(18*A + 14*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(grid_line(14*A - 21*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  cgs0.clear();
  cgs0.insert((A %= 0) / 2);
  gs0.clear();
  gs0.insert(grid_line(3*A));
  if (satisfies(gs0, cgs0))
    exit(1);

  return;
}

} // namespace

int
main() TRY {
  set_handlers();

  DO_TEST(test1);
  DO_TEST(test2);

  return 0;
}
CATCH
