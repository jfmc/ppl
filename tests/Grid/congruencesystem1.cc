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

// Public constructors.

bool
test01() {
  Test_Congruence_System cgs0;

  print_congruences(cgs0, "*** cgs0 ***");

  Test_Congruence_System cgs1(cgs0);

  bool ok = (cgs1 == cgs0);

  print_congruences(cgs1, "*** cgs1(cgs0) ***");

  if (ok) {
    Variable A(0);
    Variable B(1);

    Test_Congruence_System cgs2((A - 3*B %= 5) / 2);

    ok &= !(cgs2 == cgs0) && !(cgs2 == cgs1);

    print_congruences(cgs2, "*** cgs2((A - 3*B %= 5) / 2) ***");
  };

  return ok;
}

// operator=

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence_System cgs0;

  Test_Congruence_System cgs1((A - 3*B + 21*C %= 5) / 2);

  cgs0 = cgs1;

  bool ok = (cgs0 == cgs1);

  print_congruences(cgs0, "*** cgs0 ***");

  return ok;
}

// space_dimension

bool
test03() {
  Variable A(3);
  Variable B(7);
  Variable C(4);

  Test_Congruence_System cgs0((A - 3*B + 21*C %= 55) / 21);

  bool ok = (cgs0.space_dimension() == 8);

  print_congruences(cgs0, "*** cgs0((A - 3*B + 21*C %= 55) / 21) ***");

  return ok;
}

// clear

bool
test04() {
  Variable A(0);
  Variable B(1);

  Test_Congruence_System cgs0((A - 3*B %= 5) / 7);

  print_congruences(cgs0, "*** cgs0 ***");

  cgs0.clear();

  bool ok = (cgs0.space_dimension() == 0);

  print_congruences(cgs0, "*** cgs0.clear() ***");

  return ok;
}

// insert

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);

  Test_Congruence_System cgs0((A - 3*B + C - 18*D %= 5) / 7);
  cgs0.insert((A %= 0) / 3);
  cgs0.insert((A + B %= 3) / 5);

  dimension_type tem = cgs0.num_proper_congruences();
  bool ok = (tem == 3);

  print_congruences(cgs0, "*** cgs0.insert ***");

  return ok;
}

// num_[non_]equalities

bool
test06() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence_System cgs0((-A - B + 13*C %= 6) / 7);
  cgs0.insert((A %= 0) / 0);
  cgs0.insert((A + B %= 3) / 0);

  dimension_type neqs = cgs0.num_equalities();
  dimension_type npcgs = cgs0.num_proper_congruences();
  bool ok = (neqs == 2 && npcgs == 1);

  print_congruences(cgs0, "*** cgs0.insert ***");

  return ok;
}

// insert, including a row with all terms zero and an equality.

bool
test07() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Test_Congruence_System cgs0(0*A + 0*B %= -1);
  cgs0.insert((A + 0*B %= 1) / 0);
  cgs0.insert(0*A + 1*B %= 1);

  dimension_type neqs = cgs0.num_equalities();
  dimension_type npcgs = cgs0.num_proper_congruences();
  bool ok = (neqs == 1 && npcgs == 2);

  print_congruences(cgs0, "*** cgs0.insert ***");

  return ok;
}

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

// Divisor of 1.

bool
test08() {
  Variable A(0);
  Variable B(1);

  Test_Congruence_System cgs0((A - B %= 0) / 7);
  print_congruences(cgs0, "*** cgs0((A - B %= 0) / 7) ***");

  Grid_Generator_System gs0;

  // Points.

  gs0.insert(grid_point());
  bool ok = (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0, "*** gs0.insert(grid_point()) ***");

  gs0.clear();
  gs0.insert(grid_point(A + B));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(A + B)) ***");

  gs0.clear();
  gs0.insert(grid_point(A + 2*B));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(A + 2*B)) ***");

  gs0.clear();
  gs0.insert(grid_point(5*A + 2*B));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(5*A + 2*B)) ***");

  gs0.clear();
  gs0.insert(grid_point(5*A - 2*B));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(5*A - 2*B)) ***");

  // Parameters.

  gs0.clear();
  gs0.insert(parameter(3*A + 3*B));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(3*A + 3*B)) ***");

  gs0.clear();
  gs0.insert(parameter(A + 14*B));
  ok &= (satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(A + 14*B)) ***");

  gs0.clear();
  gs0.insert(parameter(-A + 13*B));
  ok = (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(-A + 13*B)) ***");

  // Lines.

  gs0.clear();
  gs0.insert(grid_line(13*A + 13*B));
  ok = (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(13*A + 13*B)) ***");

  gs0.clear();
  gs0.insert(grid_line(18*A + 14*B));
  ok = (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(18*A + 14*B)) ***");

  gs0.clear();
  gs0.insert(grid_line(14*A - 21*B));
  ok = (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(14*A - 21*B)) ***");

  cgs0.clear();
  cgs0.insert((A %= 0) / 2);
  gs0.clear();
  gs0.insert(grid_line(3*A));
  ok = (!satisfies(gs0, cgs0));
  print_congruences(cgs0,
    "*** cgs0.clear(); cgs0.insert((A %= 0) / 2) ***");
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(3*A)) ***");

  return ok;
}

// Divisors greater than 1.

bool
test09() {
  Variable A(0);
  Variable B(1);

  Test_Congruence_System cgs0((A - B %= 0) / 7);
  print_congruences(cgs0,
    "*** cgs0((A - B %= 0) / 7) ***");

  Grid_Generator_System gs0;

  // Points.
  gs0.clear();
  gs0.insert(grid_point(A + B, 3));
  bool ok = (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(A + B, 3)) ***");

  gs0.clear();
  gs0.insert(grid_point(A + 2*B, 3));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(A + 2*B, 3)) ***");

  gs0.clear();
  gs0.insert(grid_point(5*A + 2*B, 5));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(5*A + 2*B, 5)) ***");

  gs0.clear();
  gs0.insert(grid_point(5*A - 2*B, 7));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_point(5*A - 2*B, 7)) ***");

  // Parameters.

  gs0.clear();
  gs0.insert(parameter(3*A + 3*B, 4));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(3*A + 3*B, 4)) ***");

  gs0.clear();
  gs0.insert(parameter(A + 14*B, 5));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(A + 14*B, 5)) ***");

  gs0.clear();
  gs0.insert(parameter(-A + 13*B, 2));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(parameter(-A + 13*B, 2)) ***");

  // Lines.

  gs0.clear();
  gs0.insert(grid_line(13*A + 13*B));
  ok &= (!fails_to_satisfy(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(13*A + 13*B)) ***");

  gs0.clear();
  gs0.insert(grid_line(18*A + 14*B));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(18*A + 14*B)) ***");

  gs0.clear();
  gs0.insert(grid_line(14*A - 21*B));
  ok &= (!satisfies(gs0, cgs0));
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(14*A - 21*B)) ***");

  cgs0.clear();
  cgs0.insert((A %= 0) / 2);
  gs0.clear();
  gs0.insert(grid_line(3*A));
  ok &= (!satisfies(gs0, cgs0));
  print_congruences(cgs0,
    "*** cgs0.clear(); cgs0.insert((A %= 0) / 2) ***");
  print_generators(gs0,
    "*** gs0.clear(); gs0.insert(grid_line(3*A)) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
  NEW_TEST(test02);
  NEW_TEST(test03);
  NEW_TEST(test04);
  NEW_TEST(test05);
  NEW_TEST(test06);
  NEW_TEST(test07);
  NEW_TEST(test08);
  NEW_TEST(test09);
END_MAIN
