/* SEND + MORE = MONEY.
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
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

// The classic cryptarithmetic puzzle:
//
//      S E N D
//    + M O R E
//    ---------
//    M O N E Y

void
less_than(Polyhedron& ph, Variable X, Variable Y) {
  ph.insert(X+1 <= Y);
}

void
constraints(Polyhedron& ph,
	    Variable S,
	    Variable E,
	    Variable N,
	    Variable D,
	    Variable M,
	    Variable O,
	    Variable R,
	    Variable Y,
	    int C1,
	    int C2,
	    int C3,
	    int C4) {
  ph.insert(S >= 0);
  ph.insert(E >= 0);
  ph.insert(N >= 0);
  ph.insert(D >= 0);
  ph.insert(M >= 0);
  ph.insert(O >= 0);
  ph.insert(R >= 0);
  ph.insert(Y >= 0);
  ph.insert(S <= 9);
  ph.insert(E <= 9);
  ph.insert(N <= 9);
  ph.insert(D <= 9);
  ph.insert(M <= 9);
  ph.insert(O <= 9);
  ph.insert(R <= 9);
  ph.insert(Y <= 9);
  ph.insert(S >= 1);
  ph.insert(M >= 1);
  ph.insert(M == C1);
  ph.insert(C2 + S + M == O + C1 * 10);
  ph.insert(C3 + E + O == N + 10 * C2);
  ph.insert(C4 + N + R == E + 10 * C3);
  ph.insert(D + E == Y + 10*C4);

  less_than(ph, O, M);
  less_than(ph, M, Y);
  less_than(ph, Y, E);
  less_than(ph, E, N);
  less_than(ph, N, D);
  less_than(ph, D, R);
  less_than(ph, R, S);
}

int
main() try {
  set_handlers();

  Variable S(0);
  Variable E(1);
  Variable N(2);
  Variable D(3);
  Variable M(4);
  Variable O(5);
  Variable R(6);
  Variable Y(7);

  bool solution_found = false;

  for (int C1 = 0; C1 <= 1; ++C1)
    for (int C2 = 0; C2 <= 1; ++C2)
      for (int C3 = 0; C3 <= 1; ++C3)
	for (int C4 = 0; C4 <= 1; ++C4) {
	  Polyhedron ph(8);
	  constraints(ph,
		      S, E, N, D, M, O, R, Y,
		      C1, C2, C3, C4);
	  if (!ph.check_empty()) {
#if NOISY
	    cout << "Solution constraints" << endl;
	    const ConSys& cs = ph.constraints();
	    copy(cs.begin(), cs.end(),
		 ostream_iterator<Constraint>(cout, "\n"));
	    cout << "Solution generators" << endl;
	    const GenSys& gs = ph.generators();
	    copy(gs.begin(), gs.end(),
		 ostream_iterator<Generator>(cout, "\n"));
#endif
	    if (solution_found)
	      return 1;

	    solution_found = true;

	    Polyhedron expected(8);
	    expected.insert(S == 9);
	    expected.insert(E == 5);
	    expected.insert(N == 6);
	    expected.insert(D == 7);
	    expected.insert(M == 1);
	    expected.insert(O == 0);
	    expected.insert(R == 8);
	    expected.insert(Y == 2);

	    if (ph != expected)
	      return 1;
	  }
	}
  return 0;
}
catch (std::exception& e) {
  cout << e.what() << endl;
  exit(1);
}

