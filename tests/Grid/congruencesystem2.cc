/* Test Congruence_System saturate and satisfy member functions.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Congruence_System>


/* If PASS_EXPECTED is true:
     If the first generator in GS saturates/satifies (whichever is
     passed via MEMBER) CGS, then return true, else print an error
     message and return false.

   And, symmetrically, if PASS_EXPECTED is false:
     If the first generator in GS fails to saturates/satifies
     (whichever is passed via MEMBER) CGS, then return false, else
     print an error message and return true.

   FIX If Generator::ascii_dump was public this could take a
   Generator.
*/

bool
fulfils(const Generator_System& gs, const Congruence_System& cgs,
	bool pass_expected = false,
	bool(Congruence_System::*member)(const Generator& g) const
	= &Congruence_System::saturates_all_congruences,
	string verb = "saturate") {
  Generator_System::const_iterator gi = gs.begin();

  if ((cgs.*member)(*gi) == pass_expected)
    return pass_expected;

  nout << *gi << " should ";
  pass_expected || nout << "fail to ";
  nout << verb << " " << cgs << "." << endl
       << "ASCII dump of system holding " << *gi << ":" << endl;
  gs.ascii_dump(nout);
  nout << "ASCII dump of " << cgs << ":" << endl;
  cgs.ascii_dump(nout);

  return pass_expected == false;
}

#define saturates(gs,cgs) fulfils(gs,cgs)
#define satisfies(gs,cgs)						\
  fulfils(gs,cgs,false,							\
	  &Congruence_System::satisfies_all_congruences,		\
	  "satisfy")

inline bool
fails_to_saturate(const Generator_System& gs,
		  const Congruence_System& cgs) {
  return false == fulfils(gs, cgs, true);
}

inline bool
fails_to_satisfy(const Generator_System& gs,
		 const Congruence_System& cgs) {
  return false == fulfils(gs, cgs, true,
			  &Congruence_System::satisfies_all_congruences,
			  "satisfy");
}


/* saturates_all_congruences
   FIX temporary, usually private, find another way
*/

void
test1() {
  nout << "test1:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs0((A - B %= 0) / 7);
  if (find_variation(cgs0))
    exit(1);

  Generator_System gs0;

  /* Points. */

  gs0.insert(point());
  if (fails_to_saturate(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(A + B));
  if (fails_to_saturate(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(A + 2*B));
  if (saturates(gs0, cgs0))
    exit(1);

  /* Closure points.  */

  gs0.clear();
  gs0.insert(closure_point(2*A + B));
  if (saturates(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(closure_point(7*A + 7*B));
  if (fails_to_saturate(gs0, cgs0))
    exit(1);

  /* Rays. */

  gs0.clear();
  gs0.insert(ray(3*A + 3*B));
  if (fails_to_saturate(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(ray(A + 14*B));
  if (saturates(gs0, cgs0))
    exit(1);

  /* Lines.  */

  gs0.clear();
  gs0.insert(line(13*A + 13*B));
  if (fails_to_saturate(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(line(18*A + 14*B));
  if (saturates(gs0, cgs0))
    exit(1);

  return;
}

/* satisfies_all_congruences
   FIX temporary, usually private, find another way
*/

void
test2() {
  nout << "test2:" << endl;

  Variable A(0);
  Variable B(1);

  Congruence_System cgs0((A - B %= 0) / 7);
  if (find_variation(cgs0))
    exit(1);

  Generator_System gs0;

  /* Points. */

  gs0.insert(point());
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(A + B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(A + 2*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(5*A + 2*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(point(5*A - 2*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  /* Closure points.  */

  gs0.clear();
  gs0.insert(closure_point(2*A + B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(closure_point(7*A + 7*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(closure_point(3*A - 4*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  /* Rays. */

  gs0.clear();
  gs0.insert(ray(3*A + 3*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(ray(A + 14*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(ray(- A + 13*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  /* Lines.  */

  gs0.clear();
  gs0.insert(line(13*A + 13*B));
  if (fails_to_satisfy(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(line(18*A + 14*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  gs0.clear();
  gs0.insert(line(14*A - 21*B));
  if (satisfies(gs0, cgs0))
    exit(1);

  return;
}

int
main() TRY {
  set_handlers();

  test1();
  test2();

  return 0;
}
CATCH
