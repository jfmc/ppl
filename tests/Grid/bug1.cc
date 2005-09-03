/* Assertion-failure producing example found via CHINA.
   Copyright (C) 2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <fstream>

int
main() {
  ifstream s(SRCDIR "/bug1.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  Grid x;
  x.ascii_load(s);

  Constraint_System cs;
  cs.ascii_load(s);

  assert(x.OK());
  assert(cs.OK());

  nout << "Inputs are OK." << endl;

  // Now see the program explode in unexpected and interesting ways.
  x.add_congruences_and_minimize(cs);

  return 0;
}
