/* Test Polyhedron::ascii_dump() e Polyhedron::ascii_load():
   we test this function in the case that the file does not contain
   the right thing.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <fstream>
#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() {
  set_handlers();

  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A - B >= 0);
  
  ofstream outFile("ascii_dump_load2.dat");
  if (!outFile) {
#if NOISY
    cout << "Connot open the file" << endl;
#endif
    exit(1);
  }
  
  ph.ascii_dump(outFile);
  outFile.seekp(0);
  outFile << "A";
  outFile.close();

  ifstream inFile("ascii_dump_load2.dat");
  if (!inFile) {
#if NOISY
    cout << "Connot open the file" << endl;
#endif
    exit(1);
  }

  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(inFile);
  inFile.close();
 
  return ok ? 0 : 1;
}
