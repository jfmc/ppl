/* Try to reproduce a strange behavior observed on MacOs X.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 1
#endif

int
main() TRY {
  ifstream s(SRCDIR "/bug2.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  NNC_Polyhedron ph;
  ph.ascii_load(s);

  const Constraint_System& cs = ph.constraints();
  unsigned num_constraints = 0;
  for (Constraint_System::const_iterator i = cs.begin(), cs_end = cs.end();
       i != cs_end;
       ++i)
    ++num_constraints;
  ph.ascii_dump(cout);
  const Generator_System& gs = ph.generators();
  unsigned num_points = 0;
  for (Generator_System::const_iterator i = gs.begin(), gs_end = gs.end();
       i != gs_end;
       ++i) {
    if (i->type() != Generator::POINT) {
#if NOISY
      cout << "i->type() == " << i->type() << endl;
#endif
      exit(1);
    }
    ++num_points;
  }

  return 0;
}
CATCH
