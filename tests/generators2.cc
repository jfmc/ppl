/* Computing the set of generators of an empty polyhedron.
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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() try {
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x + y >= 2);
  cs.insert(x + y <= 1);
 
  C_Polyhedron ph(cs);

  const GenSys& gs = ph.generators();

#if NOISY
  print_generators(gs, "*** gs ***");
#endif

  if (gs.begin() != gs.end())
    exit(1);
  
  if (!ph.OK())
    exit(1);

  return 0;
}
catch (std::invalid_argument& e) {
#if NOISY
  cout << "invalid_argument: " << e.what() << endl;
#endif
  exit(1);
}
