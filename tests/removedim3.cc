/* Remove some variables from the space.
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

int
main() {
  set_handlers();

  Variable y(1);
  Variable z(2);
  Variable w(6);

  // This is the set of the variables that we want to remove.
  set<Variable> to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  to_be_removed.insert(w);

  // A 10-dim space, empty polyhedron.
  Polyhedron ph(10, Polyhedron::EMPTY);  
  ph.remove_dimensions(to_be_removed);

  // A 7-dim space, empty polyhedron.
  Polyhedron known_result(7, Polyhedron::EMPTY);

  int retval = (known_result == ph) ? 0 : 1;

#if NOISY
  cout << "*** ph ***"
       << endl
       << ph
       << endl;

  cout << "*** known_result ***"
       << endl
       << known_result
       << endl;
#endif

  return retval;
}
