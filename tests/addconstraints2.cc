/* Use of the function add_dimensions_and_constraints.
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
  set_handlers();

  Variable x(0);
  Variable y(1);
  ConSys cs1;
  cs1.insert(x >= 0);
  cs1.insert(y >= 0);
  cs1.insert(x - y >= 0);
  Polyhedron ph(cs1);

#if NOISY
  print_constraints(ph, "*** ph before ***");
#endif

  ConSys cs2;
  cs2.insert(x >= 1);
  cs2.insert(y >= 1);
  cs2.insert(x - y >= -1);

  // Making copies.
  Polyhedron copy_ph = ph;
  ConSys copy_cs2 = cs2;

  ph.add_dimensions_and_constraints(cs2);

  copy_ph.add_dimensions_and_embed(2);
  for (ConSys::const_iterator i = copy_cs2.begin(),
	 iend = copy_cs2.end(); i != iend; ++i )
    copy_ph.insert(*i >> 2);

  int retval = (ph == copy_ph) ? 0 : 1;

#if NOISY
  print_constraints(ph, "*** add_dimensions_and_constraints ***");
  print_constraints(copy_ph, "*** embed + renaming + insert ***");
#endif

  return retval;
}
catch (std::exception& e) {
  cout << e.what() << endl;
  exit(1);
}
