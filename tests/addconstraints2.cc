/* Use of the function add_dimensions_and_constraints_lazy.
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

  Variable x(0);
  Variable y(1);
  ConSys c1;
  c1.insert(x >= 0);
  c1.insert(y >= 0);
  c1.insert(x - y >= 0);
  Polyhedron ph(c1);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif
  
  Polyhedron copy_ph = ph;

  ConSys c2;
  c2.insert(x >= 1);
  c2.insert(y >= 1);
  c2.insert(x - y >= -1);

  ph.add_dimensions_and_constraints_lazy(c2);

  copy_ph.add_dimensions_and_embed(2);
  size_t c2_num_rows = c2.num_rows();
  for (size_t i = 0; i < c2_num_rows; i++)
    copy_ph.insert(c2[i] >> 2);

  int retval = (ph == copy_ph) ? 0 : 1;

#if NOISY
  print_constraints(ph, "*** ph ***");
  print_constraints(copy_ph, "*** copy_ph ***");
#endif

  return retval;
}
