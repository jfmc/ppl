/* Testing the function check_universe() for a NNC_polyhedron.
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
main() {
  set_handlers();

  NNC_Polyhedron ph1(4);
  NNC_Polyhedron ph2(3);
  NNC_Polyhedron ph3(3);
  
  ph2.add_constraint(LinExpression(1) > 0);
  ph3.add_constraint(LinExpression(1) < 0);

  ConSys cs;
  NNC_Polyhedron ph4(cs);

#if NOISY
  print_constraints(ph1, "--- ph1 ---");
  print_constraints(ph2, "--- ph2 ---");
  print_constraints(ph3, "--- ph3 ---");
  print_constraints(ph4, "--- ph4 ---");
#endif
  
  bool universe1 = ph1.check_universe();
  
#if NOISY
  cout << "*** ph1.check_universe() ***"
       << endl
       << (universe1 ? "true" : "false")
       << endl;
#endif

  bool universe2 = ph2.check_universe();
  
#if NOISY
  cout << "*** ph2.check_universe() ***"
       << endl
       << (universe2 ? "true" : "false")
       << endl;
#endif
   
  bool universe3 = ph3.check_universe();
  
#if NOISY
  cout << "*** ph3.check_universe() ***"
       << endl
       << (universe3 ? "true" : "false")
       << endl;
#endif

  bool universe4 = ph4.check_universe();

#if NOISY
  cout << "*** ph4.check_universe() ***"
       << endl
       << (universe4 ? "true" : "false")
       << endl;
#endif

  return (universe1 && universe2 && !universe3 && universe4) ? 0 : 1;
}
