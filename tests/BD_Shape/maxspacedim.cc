/* Test BD_Shape::max_space_dimension().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
#if 0
  BD_Shape<E_Rational> bd1(1);
  BD_Shape<E_NIT<long> > bd2(1);
  BD_Shape<E_NIT<int> > bd3(1);
  BD_Shape<E_NIT<signed char> > bd4(1);
#else
  BD_Shape<Checked_Number<mpq_class, Extended_Number_Policy> > bd1(1);
  BD_Shape<Checked_Number<long, Extended_Number_Policy> > bd2(1);
  BD_Shape<Checked_Number<int, Extended_Number_Policy> > bd3(1);
  BD_Shape<Checked_Number<signed char, Extended_Number_Policy> > bd4(1);
#endif

  dimension_type max_spacedim1 = bd1.max_space_dimension();
  dimension_type max_spacedim2 = bd2.max_space_dimension();
  dimension_type max_spacedim3 = bd3.max_space_dimension();
  dimension_type max_spacedim4 = bd4.max_space_dimension();


#if NOISY
  cout << endl
       << "The maximum space-dimension of a system of bounded differences " << endl
       << "of Rational is: "
       << endl << max_spacedim1 << endl; 

  cout << endl
       << "The maximum space-dimension of a system of bounded differences " << endl
       << "of long: "
       << endl << max_spacedim2 << endl; 

  cout << endl
       << "The maximum space-dimension of a system of bounded differences " << endl
       << "of int: "
       << endl << max_spacedim3 << endl; 

  cout << endl
       << "The maximum space-dimension of a system of bounded differences " << endl
       << "of signed char"
       << endl << max_spacedim4 << endl;
#endif



  if (max_spacedim1 < max_spacedim2) {
#if NOISY
    print_constraints(bd1, "*** bd1 ***");
    print_constraints(bd2, "*** bd2 ***");
#endif
  }

  if (max_spacedim3 < max_spacedim4) {
#if NOISY
    print_constraints(bd3, "*** bd3 ***");
    print_constraints(bd4, "*** bd4 ***");
#endif
  }

  return 0;

}
CATCH
