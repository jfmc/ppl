/* Test Direct_Product<NNC_Polyhedron, Grid>.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

using namespace Parma_Polyhedra_Library::IO_Operators;

// #define PH_IS_NNC
// #define PH_IS_FIRST
#define PH_IS_BOX

#ifdef PH_IS_BOX
typedef TBox Poly;
#else
#ifdef PH_IS_NNC
typedef NNC_Polyhedron Poly;
#else
typedef C_Polyhedron Poly;
#endif
#endif

#ifdef PH_IS_FIRST
typedef Domain_Product<Poly, Grid>::Direct_Product DProduct;
typedef Domain_Product<Poly, Grid>::Smash_Product SProduct;
typedef Domain_Product<Poly, Grid>::Constraints_Product CProduct;
#else
typedef Domain_Product<Grid, Poly>::Direct_Product DProduct;
typedef Domain_Product<Grid, Poly>::Smash_Product SProduct;
typedef Domain_Product<Grid, Poly>::Constraints_Product CProduct;
#endif

namespace {

// remove_higher_dimensions()
// The initial product is empty with 1 space dimension
// and this dimension is removed.
bool
test01() {
  Variable A(0);

  SProduct sp1(1);
  CProduct cp1(1);
  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(A <= 0);
  sp1.add_constraints(cs);
  cp1.add_constraints(cs);
  SProduct sp2(sp1);
  CProduct cp2(cp1);

  bool ok1s = (sp1 == sp2 && sp2.is_empty());
  if (!ok1s || !sp1.OK()) {
    print_congruences(sp1, "*** after == check: sp1 congruences ***");
    print_constraints(sp1, "*** after == check: sp1 constraints ***");
    return false;
  }

  bool ok1 = (cp1 == cp2 && cp2.is_empty());
  if (!ok1 || !cp1.OK()) {
    print_congruences(cp1, "*** after == check: cp1 congruences ***");
    print_constraints(cp1, "*** after == check: cp1 constraints ***");
    return false;
  }

  sp1.remove_higher_space_dimensions(0);

  if (!sp1.OK()) {
    print_congruences(sp1, "*** remove all dimensions: sp1 congruences ***");
    print_constraints(sp1, "*** remove all dimensions: sp1 constraints ***");
    return false;
  }

  cp1.remove_higher_space_dimensions(0);

  bool ok = cp1.OK();

  print_congruences(cp1, "*** remove all dimensions: cp1 congruences ***");
  print_constraints(cp1, "*** remove all dimensions: cp1 constraints ***");

  return ok;
}

// upper_bound_assign(cp2)
// The first product is empty and the second a single point in 1D
bool
test02() {
  Variable A(0);

  SProduct sp1(1);
  CProduct cp1(1);
  Constraint_System cs1;
  cs1.insert(A >= 1);
  cs1.insert(A <= 0);
  sp1.add_constraints(cs1);
  cp1.add_constraints(cs1);

  SProduct sp2(1);
  CProduct cp2(1);
  Constraint_System cs2;
  cs2.insert(A == 1);
  sp2.add_constraints(cs2);
  cp2.add_constraints(cs2);

  SProduct sp1_copy(sp1);
  CProduct cp1_copy(cp1);

  sp1.upper_bound_assign(sp2);
  cp1.upper_bound_assign(cp2);

  if (!sp1.OK()) {
    print_congruences(sp1, "*** after ok check: sp1 congruences ***");
    print_constraints(sp1, "*** after ok check: sp1 constraints ***");
    return false;
  }

  bool ok = cp1.OK();

  print_congruences(cp1, "*** after OK() check: cp1 congruences ***");
  print_constraints(cp1, "*** after OK() check: cp1 constraints ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
