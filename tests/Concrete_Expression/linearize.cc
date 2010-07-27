/* Testing linearization algorithm ad its related functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or moDif_FP_Expressiony it
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
#include "C_Expr.defs.hh"

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;

// Tests division by zero.
bool
test01() {
  Floating_Point_Constant<C_Expr> num("3", 2);
  Floating_Point_Constant<C_Expr> den("0", 2);
  Binary_Operator<C_Expr> div(Binary_Operator<C_Expr>::DIV, &num, &den);
  FP_Linear_Form result;
  if (!linearize(div, FP_Interval_Abstract_Store(),
                 FP_Linear_Form_Abstract_Store(), result)) {
    nout << "*** Linearization failed due to division by zero. ***" << endl;
    return true;
  }
  return false;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
