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
Concrete_Expression_Type FP_Type =
  Concrete_Expression_Type::floating_point(ANALYZED_FP_FORMAT);

// Tests division by zero.
bool
test01() {
  Floating_Point_Constant<C_Expr> num("3", 2);
  Floating_Point_Constant<C_Expr> den("0", 2);
  Binary_Operator<C_Expr> div(FP_Type, Binary_Operator<C_Expr>::DIV, &num, &den);
  FP_Linear_Form result;
  if (!linearize(div, FP_Interval_Abstract_Store(),
                 FP_Linear_Form_Abstract_Store(), result)) {
    nout << "*** Linearization failed due to division by zero. ***" << endl;
    return true;
  }
  return false;
}

// Tests multiplication by zero.
bool
test02() {
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), FP_Interval(0));
  store.set_interval(Variable(1), FP_Interval(10));
  Floating_Point_Constant<C_Expr> con("5.5", 4);
  Approximable_Reference<C_Expr> var0(FP_Type, 0);
  Approximable_Reference<C_Expr> var1(FP_Type, 1);
  Binary_Operator<C_Expr> dif(FP_Type, Binary_Operator<C_Expr>::SUB, &var1, &con);
  Binary_Operator<C_Expr> mul(FP_Type, Binary_Operator<C_Expr>::MUL, &dif, &var0);
  FP_Linear_Form result;
  linearize(mul, store, FP_Linear_Form_Abstract_Store(), result);

  FP_Linear_Form known_result(compute_absolute_error<FP_Interval>(ANALYZED_FP_FORMAT));

  nout << "*** known_result ***" << endl
       << known_result << endl;
  bool ok = (result == known_result);

  return ok;
}

// Tests linearization of variables in a given linear form abstract store.
bool
test03() {
  FP_Linear_Form_Abstract_Store store;
  Variable A(0);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  store[0] = known_result;
  Approximable_Reference<C_Expr> var(FP_Type, 0);
  FP_Linear_Form result;
  linearize(var, FP_Interval_Abstract_Store(0), store, result);

  nout << "*** known_result ***" << endl
       << known_result << endl;
  bool ok = (result == known_result);

  return ok;
}

// Tests linearization of A + B.
bool
test04() {
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), tmp);
  Approximable_Reference<C_Expr> var0(FP_Type, 0);
  Approximable_Reference<C_Expr> var1(FP_Type, 1);
  Binary_Operator<C_Expr> sum(FP_Type, Binary_Operator<C_Expr>::ADD, &var0, &var1);
  FP_Linear_Form result;
  linearize(sum, store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  Variable B(1);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  FP_Linear_Form rel_err1;
  known_result.relative_error(ANALYZED_FP_FORMAT, rel_err1);
  known_result += rel_err1;
  FP_Linear_Form lb = FP_Linear_Form(B);
  known_result += lb;
  FP_Linear_Form rel_err2;
  lb.relative_error(ANALYZED_FP_FORMAT, rel_err2);
  known_result += rel_err2;
  known_result += compute_absolute_error<FP_Interval>(ANALYZED_FP_FORMAT);

  bool ok = result == known_result;

  return ok;
}

// Tests the linearization of A - B.
bool
test05() {
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), tmp);
  Approximable_Reference<C_Expr> var0(FP_Type, 0);
  Approximable_Reference<C_Expr> var1(FP_Type, 1);
  Binary_Operator<C_Expr> dif(FP_Type, Binary_Operator<C_Expr>::SUB, &var0, &var1);
  FP_Linear_Form result;
  linearize(dif, store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  Variable B(1);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  FP_Linear_Form rel_err1;
  known_result.relative_error(ANALYZED_FP_FORMAT, rel_err1);
  known_result += rel_err1;
  FP_Linear_Form lb = FP_Linear_Form(B);
  lb.negate();
  known_result += lb;
  FP_Linear_Form rel_err2;
  lb.relative_error(ANALYZED_FP_FORMAT, rel_err2);
  known_result += rel_err2;
  known_result += compute_absolute_error<FP_Interval>(ANALYZED_FP_FORMAT);

  bool ok = result == known_result;

  return ok;
}

// Tests linearization of A * B where A in [0, 1] and B in [2, 2].
bool
test06() {
  FP_Interval tmp(0);
  tmp.join_assign(1);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), FP_Interval(2));
  Approximable_Reference<C_Expr> var0(FP_Type, 0);
  Approximable_Reference<C_Expr> var1(FP_Type, 1);
  Binary_Operator<C_Expr> mul(FP_Type, Binary_Operator<C_Expr>::MUL, &var0, &var1);
  FP_Linear_Form result;
  linearize(mul, store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  FP_Interval coeff = FP_Interval(2);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  FP_Linear_Form rel_err;
  known_result.relative_error(ANALYZED_FP_FORMAT, rel_err);
  known_result *= coeff;
  known_result += coeff * rel_err;
  known_result += compute_absolute_error<FP_Interval>(ANALYZED_FP_FORMAT);

  bool ok = (result == known_result);

  return ok;
}

// Tests the linearization of A / B where A = [0, 1] and B = [2, 2].
bool
test07() {
  FP_Interval tmp(0);
  tmp.join_assign(1);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), FP_Interval(2));
  Approximable_Reference<C_Expr> var0(FP_Type, 0);
  Approximable_Reference<C_Expr> var1(FP_Type, 1);
  Binary_Operator<C_Expr> div(FP_Type, Binary_Operator<C_Expr>::DIV, &var0, &var1);
  FP_Linear_Form result;
  linearize(div, store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  FP_Interval coeff = FP_Interval(1 / 2.0);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  FP_Linear_Form rel_err;
  known_result.relative_error(ANALYZED_FP_FORMAT, rel_err);
  known_result *= coeff;
  known_result += rel_err * coeff;
  known_result += compute_absolute_error<FP_Interval>(ANALYZED_FP_FORMAT);

  bool ok = (result == known_result);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
END_MAIN
