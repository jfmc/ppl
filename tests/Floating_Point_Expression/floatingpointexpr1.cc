/* Testing class Floating_Point_Expression ad its derivate classes.
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

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;

// Tests division by zero.
bool
test01() {
  Con_FP_Expression* num = new Con_FP_Expression(3, 5);
  Con_FP_Expression* den = new Con_FP_Expression(-1, 1);
  Div_FP_Expression div(num, den);
  try {
    FP_Linear_Form result;
    div.linearize(FP_Interval_Abstract_Store(0),
                  FP_Linear_Form_Abstract_Store(), result);
  }
  catch (Linearization_Failed e) {
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
  Con_FP_Expression* con = new Con_FP_Expression(5, 6);
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Dif_FP_Expression* dif = new Dif_FP_Expression(var1, con);
  Mul_FP_Expression mul(dif, var0);
  FP_Linear_Form result;
  mul.linearize(store, FP_Linear_Form_Abstract_Store(), result);
  FP_Interval kr(-FP_Expression::absolute_error);
  kr.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result(kr);

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
  Var_FP_Expression var(0);
  FP_Linear_Form result;
  var.linearize(FP_Interval_Abstract_Store(0), store, result);

  nout << "*** known_result ***" << endl
       << known_result << endl;
  bool ok = (result == known_result);

  return ok;
}

// Tests the linearization of A + B.
bool
test04() {
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), tmp);
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Sum_FP_Expression sum(var0, var1);
  FP_Linear_Form result;
  sum.linearize(store, FP_Linear_Form_Abstract_Store(), result);
  Variable A(0);
  Variable B(1);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  ANALYZER_FP_FORMAT exp = pow(ANALYZED_FP_FORMAT::BASE,
    -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS));
  tmp = FP_Interval(1);
  tmp -= exp;
  FP_Interval tmp2(1);
  tmp2 += exp;
  tmp.join_assign(tmp2);
  known_result *= tmp;
  known_result += tmp * FP_Linear_Form(B);
  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;

  // FIXME: Computed result should over-approximates the known result.
  FP_Expression::intervalize(result, store, tmp);
  FP_Expression::intervalize(known_result, store, tmp2);
  return tmp.contains(tmp2);
}

// Tests the linearization of A - B.
bool
test05() {
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), tmp);
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Dif_FP_Expression dif(var0, var1);
  FP_Linear_Form result;
  dif.linearize(store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  Variable B(1);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  ANALYZER_FP_FORMAT exp = pow(ANALYZED_FP_FORMAT::BASE,
    -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS));
  tmp = FP_Interval(1);
  tmp -= exp;
  FP_Interval tmp2(1);
  tmp2 += exp;
  tmp.join_assign(tmp2);
  known_result *= tmp;
  known_result -= tmp * FP_Linear_Form(B);
  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;

  // FIXME: Computed result should over-approximates the known result.
  FP_Expression::intervalize(result, store, tmp);
  FP_Expression::intervalize(known_result, store, tmp2);
  return tmp.contains(tmp2);
}

// Tests the linearization of A * B where A = [0, 1] and B = [2, 2].
bool
test06() {
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(0);
  tmp.join_assign(1);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), FP_Interval(2));
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Mul_FP_Expression mul(var0, var1);
  FP_Linear_Form result;
  mul.linearize(store, FP_Linear_Form_Abstract_Store(), result);

  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  ANALYZER_FP_FORMAT exp = pow(ANALYZED_FP_FORMAT::BASE,
    -static_cast<ANALYZER_FP_FORMAT>((ANALYZED_FP_FORMAT::MANTISSA_BITS-1)));
  FP_Interval coeff = FP_Interval(2);
  coeff -= exp;
  FP_Interval coeff2(2);
  coeff2 += exp;
  coeff.join_assign(coeff2);
  FP_Linear_Form known_result =
  FP_Linear_Form(Variable(0));
  known_result *= coeff;
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;

  // FIXME: Computed result should over-approximates the known result.
  FP_Expression::intervalize(result, store, coeff);
  FP_Expression::intervalize(known_result, store, coeff2);
  return coeff.contains(coeff2);
}

// Tests the linearization of A / B where A = [0, 1] and B = [2, 2].
bool
test07() {
  FP_Interval_Abstract_Store store(2);
  FP_Interval tmp(0);
  tmp.join_assign(1);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), FP_Interval(2));
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Div_FP_Expression div(var0, var1);
  FP_Linear_Form result;
  div.linearize(store, FP_Linear_Form_Abstract_Store(), result);

  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  ANALYZER_FP_FORMAT exp = pow(ANALYZED_FP_FORMAT::BASE,
    -static_cast<ANALYZER_FP_FORMAT>((ANALYZED_FP_FORMAT::MANTISSA_BITS+1)));
  FP_Interval coeff = FP_Interval(1 / 2.0);
  coeff -= exp;
  FP_Interval coeff2(1 / 2.0);
  coeff2 += exp;
  coeff.join_assign(coeff2);
  FP_Linear_Form known_result = FP_Linear_Form(Variable(0));
  known_result *= coeff;
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;

  // FIXME: Computed result should over-approximates the known result.
  FP_Expression::intervalize(result, store, coeff);
  FP_Expression::intervalize(known_result, store, coeff2);
  return coeff.contains(coeff2);
}

// Tests the linearization of [1/4, 1/2] * (-A) where A = [1, 10].
bool
test08() {
  FP_Interval_Abstract_Store store(1);
  FP_Interval tmp(1);
  tmp.join_assign(10);
  store.set_interval(Variable(0), tmp);
  Con_FP_Expression* con = new Con_FP_Expression(1 / 4.0, 1 / 2.0);
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Opp_FP_Expression* opp = new Opp_FP_Expression(var0);
  Mul_FP_Expression mul(con, opp);
  FP_Linear_Form result;
  mul.linearize(store, FP_Linear_Form_Abstract_Store(), result);

  Variable A(0);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  ANALYZER_FP_FORMAT exp = pow(ANALYZED_FP_FORMAT::BASE,
    -static_cast<ANALYZER_FP_FORMAT>((ANALYZED_FP_FORMAT::MANTISSA_BITS+1)));
  tmp = FP_Interval(-1 / 2.0);
  tmp -= exp;
  FP_Interval tmp2(-1 / 4.0);
  tmp2 += exp;
  tmp.join_assign(tmp2);
  known_result *= tmp;
  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;

  // FIXME: Computed result should over-approximates the known result.
  FP_Expression::intervalize(result, store, tmp);
  FP_Expression::intervalize(known_result, store, tmp2);
  return tmp.contains(tmp2);
}

// Tests linearization of multiplication by unbounded operands.
bool
test09() {
  ANALYZER_FP_FORMAT max = std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  FP_Interval min = FP_Interval(-FP_Expression::absolute_error);
  min.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result1 = FP_Linear_Form(min);
  Con_FP_Expression* con1 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con2 = new Con_FP_Expression(0, max);
  Sum_FP_Expression* sum  = new Sum_FP_Expression(con1, con2);
  Con_FP_Expression* con3 = new Con_FP_Expression(0, 0);
  Mul_FP_Expression mul(con3, sum);
  FP_Linear_Form result;
  bool ok1 = false;
  try {
    mul.linearize(FP_Interval_Abstract_Store(),
                  FP_Linear_Form_Abstract_Store(), result);
  }
  catch (Linearization_Failed e) {
    nout << "*** Linearization failed due to overflow. ***" << endl;
    ok1 = true;
  }

  FP_Linear_Form known_result2 = FP_Linear_Form(min);
  Con_FP_Expression* con4 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con5 = new Con_FP_Expression(0, max);
  Sum_FP_Expression* sum2 = new Sum_FP_Expression(con4, con5);
  Con_FP_Expression* con6 = new Con_FP_Expression(0, 0);
  Mul_FP_Expression mul2(sum2, con6);
  FP_Linear_Form result2;
  bool ok2 = false;
  try {
    mul2.linearize(FP_Interval_Abstract_Store(),
                   FP_Linear_Form_Abstract_Store(), result2);
  }
  catch (Linearization_Failed e) {
    nout << "*** Linearization failed due to overflow. ***" << endl;
    ok2 = true;
  }

  Con_FP_Expression* con7 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con8 = new Con_FP_Expression(0, max);
  Sum_FP_Expression* sum3 = new Sum_FP_Expression(con7, con8);
  Con_FP_Expression* con9 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con10 = new Con_FP_Expression(0, max);
  Sum_FP_Expression* sum4 = new Sum_FP_Expression(con9, con10);
  Mul_FP_Expression  mul3(sum3, sum4);

  bool ok3 = false;
  try {
    mul3.linearize(FP_Interval_Abstract_Store(),
                   FP_Linear_Form_Abstract_Store(), result2);
  }
  catch (Linearization_Failed e) {
    nout << "*** Linearization failed due to overflow. ***" << endl;
    ok3 = true;
  }

  return ok1 && ok2 && ok3;
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
  DO_TEST(test08);
  DO_TEST(test09);
END_MAIN
