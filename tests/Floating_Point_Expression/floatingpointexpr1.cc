/* Testing class Floating_Point_Expression ad its derivate classes.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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
  FP_Interval_Abstract_Store store_fl(2);
  store_fl.set_interval(Variable(0), FP_Interval(0));
  store_fl.set_interval(Variable(1), FP_Interval(10));
  Con_FP_Expression* con_fl = new Con_FP_Expression(5, 6);
  Var_FP_Expression* var0_fl = new Var_FP_Expression(0);
  Var_FP_Expression* var1_fl = new Var_FP_Expression(1);
  Dif_FP_Expression* dif_fl = new Dif_FP_Expression(var1_fl, con_fl);
  Mul_FP_Expression mul_fl(dif_fl, var0_fl);
  FP_Linear_Form result_fl;
  mul_fl.linearize(store_fl, FP_Linear_Form_Abstract_Store(), result_fl);
  FP_Interval kr_fl(-FP_Expression::absolute_error);
  kr_fl.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result_fl(kr_fl);

  nout << "*** known_result_fl ***" << endl
       << known_result_fl << endl;
  bool ok_fl = (result_fl == known_result_fl);

  FP_Interval_Abstract_Store store_db(2);
  store_db.set_interval(Variable(0), FP_Interval(0));
  store_db.set_interval(Variable(1), FP_Interval(4));
  Con_FP_Expression* con_db  = new Con_FP_Expression(5, 6);
  Var_FP_Expression* var0_db = new Var_FP_Expression(0);
  Var_FP_Expression* var1_db = new Var_FP_Expression(1);
  Sum_FP_Expression* sum_db  = new Sum_FP_Expression(con_db, var1_db);
  Mul_FP_Expression mul_db(var0_db, sum_db);
  FP_Linear_Form result_db;
  mul_db.linearize(store_db, FP_Linear_Form_Abstract_Store(), result_db);

  FP_Interval kr_db(-FP_Expression::absolute_error);
  kr_db.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result_db(kr_db);

  nout << "*** known_result_db ***" << endl
       << known_result_db << endl;
  bool ok_db = (result_db == known_result_db);

  return ok_fl && ok_db;
}

// Tests linearization of variables in a given linear form abstract store.
bool
test03() {
  FP_Linear_Form_Abstract_Store store_fl;
  Variable A(0);
  FP_Linear_Form known_result_fl = FP_Linear_Form(A);
  store_fl[0] = known_result_fl;
  Var_FP_Expression var_fl(0);
  FP_Linear_Form result_fl;
  var_fl.linearize(FP_Interval_Abstract_Store(0), store_fl, result_fl);

  nout << "*** known_result_fl ***" << endl
       << known_result_fl << endl;
  bool ok_fl = (result_fl == known_result_fl);

  FP_Linear_Form_Abstract_Store store_db;
  FP_Linear_Form known_result_db =
  FP_Linear_Form(A);
  store_db[0] = known_result_db;
  Var_FP_Expression var_db(0);
  FP_Linear_Form result_db;
  var_db.linearize(FP_Interval_Abstract_Store(0), store_db, result_db);

  nout << "*** known_result_db ***" << endl
       << known_result_db << endl;
  bool ok_db = (result_db == known_result_db);

  return ok_fl && ok_db;
}

// Tests the linearization of A + B.
bool
test04() {
  FP_Interval tmp;
  FP_Interval_Abstract_Store store(2);
  store.set_interval(Variable(0), tmp);
  store.set_interval(Variable(1), tmp);
  Var_FP_Expression* var0 = new Var_FP_Expression(0);
  Var_FP_Expression* var1 = new Var_FP_Expression(1);
  Sum_FP_Expression sum(var0, var1);
  FP_Linear_Form result;
  sum.linearize(store, FP_Linear_Form_Abstract_Store(), result);
  nout << result << endl;
  Variable A(0);
  Variable B(1);
  FP_Linear_Form known_result = FP_Linear_Form(A);
  ANALYZER_FP_FORMAT exp = pow(2,
    -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS));
  nout << exp << endl;
  tmp = FP_Interval(1);
  tmp -= exp;
  FP_Interval tmp2(1);
  tmp2 += exp;
  tmp.join_assign(tmp2);
  nout << tmp << endl;
  known_result *= tmp;
  known_result += tmp * FP_Linear_Form(B);
  nout << FP_Expression::absolute_error << endl;
  tmp = FP_Interval(-FP_Expression::absolute_error);
  tmp.join_assign(FP_Expression::absolute_error);
  known_result += tmp;

  nout << "*** known_result ***" << endl
       << known_result << endl;
  return result == known_result;
}

// Tests the linearization of A - B.
bool
test05() {
  FP_Interval tmp;
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
  ANALYZER_FP_FORMAT exp = pow(2,
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
  return result == known_result;
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
  ANALYZER_FP_FORMAT exp = pow(2,
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
  return result == known_result;
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
  ANALYZER_FP_FORMAT exp = pow(2,
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
  return result == known_result;
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
  ANALYZER_FP_FORMAT exp = pow(2,
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
  return result == known_result;
}

// Tests linearization of multiplication by unbounded operands.
bool
test09() {
  ANALYZER_FP_FORMAT max_fl = std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  FP_Interval min_fl = FP_Interval(
                     -FP_Expression::absolute_error);
  min_fl.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result1 = FP_Linear_Form(min_fl);
  Con_FP_Expression* con1 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con2 = new Con_FP_Expression(0, max_fl);
  Sum_FP_Expression* sum  = new Sum_FP_Expression(con1, con2);
  Con_FP_Expression* con3 = new Con_FP_Expression(0, 0);
  Mul_FP_Expression mul(con3, sum);
  FP_Linear_Form result;
  mul.linearize(FP_Interval_Abstract_Store(),
                FP_Linear_Form_Abstract_Store(), result);

  nout << "*** known_result1 ***" << endl
       << known_result1 << endl;
  bool ok1 = (known_result1 == result);

  ANALYZER_FP_FORMAT max_db = std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  FP_Interval min_db = FP_Interval(
                     -FP_Expression::absolute_error);
  min_db.join_assign(FP_Expression::absolute_error);
  FP_Linear_Form known_result2 = FP_Linear_Form(min_db);
  Con_FP_Expression* con4 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con5 = new Con_FP_Expression(0, max_db);
  Sum_FP_Expression* sum2 = new Sum_FP_Expression(con4, con5);
  Con_FP_Expression* con6 = new Con_FP_Expression(0, 0);
  Mul_FP_Expression mul2(sum2, con6);
  FP_Linear_Form result2;
  mul2.linearize(FP_Interval_Abstract_Store(),
                 FP_Linear_Form_Abstract_Store(), result2);

  nout << "*** known_result2 ***" << endl
       << known_result2 << endl;
  bool ok2 = (known_result2 == result2);

  Con_FP_Expression* con7 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con8 = new Con_FP_Expression(0, max_db);
  Sum_FP_Expression* sum3 = new Sum_FP_Expression(con7, con8);
  Con_FP_Expression* con9 = new Con_FP_Expression(0, 0);
  Con_FP_Expression* con10 = new Con_FP_Expression(0, max_db);
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
