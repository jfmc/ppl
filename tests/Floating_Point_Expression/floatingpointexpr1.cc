/* Testing class Floating_Point_Expression ad its derivate classes.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

typedef Linear_Form<fl_r_oc> Float_Interval_Linear_Form;
typedef Linear_Form<db_r_oc> Double_Interval_Linear_Form;

typedef Division_Floating_Point_Expression<fl_r_oc, IEEE754_Single> div_fpess;
typedef Division_Floating_Point_Expression<fl_r_oc, IEEE754_Double> div_fpesd;
typedef Difference_Floating_Point_Expression<fl_r_oc, IEEE754_Single> dif_fpess;
typedef Difference_Floating_Point_Expression<fl_r_oc, IEEE754_Double> dif_fpesd;
typedef Multiplication_Floating_Point_Expression<fl_r_oc, IEEE754_Single>
mul_fpess;
typedef Multiplication_Floating_Point_Expression<fl_r_oc, IEEE754_Double>
mul_fpesd;
typedef Sum_Floating_Point_Expression<fl_r_oc, IEEE754_Single> sum_fpess;
typedef Sum_Floating_Point_Expression<fl_r_oc, IEEE754_Double> sum_fpesd;
typedef Constant_Floating_Point_Expression<fl_r_oc, IEEE754_Single> con_fpess;
typedef Constant_Floating_Point_Expression<fl_r_oc, IEEE754_Double> con_fpesd;
typedef Variable_Floating_Point_Expression<fl_r_oc, IEEE754_Single> var_fpess;
typedef Variable_Floating_Point_Expression<fl_r_oc, IEEE754_Double> var_fpesd;
typedef Opposite_Floating_Point_Expression<fl_r_oc, IEEE754_Single> opp_fpess;
typedef Opposite_Floating_Point_Expression<fl_r_oc, IEEE754_Double> opp_fpesd;

typedef Division_Floating_Point_Expression<db_r_oc, IEEE754_Single> div_fpeds;
typedef Division_Floating_Point_Expression<db_r_oc, IEEE754_Double> div_fpedd;
typedef Difference_Floating_Point_Expression<db_r_oc, IEEE754_Single> dif_fpeds;
typedef Difference_Floating_Point_Expression<db_r_oc, IEEE754_Double> dif_fpedd;
typedef Multiplication_Floating_Point_Expression<db_r_oc, IEEE754_Single>
mul_fpeds;
typedef Multiplication_Floating_Point_Expression<db_r_oc, IEEE754_Double>
mul_fpedd;
typedef Sum_Floating_Point_Expression<db_r_oc, IEEE754_Single> sum_fpeds;
typedef Sum_Floating_Point_Expression<db_r_oc, IEEE754_Double> sum_fpedd;
typedef Constant_Floating_Point_Expression<db_r_oc, IEEE754_Single> con_fpeds;
typedef Constant_Floating_Point_Expression<db_r_oc, IEEE754_Double> con_fpedd;
typedef Variable_Floating_Point_Expression<db_r_oc, IEEE754_Single> var_fpeds;
typedef Variable_Floating_Point_Expression<db_r_oc, IEEE754_Double> var_fpedd;
typedef Opposite_Floating_Point_Expression<db_r_oc, IEEE754_Single> opp_fpeds;
typedef Opposite_Floating_Point_Expression<db_r_oc, IEEE754_Double> opp_fpedd;

typedef Floating_Point_Expression<fl_r_oc, IEEE754_Single>::FP_Interval_Abstract_Store sstr;
typedef Constant_Floating_Point_Expression<db_r_oc, IEEE754_Single>::FP_Interval_Abstract_Store dstr;

typedef Floating_Point_Expression<fl_r_oc, IEEE754_Single>::FP_Linear_Form_Abstract_Store lsstr;
typedef Constant_Floating_Point_Expression<db_r_oc, IEEE754_Single>::FP_Linear_Form_Abstract_Store ldstr;

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;

bool
test01() {
  nout << std::numeric_limits<float>::denorm_min() << endl;
  nout << div_fpess::absolute_error << endl;
  nout << div_fpesd::absolute_error << endl;
  nout << div_fpeds::absolute_error << endl;
  if (div_fpess::absolute_error != std::numeric_limits<float>::denorm_min())
    return false;

  if (div_fpesd::absolute_error != std::numeric_limits<float>::denorm_min())
    return false;

  if (div_fpeds::absolute_error != std::numeric_limits<float>::denorm_min())
    return false;

  return true;
}

bool
test02() {
  nout << std::numeric_limits<double>::denorm_min() << endl;
  nout << div_fpedd::absolute_error << endl;
  if (div_fpedd::absolute_error != std::numeric_limits<double>::denorm_min())
    return false;

  return true;
}

bool
test03() {
  con_fpess* num = new con_fpess(3, 5);
  con_fpess* den = new con_fpess(-1, 1);
  div_fpess div(num, den);
  try {
    Float_Interval_Linear_Form result;
    div.linearize(sstr(), lsstr(), result);
  }
  catch (Linearization_Failed e) {
    return true;
  }
  return false;
}

bool
test04() {
  sstr store;
  store[0] = fl_r_oc(0);
  store[1] = fl_r_oc(10);
  con_fpess* con = new con_fpess(5, 6);
  var_fpess* var0 = new var_fpess(0);
  var_fpess* var1 = new var_fpess(1);
  dif_fpess* dif = new dif_fpess(var1, con);
  mul_fpess mul(dif, var0);
  Float_Interval_Linear_Form result;
  mul.linearize(store, lsstr(), result);
  nout << result << endl;
  fl_r_oc kr(-std::numeric_limits<float>::denorm_min());
  kr.join_assign(std::numeric_limits<float>::denorm_min());
  Float_Interval_Linear_Form known_result(kr);
  nout << "*** known_result ***" << endl
       << known_result << endl;
  return result == known_result;
}

bool
test05() {
  dstr store;
  store[0] = db_r_oc(0);
  store[1] = db_r_oc(4);
  con_fpedd* con = new con_fpedd(5, 6);
  var_fpedd* var0 = new var_fpedd(0);
  var_fpedd* var1 = new var_fpedd(1);
  sum_fpedd* sum = new sum_fpedd(con, var1);
  mul_fpedd mul(var0, sum);
  Double_Interval_Linear_Form result;
  mul.linearize(store, ldstr(), result);
  nout << result << endl;
  db_r_oc kr(-std::numeric_limits<double>::denorm_min());
  kr.join_assign(std::numeric_limits<double>::denorm_min());
  Double_Interval_Linear_Form known_result(kr);
  nout << "*** known_result ***" << endl
       << known_result << endl;
  return result == known_result;
}

bool
test06() {
  sstr store;
  fl_r_oc tmp = fl_r_oc(0);
  tmp.join_assign(1);
  store[0] = tmp;
  store[1] = fl_r_oc(2);
  var_fpess* var0 = new var_fpess(0);
  var_fpess* var1 = new var_fpess(1);
  mul_fpess mul(var0, var1);
  Float_Interval_Linear_Form result;
  mul.linearize(store, lsstr(), result);
  tmp = fl_r_oc(-std::numeric_limits<float>::denorm_min());
  tmp.join_assign(std::numeric_limits<float>::denorm_min());
  float exp = pow(2, -22);
  fl_r_oc coeff = fl_r_oc(-exp);
  coeff.join_assign(exp);
  coeff += fl_r_oc(2);
  Float_Interval_Linear_Form known_result =
  Float_Interval_Linear_Form(Variable(0));
  known_result *= coeff;
  known_result += tmp;
  nout << "*** known_result ***" << endl
       << known_result << endl;
  return result == known_result;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
END_MAIN
