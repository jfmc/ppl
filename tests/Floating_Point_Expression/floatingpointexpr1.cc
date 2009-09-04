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

typedef Linear_Form<fl_r_oc> Interval_Linear_Form;
typedef Division_Floating_Point_Expression<fl_r_oc, IEEE754_Single> div_fpes;
typedef Division_Floating_Point_Expression<fl_r_oc, IEEE754_Double> div_fped;
typedef Difference_Floating_Point_Expression<fl_r_oc, IEEE754_Single> dif_fpes;
typedef Difference_Floating_Point_Expression<fl_r_oc, IEEE754_Double> dif_fped;
typedef Multiplication_Floating_Point_Expression<fl_r_oc, IEEE754_Single> mul_fpes;
typedef Multiplication_Floating_Point_Expression<fl_r_oc, IEEE754_Double> mul_fped;
typedef Sum_Floating_Point_Expression<fl_r_oc, IEEE754_Single> sum_fpes;
typedef Sum_Floating_Point_Expression<fl_r_oc, IEEE754_Double> sum_fped;
typedef Constant_Floating_Point_Expression<fl_r_oc, IEEE754_Single> con_fpes;
typedef Constant_Floating_Point_Expression<fl_r_oc, IEEE754_Double> con_fped;
typedef Variable_Floating_Point_Expression<fl_r_oc, IEEE754_Single> var_fpes;
typedef Variable_Floating_Point_Expression<fl_r_oc, IEEE754_Double> var_fped;
typedef Opposite_Floating_Point_Expression<fl_r_oc, IEEE754_Single> opp_fpes;
typedef Opposite_Floating_Point_Expression<fl_r_oc, IEEE754_Double> opp_fped;

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;

bool
test01() {
  return true;
}

bool
test02() {
  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
