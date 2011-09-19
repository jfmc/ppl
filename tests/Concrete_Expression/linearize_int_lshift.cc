/* Testing linearization algorithm ad its related function of the bitwise
   left shift.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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
#include "linearize_integer.hh"
#include <vector>

namespace {

class Test_Oracle : public Oracle<C_Expr, Integer_Int_Interval> {
public:
  Test_Oracle() : int_store(0) {}
  Test_Oracle(Integer_Interval_Abstract_Store init) : int_store(init) {}
  bool get_interval(dimension_type dim, Integer_Int_Interval& result) const {
    result = int_store.get_interval(Variable(dim));
    return true;
  }
  bool get_fp_constant_value(const Floating_Point_Constant<C_Expr>&,
                             Integer_Int_Interval&) const {
    return false;
  }
  bool get_int_constant_value(const Int_Constant<C_Expr>& expr,
			      Integer_Int_Interval& result) const {
    result = expr.value;
    return true;
  }
  bool get_integer_expr_value(const Concrete_Expression<C_Expr>&,
                              Integer_Int_Interval&) const {
    return false;
  }
  bool get_associated_dimensions(const Approximable_Reference<C_Expr>& expr,
				 std::set<dimension_type>& result) const {
    result = expr.dimensions;
    return true;
  }

  Integer_Interval_Abstract_Store int_store;
};

  using namespace Parma_Polyhedra_Library::IO_Operators;

Concrete_Expression_Type Integer_Type =
  Concrete_Expression_Type::bounded_integer(BITS_32, SIGNED_2_COMPLEMENT,
					    OVERFLOW_IMPOSSIBLE);

typedef Integer_Interval Int_Interval;

/*
  Tests linearization 3*B + [2, 2147483639] << B + [1, 16]
  where:
  -) B in [2, 3];
  -) B in [0, 7];
  -) B in [8, 10];
  -) B in [3, 6].
*/
bool
test01() {
  Integer_Int_Interval tmp(2);
  tmp.join_assign(3);
  nout << "B in " << tmp << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(1), tmp);

  Integer_Int_Interval b(3);

  Integer_Int_Interval inh_term(2);
  inh_term.join_assign(2147483639);

  Integer_Int_Interval b1(1);

  Integer_Int_Interval inh_term1(1);
  inh_term1.join_assign(16);

  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term,&term_b);

  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &term_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable B(1);

  Integer_Linear_Form known_result = B;
  known_result *= b;
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form known_result3 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(B);
  lf += inh_term1;

  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp1(0);
  tmp1.join_assign(7);
  nout << "B in " << tmp1 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(1), tmp1);

  tmp1 += inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp1;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp2(8);
  tmp2.join_assign(10);
  nout << "B in " << tmp2 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(1), tmp2);

  tmp2 += inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp2;
  nout << endl;

  bool ok2 = result == known_result2;

  Integer_Int_Interval tmp3(3);
  tmp3.join_assign(6);
  nout << "B in " << tmp3 << endl;
  Test_Oracle oracle3(Integer_Interval_Abstract_Store(2));
  oracle3.int_store.set_interval(Variable(1), tmp3);

  tmp3 += inh_term1;

  linearize_int(lshift, oracle3, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result3 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result3 << tmp3;
  nout << endl;

  bool ok3 = result == known_result3;

  return (ok && ok1 && ok2 && ok3);
}

/*
  Tests linearization A + 3*B + [2, 3] << 2*A + B + [1, 9]
  where:
   -) A in [0, 1] and B in [2, 3];
   -) A in [2, 3] and B in [0, 1];
   -) A in [0, 5] and B in [4, 10];
   -) A in [5, 7] and B in [1, 4].
*/
bool
test02() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;

  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval b(3);

  Integer_Int_Interval inh_term(2);
  inh_term.join_assign(3);

  Integer_Int_Interval a1(2);
  Integer_Int_Interval inh_term1(1);
  inh_term1.join_assign(9);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &var0, &term_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term,&sum_a_b);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a1, &var1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = A;
  known_result += b * Integer_Linear_Form(B);
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form known_result3 = known_result;

  Integer_Linear_Form lf = A;
  lf *= a1;
  lf += Integer_Linear_Form(B);
  lf += inh_term1;

  tmp1 += a1 * tmp;
  tmp1 += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp1;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp2(2);
  tmp2.join_assign(3);
  Integer_Int_Interval tmp3(0);
  tmp3.join_assign(1);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;

  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  tmp3 += a1 * tmp2;
  tmp3 += inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp3;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp4(0);
  tmp4.join_assign(5);
  Integer_Int_Interval tmp5(4);
  tmp5.join_assign(10);
  nout << "A in " << tmp4 << " and B in " << tmp5 << endl;

  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(0), tmp4);
  oracle2.int_store.set_interval(Variable(1), tmp5);

  tmp5 += a1 * tmp4;
  tmp5 += inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp5;
  nout << endl;

  bool ok2 = result == known_result2;

  Integer_Int_Interval tmp6(5);
  tmp6.join_assign(7);
  Integer_Int_Interval tmp7(1);
  tmp7.join_assign(4);
  nout << "A in " << tmp6 << " and B in " << tmp7 << endl;

  Test_Oracle oracle3(Integer_Interval_Abstract_Store(2));
  oracle3.int_store.set_interval(Variable(0), tmp6);
  oracle3.int_store.set_interval(Variable(1), tmp7);

  tmp7 += a1 * tmp6;
  tmp7 += inh_term1;
 
  linearize_int(lshift, oracle3, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result3 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result3 << tmp7;
  nout << endl;

  bool ok3 = result == known_result3;

  return (ok && ok1 && ok2 && ok3);
}

/*
  Tests linearization -A + -3*B + [-3, -2] << -A -B + [-4, -1]
  where:
  -) A in [0, 1] and B in [2, 3];
  -) A in [4, 8] and B in [0, 5].
*/
bool
test03() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(-1);

  Integer_Int_Interval b(-3);

  Integer_Int_Interval inh_term(-2);
  inh_term.join_assign(-3);

  Integer_Int_Interval inh_term1(-1);
  inh_term1.join_assign(-4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, a);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term,&sum_a_b);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = A;
  known_result *= a;
  known_result += b * Integer_Linear_Form(B);
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a;
  lf += a * Integer_Linear_Form(B);
  lf += inh_term1;

  tmp *= a;
  tmp += a * tmp1;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp2(0);
  tmp2.join_assign(1);
  Integer_Int_Interval tmp3(tmp2);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  tmp2 *= a;
  tmp2 += a * tmp3;
  tmp2 += inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp2;
  nout << endl;

  bool ok1 = result == known_result1;

  return (ok && ok1);
}

/*
  Tests linearization -A + -3*B + [-3, -2] << A + B + [1, 4]
  where:
  -) A in [0, 1] and B in [2, 3];
  -) A in [2, 3] and B in [0, 1];
  -) A in [2, 9] and B in [4, 12].
*/
bool
test04() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(-1);

  Integer_Int_Interval b(-3);

  Integer_Int_Interval inh_term(-2);
  inh_term.join_assign(-3);

  Integer_Int_Interval inh_term1(1);
  inh_term1.join_assign(4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term, &sum_a_b);

  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &var0, &var1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);


  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = A;
  known_result *= a;
  known_result += b * Integer_Linear_Form(B);
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf += Integer_Linear_Form(B);
  lf += inh_term1;

  tmp1 += tmp + inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result << tmp1;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp2(2);
  tmp2.join_assign(3);
  Integer_Int_Interval tmp3(0);
  tmp3.join_assign(2);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  tmp3 += tmp2 + inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp3;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp4(2);
  tmp4.join_assign(9);
  Integer_Int_Interval tmp5(4);
  tmp5.join_assign(12);
  nout << "A in " << tmp4 << " and B in " << tmp5 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(0), tmp4);
  oracle2.int_store.set_interval(Variable(1), tmp5);

  tmp5 += tmp4 + inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp5;
  nout << endl;

  bool ok2 = result == known_result2;

  return (ok && ok1 && ok2);
}

/*
  Tests linearization A + 3*B + [2, 3] << -A -B + [-4, -1]
  where:
  -) A in [0, 1] and B in [2, 3];
  -) A in [0, 5] and B in [0, 4];
  -) A in [2, 8] and B in [1, 11];
*/
bool
test05() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(1);

  Integer_Int_Interval b(3);

  Integer_Int_Interval inh_term(2);
  inh_term.join_assign(3);

  Integer_Int_Interval b1(-1);

  Integer_Int_Interval inh_term1(-1);
  inh_term1.join_assign(-4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term,&sum_a_b);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = A;
  known_result *= a;
  known_result += b * Integer_Linear_Form(B);
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= b1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  tmp *= b1;
  tmp += b1 * tmp1;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp2(0);
  tmp2.join_assign(5);
  Integer_Int_Interval tmp3(0);
  tmp3.join_assign(4);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  tmp2 *= b1;
  tmp2 += b1 * tmp3;
  tmp2 += inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp2;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp4(2);
  tmp4.join_assign(8);
  Integer_Int_Interval tmp5(1);
  tmp5.join_assign(11);
  nout << "A in " << tmp4 << " and B in " << tmp5 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(0), tmp4);
  oracle2.int_store.set_interval(Variable(1), tmp5);

  tmp4 *= b1;
  tmp4 += b1 * tmp5;
  tmp4 += inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp4;
  nout << endl;

  bool ok2 = result == known_result2;

 return (ok && ok1 && ok2);
}

/*
  Tests linearization A << A
  where:
  -) A in [0, 1];
  -) A in [2, 2];
  -) A in [4, 8].
*/
bool
test06() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  nout << "A in " << tmp << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(1));
  oracle.int_store.set_interval(Variable(0), tmp);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &var0, &var0);

  Integer_Linear_Form result;

  Variable A(0);
  Integer_Linear_Form known_result = A;
  Integer_Linear_Form known_result1 = A;
  Integer_Linear_Form known_result2 = A;

  Integer_Linear_Form lf = A;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp1(2);
  nout << "A in " << tmp1 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(1));
  oracle1.int_store.set_interval(Variable(0), tmp1);

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp1;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp2(4);
  tmp2.join_assign(8);
  nout << "A in " << tmp2 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(1));
  oracle2.int_store.set_interval(Variable(0), tmp2);

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp2;
  nout << endl;

  bool ok2 = result == known_result2;

  return (ok && ok1 && ok2);
}

/*
  Tests linearization A + 3 << B
  where:
  -) A in [1, 1] and B in [5, 5];
  -) A in [5, 5] and B in [1, 1];
  -) A in [1, 4] and B in [4, 19].
*/
bool
test07() {
  Integer_Int_Interval tmp(1);
  Integer_Int_Interval tmp1(5);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff(Integer_Type, Integer_Int_Interval(3));

  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&var0, &coeff);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &var1);

  Integer_Linear_Form result;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = A;
  known_result += Integer_Int_Interval(3);

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(B);

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result << tmp1;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp2(5);
  Integer_Int_Interval tmp3(1);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp3;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp4(1);
  tmp4.join_assign(4);
  Integer_Int_Interval tmp5(4);
  tmp5.join_assign(19);
  nout << "A in " << tmp4 << " and B in " << tmp5 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(0), tmp4);
  oracle2.int_store.set_interval(Variable(1), tmp5);

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***" << endl
       << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp5;
  nout << endl;

  bool ok2 = result == known_result2;

  return (ok && ok1 && ok2);
}

/*
  Tests linearization -3*B + [-2147483641, -2] << -B + [-9, -1]
  where:
  -) B in [2, 3];
  -) B in [4, 7];
  -) B in [1, 12];
  -) B in [0, 10].
*/
bool
test08() {
  Integer_Int_Interval tmp(2);
  tmp.join_assign(3);
  nout << "B in " << tmp << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(1), tmp);

  Integer_Int_Interval b(-3);

  Integer_Int_Interval inh_term(-2);
  inh_term.join_assign(-2147483641);

  Integer_Int_Interval b1(-1);

  Integer_Int_Interval inh_term1(-1);
  inh_term1.join_assign(-9);

  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term,&term_b);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &term_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable B(1);

  Integer_Linear_Form known_result = B;
  known_result *= b;
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form known_result3 = known_result;

  Integer_Linear_Form lf = B;
  lf *= b1;
  lf += inh_term1;

  tmp *= b1;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = result == known_result;

  Integer_Int_Interval tmp1(4);
  tmp1.join_assign(7);
  nout << "B in " << tmp1 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(1), tmp1);

  tmp1 *= b1;
  tmp1 += inh_term1;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result1 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp1;
  nout << endl;

  bool ok1 = result == known_result1;

  Integer_Int_Interval tmp2(1);
  tmp2.join_assign(12);
  nout << "B in " << tmp2 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(1), tmp2);

  tmp2 *= b1;
  tmp2 += inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result2 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp2;
  nout << endl;

  bool ok2 = result == known_result2;

  Integer_Int_Interval tmp3(0);
  tmp3.join_assign(10);
  nout << "B in " << tmp3 << endl;
  Test_Oracle oracle3(Integer_Interval_Abstract_Store(2));
  oracle3.int_store.set_interval(Variable(1), tmp3);

  tmp3 *= b1;
  tmp3 += inh_term1;

  linearize_int(lshift, oracle3, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result3 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result3 << tmp3;
  nout << endl;

  bool ok3 = result == known_result3;

  return (ok && ok1 && ok2 && ok3);
}

/*
  Tests linearization A + 12*B + [2, 3] << -A -2147483638*B + [-4, -1]
  where:
  -) A in [0, 1] and B in [2, 3];
*/
bool
test09() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(1);

  Integer_Int_Interval b(12);

  Integer_Int_Interval num(-4);
  num.join_assign(-6);

  Integer_Int_Interval den(2);

  Integer_Int_Interval a1(-1);

  Integer_Int_Interval b1(-2147483638);

  Integer_Int_Interval inh_term1(-1);
  inh_term1.join_assign(-4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeffnum(Integer_Type, num);
  Int_Constant<C_Expr> coeffden(Integer_Type, den);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> div(Integer_Type, Binary_Operator<C_Expr>::DIV,
                              &coeffnum, &coeffden);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::SUB,
				&sum_a_b,&div);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = Integer_Linear_Form(A);
  known_result *= a;
  num /= den;
  known_result += b * Integer_Linear_Form(B);
  known_result -= num;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  bool failed_lshift = false;

  nout << "*** " << known_result << " << " << lf << " ***" << endl;
  if (!linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result)) {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "is not bounded ***" << endl << endl;
    failed_lshift = true;
  }

  bool ok = failed_lshift;

  return ok;
}

/*
  Tests linearization -A + -12*B + [-3, -2] << 2*A + 2147483638*B + [1, 4]
  where A in [0,1] and B in [2,3]
*/
bool
test10() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(-1);

  Integer_Int_Interval b(-12);

  Integer_Int_Interval num(4);
  num.join_assign(6);

  Integer_Int_Interval den(2);

  Integer_Int_Interval a1(2);

  Integer_Int_Interval b1(2147483638);

  Integer_Int_Interval inh_term1(1);
  inh_term1.join_assign(4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeffnum(Integer_Type, num);
  Int_Constant<C_Expr> coeffden(Integer_Type, den);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> div(Integer_Type, Binary_Operator<C_Expr>::DIV,
                              &coeffnum, &coeffden);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::SUB,
				&sum_a_b,&div);
  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
			       &coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = Integer_Linear_Form(A);
  known_result *= a;
  num /= den;
  known_result += b * Integer_Linear_Form(B);
  known_result -= num;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  bool failed_lshift = false;

  nout << "*** " << known_result << " << " << lf << " ***" << endl;
  if (!linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result)) {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "is not bounded ***" << endl << endl;
    failed_lshift = true;
  }

  bool ok = failed_lshift;

  return ok;
}

/*
  Tests linearization A + 12*B + [2, 3] << -A -8*B + [-4, -1]
  where:
  -) A in [0, 1] and B in [2, 3];
*/
bool
test11() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(1);

  Integer_Int_Interval b(12);

  Integer_Int_Interval num(-4);
  num.join_assign(-6);

  Integer_Int_Interval den(2);

  Integer_Int_Interval a1(-1);

  Integer_Int_Interval b1(-8);

  Integer_Int_Interval inh_term1(-1);
  inh_term1.join_assign(-4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeffnum(Integer_Type, num);
  Int_Constant<C_Expr> coeffden(Integer_Type, den);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> div(Integer_Type, Binary_Operator<C_Expr>::DIV,
                              &coeffnum, &coeffden);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::SUB,
				&sum_a_b,&div);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = Integer_Linear_Form(A);
  known_result *= a;
  num /= den;
  known_result += b * Integer_Linear_Form(B);
  known_result -= num;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  tmp *= a1;
  tmp += b1 * tmp1;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = known_result == result;

  return ok;
}

/*
  Tests linearization -A + -12*B + [-3, -2] << A + 8*B + [1, 4]
  where:
  -) A in [0, 1] and B in [2, 3];
  -) A in [2, 3] and B in [0, 1].
  -) A in [0, 3] and B in [1, 2].
*/
bool
test12() {
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);

  Integer_Int_Interval a(-1);

  Integer_Int_Interval b(-12);

  Integer_Int_Interval inh_term(-3);
  inh_term.join_assign(-2);

  Integer_Int_Interval a1(1);

  Integer_Int_Interval b1(8);

  Integer_Int_Interval inh_term1(1);
  inh_term1.join_assign(4);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&sum_a_b,&coeff_inh_term);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&coeff_inh_term1, &sum_a1_b1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result = Integer_Linear_Form(A);
  known_result *= a;
  known_result += b * Integer_Linear_Form(B);
  known_result += inh_term;

  Integer_Linear_Form known_result1 = known_result;

  Integer_Linear_Form known_result2 = known_result;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  tmp *= a1;
  tmp += b1 * tmp1;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result);
  nout << "*** " << known_result << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = known_result == result;

  Integer_Int_Interval tmp2(2);
  tmp2.join_assign(3);
  Integer_Int_Interval tmp3(0);
  tmp3.join_assign(1);
  nout << "A in " << tmp2 << " and B in " << tmp3 << endl;
  Test_Oracle oracle1(Integer_Interval_Abstract_Store(2));
  oracle1.int_store.set_interval(Variable(0), tmp2);
  oracle1.int_store.set_interval(Variable(1), tmp3);

  tmp2 *= a1;
  tmp3 *= b1;
  tmp2 += inh_term1;
  tmp2 += tmp3;

  linearize_int(lshift, oracle1, Integer_Linear_Form_Abstract_Store(),
		     result);
  nout << "*** " << known_result1 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result1 << tmp2;
  nout << endl;

  bool ok1 = known_result1 == result;

  Integer_Int_Interval tmp4(0);
  tmp4.join_assign(3);
  Integer_Int_Interval tmp5(1);
  tmp5.join_assign(2);
  nout << "A in " << tmp4 << " and B in " << tmp5 << endl;
  Test_Oracle oracle2(Integer_Interval_Abstract_Store(2));
  oracle2.int_store.set_interval(Variable(0), tmp4);
  oracle2.int_store.set_interval(Variable(1), tmp5);

  tmp4 *= a1;
  tmp4 += b1 * tmp5;
  tmp4 += inh_term1;

  linearize_int(lshift, oracle2, Integer_Linear_Form_Abstract_Store(),
		     result);
  nout << "*** " << known_result2 << " << " << lf << " ***"
       << endl << "*** result *** " << endl
       << result << endl;
  known_result2 << tmp4;
  nout << endl;

  bool ok2 = known_result2 == result;

  return (ok && ok1 && ok2);
}

/*
  Tests linearization A + 4*B + 3*C + [3, 9] << 3*A + B + 2*C + [9, 12]
  where A in [1, 2], B in [3, 5] and C in [0, 2].
*/
bool
test13() {
  Integer_Int_Interval tmp(1);
  tmp.join_assign(2);
  Integer_Int_Interval tmp1(5);
  tmp1.join_assign(3);
  Integer_Int_Interval tmp2(0);
  tmp2.join_assign(2);
  nout << "A in " << tmp << " B in " << tmp1 << " and C in " << tmp2 << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(3));
  oracle.int_store.set_interval(Variable(0), tmp);
  oracle.int_store.set_interval(Variable(1), tmp1);
  oracle.int_store.set_interval(Variable(2), tmp2);

  Integer_Int_Interval a(1);
  Integer_Int_Interval b(4);
  Integer_Int_Interval c(3);
  Integer_Int_Interval inh_term(3);
  inh_term.join_assign(9);

  Integer_Int_Interval a1(3);
  Integer_Int_Interval b1(1);
  Integer_Int_Interval c1(2);
  Integer_Int_Interval inh_term1(9);
  inh_term1.join_assign(12);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);
  Approximable_Reference<C_Expr> var1(Integer_Type,
				      Int_Interval(mpz_class(0)), 1);
  Approximable_Reference<C_Expr> var2(Integer_Type,
				      Int_Interval(mpz_class(0)), 2);

  Int_Constant<C_Expr> coeff_a(Integer_Type, a);
  Int_Constant<C_Expr> coeff_b(Integer_Type, b);
  Int_Constant<C_Expr> coeff_c(Integer_Type, c);
  Int_Constant<C_Expr> coeff_inh_term(Integer_Type, inh_term);
  Int_Constant<C_Expr> coeff_a1(Integer_Type, a1);
  Int_Constant<C_Expr> coeff_b1(Integer_Type, b1);
  Int_Constant<C_Expr> coeff_c1(Integer_Type, c1);
  Int_Constant<C_Expr> coeff_inh_term1(Integer_Type, inh_term1);

  Binary_Operator<C_Expr> term_a(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var0, &coeff_a);
  Binary_Operator<C_Expr> term_b(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var1, &coeff_b);
  Binary_Operator<C_Expr> term_c(Integer_Type, Binary_Operator<C_Expr>::MUL,
				 &var2, &coeff_c);
  Binary_Operator<C_Expr> sum_a_b(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &term_a, &term_b);
  Binary_Operator<C_Expr> sum_a_b_c(Integer_Type, Binary_Operator<C_Expr>::ADD,
				  &sum_a_b, &term_c);
  Binary_Operator<C_Expr> expr1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&sum_a_b_c, &coeff_inh_term);

  Binary_Operator<C_Expr> term_a1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var0, &coeff_a1);
  Binary_Operator<C_Expr> term_b1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var1, &coeff_b1);
  Binary_Operator<C_Expr> term_c1(Integer_Type, Binary_Operator<C_Expr>::MUL,
				  &var2, &coeff_c1);
  Binary_Operator<C_Expr> sum_a1_b1(Integer_Type, Binary_Operator<C_Expr>::ADD,
				    &term_a1, &term_b1);
  Binary_Operator<C_Expr>
    sum_a1_b1_c1(Integer_Type, Binary_Operator<C_Expr>::ADD,
		 &sum_a1_b1, &term_c1);
  Binary_Operator<C_Expr> expr2(Integer_Type, Binary_Operator<C_Expr>::ADD,
				&sum_a1_b1_c1, &coeff_inh_term1);

  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result;

  Variable A(0);
  Variable B(1);
  Variable C(2);

  Integer_Linear_Form known_result = A;
  known_result *= a;
  known_result += b * Integer_Linear_Form(B);
  known_result += c * Integer_Linear_Form(C);
  known_result += inh_term;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += c1 * Integer_Linear_Form(C);
  lf += inh_term1;

  tmp *= a1;
  tmp += b1 * tmp1;
  tmp += c1 * tmp2;
  tmp += inh_term1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result);
  nout << "*** " << known_result << " << " << lf << " ***" << endl
	 << "*** result *** " << endl
	 << result << endl;
  known_result << tmp;
  nout << endl;

  bool ok = known_result == result;

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
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
  DO_TEST(test11);
  DO_TEST(test12);
  DO_TEST(test13);
END_MAIN
