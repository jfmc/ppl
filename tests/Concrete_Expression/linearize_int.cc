/* Testing linearization algorithm ad its related functions.
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
  Tests linearization 3*B + [2, 2147483639] op B + [1, 16]
  with op content {|, & , ^ , << , >> } where B in [2, 3].
*/
bool
test01(){
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "B in " << tmp1 << endl << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(1), tmp1);

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

  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);
  Integer_Linear_Form result_or, result_and,result_xor, result_lshift,
    result_rshift;

  Variable B(1);

  Integer_Linear_Form known_result_or = B;
  known_result_or *= b;
  known_result_or += inh_term;

  Integer_Linear_Form lf = Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_and, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_and = known_result_or;
  known_result_xor = known_result_or;
  known_result_lshift = known_result_or;
  known_result_rshift = known_result_or;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  inh_term1 += tmp1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***"
       << endl << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***"
       << endl << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization A + 3*B + [2, 3] op 2*A + B + [1, 9]
  with op content {|, & , ^ , << , >> } where A in [0,1] and B in [2, 3].
*/
bool
test02(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;

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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_and,result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = A;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and += inh_term;

  Integer_Linear_Form lf = A;
  lf *= a1;
  lf += Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  inh_term1 += tmp1;
  a1 *= tmp;
  inh_term1 += a1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***"
       << endl << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
   nout << "*** " << known_result_rshift << " >> " << lf << " ***"
	<< endl << "*** result_rshift *** " << endl
	<< result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
     &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization -A + -3*B + [-3, -2] op -A -B + [-4, -1]
  with op content {|, & , ^ , << , >> } where A in [0, 1] and B in [2, 3].
*/
bool
test03(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = A;
  known_result_and *= a;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and += inh_term;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a;
  lf += a * Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  tmp *= a;
  tmp1 *= a;
  inh_term1 += tmp;
  inh_term1 += tmp1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl
       << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***" << endl
       << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization -A + -3*B + [-3, -2] op A + B + [1, 4]
  with op content {|, & , ^ , << , >> } where A in [0,1] and B in [2,3].
*/
bool
test04(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_and ,result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = A;
  known_result_and *= a;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and += inh_term;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf += Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  inh_term1 += tmp;
  inh_term1 += tmp1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl
       << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***" << endl
       << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);
    ;
  return ok;
}

/*
  Tests linearization A + 3*B + [2, 3] op -A -B + [-4, -1]
  with op content {|, & , ^ , << , >> } where A in [0,1] and B in [2,3].
*/
bool
test05(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = A;
  known_result_and *= a;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and += inh_term;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= b1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form  known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  tmp *=b1;
  tmp1 *= b1;
  inh_term1 += tmp;
  inh_term1 += tmp1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl
       << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***" << endl
       << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization A op A with op content {|, & , ^ , << , >> }
  where A in [0,1].
*/
bool
test06(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  nout << "A in " << tmp << endl << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(1));
  oracle.int_store.set_interval(Variable(0), tmp);

  Approximable_Reference<C_Expr> var0(Integer_Type,
				      Int_Interval(mpz_class(0)), 0);

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &var0, &var0);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &var0, &var0);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &var0, &var0);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &var0, &var0);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &var0, &var0);

  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Integer_Linear_Form known_result_and = A;

  Integer_Linear_Form lf = A;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = A;
  known_result_xor = A;
  known_result_lshift = A;
  known_result_rshift = A;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***"
       << endl << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << tmp;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***"
       << endl << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> tmp;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization A + 3 op B with op content {|, & , ^ , << , >> }
  where A in [1,1] and B in [5,5].
*/
bool
test07(){
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
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &var1);
  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &var1);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &var1);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &var1);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &var1);

  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_or = A;
  known_result_or += Integer_Int_Interval(3);

  Integer_Linear_Form known_result_and = known_result_or;
  Integer_Linear_Form known_result_xor = known_result_or;
  Integer_Linear_Form known_result_lshift = known_result_or;
  Integer_Linear_Form known_result_rshift = known_result_or;
  Integer_Linear_Form lf = Integer_Linear_Form(B);

  bool failed_and = false;
  bool failed_or = false;
  bool failed_xor = false;

  if (!linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_or)) {
    nout << "*** Linearization failed, the two linear forms have "
	 << "different size *** " << endl << endl;
    failed_or = true;
  }

  if (!linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_and)) {
    nout << "*** Linearization failed, the two linear forms have "
	 << "different size *** " << endl << endl;
    failed_and = true;
  }

  if (!linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_xor)) {
    nout << "*** Linearization failed, the two linear forms have "
	 << "different size *** " << endl << endl;
    failed_xor = true;
  }

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl
       << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << tmp1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***" << endl
       << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> tmp1;
  nout << endl;

  bool ok =
    (failed_or)
    &&
    (failed_and)
    &&
    (failed_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization -3*B + [-2147483641, -2] op -B + [-9, -1]
  with op content {|, & , ^ , << , >> } where B in [2, 3].
*/
bool
test08(){
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "B in " << tmp1 << endl << endl;
  Test_Oracle oracle(Integer_Interval_Abstract_Store(2));
  oracle.int_store.set_interval(Variable(1), tmp1);

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

  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);
  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_or, result_and, result_xor, result_lshift,
    result_rshift;

  Variable B(1);

  Integer_Linear_Form known_result_or = B;
  known_result_or *= b;
  known_result_or += inh_term;

  Integer_Linear_Form lf = B;
  lf *= b1;
  lf += inh_term1;

  Integer_Linear_Form known_result_and, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_and = known_result_or;
  known_result_xor = known_result_or;
  known_result_lshift = known_result_or;
  known_result_rshift = known_result_or;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  tmp1 *= b1;
  inh_term1 += tmp1;

  linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_lshift);
  nout << "*** " << known_result_lshift << " << " << lf << " ***"
       << endl << "*** result_lshift *** " << endl
       << result_lshift << endl;
  known_result_lshift << inh_term1;
  nout << endl;

  linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		result_rshift);
  nout << "*** " << known_result_rshift << " >> " << lf << " ***"
       << endl << "*** result_rshift *** " << endl
       << result_rshift << endl;
  known_result_rshift >> inh_term1;
  nout << endl;

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    (result_lshift == known_result_lshift)
    &&
    (result_rshift == known_result_rshift);

  return ok;
}

/*
  Tests linearization A + 12*B + [2, 3] op -A -2147483638*B + [-4, -1]
  with op content {|, & , ^ , << , >> } where A in [0, 1] and B in [2, 3].
*/
bool
test09(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);

  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);
  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;
  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = Integer_Linear_Form(A);
  known_result_and *= a;
  num /= den;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and -= num;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;
  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  //  b1 *= tmp1;
  //inh_term1 += b1;

  bool failed_lshift = false;
  bool failed_rshift = false;

  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl;
  if (!linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_lshift)) {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "contain infinity *** " << endl << endl;
    failed_lshift = true;
  }

  nout << "*** " << known_result_rshift << " << " << lf << " ***" << endl;
  if (!linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_rshift))  {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "contain infinity *** " << endl << endl;
    failed_rshift = true;
  }

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    failed_lshift
    &&
    failed_rshift;

  return ok;
}

/*
  Tests linearization -A + -12*B + [-3, -2] op 2*A + 2147483638*B + [1, 4]
  with op content {|, & , ^ , << , >> } where A in [0,1] and B in [2,3]
*/
bool
test10(){
  Integer_Int_Interval tmp(0);
  tmp.join_assign(1);
  Integer_Int_Interval tmp1(2);
  tmp1.join_assign(3);
  nout << "A in " << tmp << " and B in " << tmp1 << endl << endl;
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

  Binary_Operator<C_Expr> band(Integer_Type, Binary_Operator<C_Expr>::BAND,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> bor(Integer_Type, Binary_Operator<C_Expr>::BOR,
                              &expr1, &expr2);

  Binary_Operator<C_Expr> bxor(Integer_Type, Binary_Operator<C_Expr>::BXOR,
			       &expr1, &expr2);
  Binary_Operator<C_Expr> lshift(Integer_Type, Binary_Operator<C_Expr>::LSHIFT,
				 &expr1, &expr2);
  Binary_Operator<C_Expr> rshift(Integer_Type, Binary_Operator<C_Expr>::RSHIFT,
				 &expr1, &expr2);

  Integer_Linear_Form result_and, result_or, result_xor, result_lshift,
    result_rshift;

  Variable A(0);
  Variable B(1);

  Integer_Linear_Form known_result_and = Integer_Linear_Form(A);
  known_result_and *= a;
  num /= den;
  known_result_and += b * Integer_Linear_Form(B);
  known_result_and -= num;

  Integer_Linear_Form lf = Integer_Linear_Form(A);
  lf *= a1;
  lf += b1 * Integer_Linear_Form(B);
  lf += inh_term1;

  Integer_Linear_Form known_result_or, known_result_xor, known_result_lshift,
    known_result_rshift;
  known_result_or = known_result_and;
  known_result_xor = known_result_and;
  known_result_lshift = known_result_and;
  known_result_rshift = known_result_and;

  linearize_int(band, oracle, Integer_Linear_Form_Abstract_Store(), result_and);
  nout << "*** " << known_result_and << " & " << lf << " ***" << endl
       << "*** result_and *** " << endl
       << result_and << endl;
  known_result_and &= lf;
  nout << endl;

  linearize_int(bor, oracle, Integer_Linear_Form_Abstract_Store(), result_or);
  nout << "*** " << known_result_or << " | " << lf << " ***" << endl
       << "*** result_or *** " << endl
       << result_or << endl;
  known_result_or |= lf;
  nout << endl;

  linearize_int(bxor, oracle, Integer_Linear_Form_Abstract_Store(), result_xor);
  nout << "*** " << known_result_xor << " ^ " << lf << " ***" << endl
       << "*** result_xor *** " << endl
       << result_xor << endl;
  known_result_xor ^= lf;
  nout << endl;

  bool failed_lshift = false;
  bool failed_rshift = false;

  nout << "*** " << known_result_lshift << " << " << lf << " ***" << endl;
  if (!linearize_int(lshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_lshift)) {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "contain infinity *** " << endl << endl;
    failed_lshift = true;
  }

  nout << "*** " << known_result_rshift << " >> " << lf << " ***" << endl;
  if (!linearize_int(rshift, oracle, Integer_Linear_Form_Abstract_Store(),
		     result_rshift)) {
    nout << "*** Linearization failed, intervalize second linear form "
	 << "contain infinity *** " << endl << endl;
    failed_rshift = true;
  }

  bool ok =
    (result_or == known_result_or)
    &&
    (result_and == known_result_and)
    &&
    (result_xor == known_result_xor)
    &&
    failed_lshift
    &&
    failed_rshift;

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
END_MAIN
