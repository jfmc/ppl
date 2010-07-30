/* Test Rate Limiter on differents abstract domains.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "C_Expr.defs.hh"

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;
Concrete_Expression_Type FP_Type =
  Concrete_Expression_Type::floating_point(ANALYZED_FP_FORMAT);

typedef Integer_Interval_Type Int_Interval;

/*
This file tests a rate limiter that, given random input flows of floating
point values X and D, bounded respectively by [-128, 128] and [0, 16],
computes an output flow Y that tries to follow X while having a change rate
limited by D. The pseudo-code of such rate limiter is the following:

Y = 0;
for (n = 0; n < N; ++n) {
  X = [-128, 128];
  D = [0, 16];
  S = Y;
  R = X - S;
  Y = X;
  if (R <= -D)
    Y = S - D;
  if (R >= D)
    Y = S + D;
}
*/

void
set_M(Coefficient& M, int m) {
  if (std::numeric_limits<Coefficient>::is_bounded) {
    if (greater_than(std::numeric_limits<Coefficient>::min(),
                     std::numeric_limits<ANALYZER_FP_FORMAT>::min())
        || less_than(std::numeric_limits<Coefficient>::max(),
                     std::numeric_limits<ANALYZER_FP_FORMAT>::max())) {
      // This may still provoke an arithmetic overflow exception:
      // no problem.
      M = m;
      return;
    }
  }
  // Cannot provoke an overflow.
  assign_r(M, m, ROUND_DOWN);
}

// Tests rate limiter using boxes and ignoring rounding errors.
bool
test01() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval_Abstract_Store as_begin(5);
  FP_Interval tmp(0);
  unsigned short n = 0;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 144);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  abstract_store.affine_form_image(Y, FP_Linear_Form(tmp));

  do {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;
    print_constraints(abstract_store, "*** begin loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.affine_form_image(X, FP_Linear_Form(tmp));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.affine_form_image(D, FP_Linear_Form(tmp));

    // S = Y;
    abstract_store.affine_form_image(S, FP_Linear_Form(Y));

    // R = X - S;
    abstract_store.affine_form_image(R, FP_Linear_Form(X - S));

    // Y = X;
    abstract_store.affine_form_image(Y, FP_Linear_Form(X));

    // if (R <= -D) Y = S - D;
    FP_Interval_Abstract_Store as_then(abstract_store);
    as_then.refine_with_constraint(R <= -D);
    as_then.affine_form_image(Y, FP_Linear_Form(S - D));

    abstract_store.refine_with_constraint(R > -D);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_constraint(R >= D);
    as_then.affine_form_image(Y, FP_Linear_Form(S + D));

    abstract_store.refine_with_constraint(R < D);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R >= D)  Y = S + D; ***");

    abstract_store.upper_bound_assign(as_begin);
    abstract_store.limited_CC76_extrapolation_assign(as_begin, cs);
    print_constraints(abstract_store, "*** end loop ***");
    ++n;

  } while (as_begin != abstract_store);

  FP_Interval y = abstract_store.get_interval(Y);
  nout << "*** Y in " << y << " ***" << endl;
  return !y.is_bounded();
}

// Tests rate limiter using bounded differences and ignoring rounding
// errors.
bool
test02() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  FP_BD_Shape abstract_store(5);
  FP_BD_Shape as_begin(5);
  FP_Interval tmp(0);
  unsigned short n = 0;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 144);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  abstract_store.affine_form_image(Y, FP_Linear_Form(tmp));

  do {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;
    print_constraints(abstract_store, "*** begin loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.affine_form_image(X, FP_Linear_Form(tmp));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.affine_form_image(D, FP_Linear_Form(tmp));

    // S = Y;
    abstract_store.affine_form_image(S, FP_Linear_Form(Y));

    // R = X - S;
    abstract_store.affine_form_image(R, FP_Linear_Form(X - S));

    // Y = X;
    abstract_store.affine_form_image(Y, FP_Linear_Form(X));

    // if (R <= -D) Y = S - D;
    FP_BD_Shape as_then(abstract_store);
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    as_then.affine_form_image(Y, FP_Linear_Form(S - D));

    abstract_store.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                                       FP_Linear_Form(R));
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
                      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    as_then.affine_form_image(Y, FP_Linear_Form(S + D));

    abstract_store.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                                      FP_Linear_Form(D));
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
                      "*** after if (R >= D)  Y = S + D; ***");

    abstract_store.upper_bound_assign(as_begin);
    abstract_store.limited_BHMZ05_extrapolation_assign(as_begin, cs);
    print_constraints(abstract_store, "*** end loop ***");
    n++;

  } while (as_begin != abstract_store);

  tmp = (FP_Interval_Abstract_Store(abstract_store)).get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return !tmp.is_bounded();
}

// Tests rate limiter using octagonal shapes and ignoring rounding
// errors.
bool
test03() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  //FP_Interval_Abstract_Store abstract_store();
  FP_Octagonal_Shape abstract_store(5, UNIVERSE);
  FP_Octagonal_Shape as_begin(5, EMPTY);
  FP_Interval tmp(0);
  unsigned short n = 0;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 136);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  abstract_store.affine_form_image(Y, FP_Linear_Form(tmp));

  do {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;
    print_constraints(abstract_store, "*** begin loop ***");

    //X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.affine_form_image(X, FP_Linear_Form(tmp));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.affine_form_image(D, FP_Linear_Form(tmp));

    // S = Y;
    abstract_store.affine_form_image(S, FP_Linear_Form(Y));

    // R = X - S;
    abstract_store.affine_form_image(R, FP_Linear_Form(X - S));

    // Y = X;
    abstract_store.affine_form_image(Y, FP_Linear_Form(X));

    //if (R <= -D) Y = S - D;
    FP_Octagonal_Shape as_then(abstract_store);
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    as_then.affine_form_image(Y, FP_Linear_Form(S - D));

    abstract_store.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                                      FP_Linear_Form(R));
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    as_then.affine_form_image(Y, FP_Linear_Form(S + D));

    abstract_store.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));

    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after (R >= D)  Y = S + D; ***");

    abstract_store.upper_bound_assign(as_begin);
    abstract_store.limited_BHMZ05_extrapolation_assign(as_begin, cs);
    print_constraints(abstract_store, "*** end loop ***");
    ++n;

  } while (as_begin != abstract_store);

  tmp = (FP_Interval_Abstract_Store(abstract_store)).get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return (tmp.lower() == -136 && tmp.upper() == 136);
}

// Tests rate limiter using polyhedra and ignoring rounding errors.
bool
test04() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  NNC_Polyhedron abstract_store(5);
  NNC_Polyhedron as_begin(5);
  FP_Interval tmp(0);
  unsigned short n = 0;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 128);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  abstract_store.affine_form_image(Y, FP_Linear_Form(tmp));

  do {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;
    print_constraints(abstract_store, "*** begin loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.affine_form_image(X, FP_Linear_Form(tmp));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.affine_form_image(D, FP_Linear_Form(tmp));

    // S = Y;
    abstract_store.affine_form_image(S, FP_Linear_Form(Y));

    // R = X - S;
    abstract_store.affine_form_image(R, FP_Linear_Form(X - S));

    // Y = X;
    abstract_store.affine_form_image(Y, FP_Linear_Form(X));

    // if (R <= -D) Y = S - D;
    NNC_Polyhedron as_then(abstract_store);
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    as_then.affine_form_image(Y, FP_Linear_Form(S - D));

    abstract_store.generalized_refine_with_linear_form_inequality(
      -FP_Linear_Form(D), FP_Linear_Form(R), LESS_THAN);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    as_then.affine_form_image(Y, FP_Linear_Form(S + D));

    abstract_store.generalized_refine_with_linear_form_inequality(
      FP_Linear_Form(R), FP_Linear_Form(D), LESS_THAN);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R >= D)  Y = S + D; ***");

    abstract_store.upper_bound_assign(as_begin);
    abstract_store.limited_BHRZ03_extrapolation_assign(as_begin, cs);
    print_constraints(abstract_store, "*** end loop ***");
    ++n;

  } while(as_begin != abstract_store);

  tmp = (FP_Interval_Abstract_Store(abstract_store)).get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return (tmp.lower() == -128 && tmp.upper() == 128);
}


// Tests rate limiter using bounded differences and linearization of
// floating point expressions.
// In order to improve the analysis, the interval domain is used
// in parallel with bounded differences domain.
bool
test05() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  FP_Interval_Abstract_Store interval_store(5);
  FP_Interval_Abstract_Store is_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  FP_BD_Shape bd(5);
  FP_BD_Shape bd_begin(5);
  unsigned short n = 0;
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 144);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  Floating_Point_Constant<C_Expr> con_y("0", 2);
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk. If linearization succeeded, we model
  // the assignment Y = 0, invoking affine_form_image method.
  // In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the bounded
  // differences domain.
  // Then, we consider the intersection between these abstract domains.

  interval_store.affine_form_image(Y, FP_Linear_Form(tmp));
  if (linearize(con_y, interval_store, lf_abstract_store, lk))
    bd.affine_form_image(Y, lk);
  else
    bd.intersection_assign(FP_BD_Shape(interval_store));
  interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

  // This loop iterate until a fixed point is reached.
  do {

    // Iteration no. n+1.
    nout << "*** n = " << n << " ***" << endl;
    bd_begin = bd;
    is_begin = interval_store;
    print_constraints(interval_store, "*** before loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    interval_store.affine_form_image(X, FP_Linear_Form(tmp));
    bd.affine_form_image(X, FP_Linear_Form(tmp));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    interval_store.affine_form_image(D, FP_Linear_Form(tmp));
    bd.affine_form_image(D, FP_Linear_Form(tmp));    
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // S = Y;
    interval_store.affine_form_image(S, FP_Linear_Form(Y));
    Approximable_Reference<C_Expr> var_y(FP_Type, Int_Interval(mpz_class(0)),
                                         Y.id());
    if (linearize(var_y, interval_store, lf_abstract_store, ly))
      bd.affine_form_image(S, ly);
    else
      bd.intersection_assign(FP_BD_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // R = X - S;
    Approximable_Reference<C_Expr> px(FP_Type, Int_Interval(mpz_class(0)),
                                      X.id());
    Approximable_Reference<C_Expr> ps(FP_Type, Int_Interval(mpz_class(0)),
                                      S.id());
    Binary_Operator<C_Expr> x_dif_s(FP_Type, Binary_Operator<C_Expr>::SUB,
                                    &px, &ps);
    interval_store.affine_form_image(R, FP_Linear_Form(X - S));
    if (linearize(x_dif_s, interval_store, lf_abstract_store, lr))
      bd.affine_form_image(R, lr);
    else
      bd.intersection_assign(FP_BD_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // Y = X;
    interval_store.affine_form_image(Y, FP_Linear_Form(X));
    if (linearize(px, interval_store, lf_abstract_store, lx))
      bd.affine_form_image(Y, lx);
    else
      bd.intersection_assign(FP_BD_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // if (R <= -D)
    FP_BD_Shape bd_then(bd);
    FP_Interval_Abstract_Store is_then(interval_store);
    is_then.refine_with_constraint(R <= -D);
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                               -FP_Linear_Form(D));
    is_then.intersection_assign(FP_Interval_Abstract_Store(bd_then));

    // then Y = S - D;
    Approximable_Reference<C_Expr> pd(FP_Type, Int_Interval(mpz_class(0)),
                                      D.id());
    Binary_Operator<C_Expr> s_dif_d(FP_Type, Binary_Operator<C_Expr>::SUB,
                                    &ps, &pd);
    is_then.affine_form_image(Y, FP_Linear_Form(S - D));
    if (linearize(s_dif_d, is_then, lf_abstract_store, ly))
      bd_then.affine_form_image(Y, ly);
    else
      bd_then.intersection_assign(FP_BD_Shape(is_then));
    is_then.intersection_assign(FP_Interval_Abstract_Store(bd_then));

    // else skip;
    interval_store.refine_with_constraint(R > -D);
    bd.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                          FP_Linear_Form(R));
    bd.intersection_assign(FP_BD_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // LUB between then and else branches.
    bd.upper_bound_assign(bd_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    bd_then = bd;
    is_then = interval_store;
    is_then.refine_with_constraint(R >= D);
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    is_then.intersection_assign(FP_Interval_Abstract_Store(bd_then));

    // then Y = S + D;
    Binary_Operator<C_Expr> s_sum_d(FP_Type, Binary_Operator<C_Expr>::ADD,
                                    &ps, &pd);
    is_then.affine_form_image(Y, FP_Linear_Form(S + D));
    if (linearize(s_sum_d, is_then, lf_abstract_store, ly))
      bd_then.affine_form_image(Y, ly);
    else
      bd_then.intersection_assign(FP_BD_Shape(is_then));
    is_then.intersection_assign(FP_Interval_Abstract_Store(bd_then));

    // else skip;
    bd.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));
    interval_store.refine_with_constraint(R < D);
    bd.intersection_assign(FP_BD_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(bd));

    // LUB between then and else branches.
    bd.upper_bound_assign(bd_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    bd.upper_bound_assign(bd_begin);
    interval_store.upper_bound_assign(is_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= M; Y >= -M}
    bd.limited_BHMZ05_extrapolation_assign(bd_begin, cs);
    interval_store.limited_CC76_extrapolation_assign(is_begin, cs);
    print_constraints(interval_store, "*** end loop ***");
    ++n;

  } while(is_begin != interval_store);

  tmp = interval_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return (tmp.lower() == -144 && tmp.upper() == 144);
}

/*
// Tests rate limiter using octagonal shapes and linearization of
// floating point expressions.
// In order to improve the analysis, the interval domain is used
// in parallel with octagons domain.
bool
test06() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  FP_Interval_Abstract_Store interval_store(5);
  FP_Interval_Abstract_Store is_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  FP_Octagonal_Shape oc(5);
  FP_Octagonal_Shape oc_begin(5);
  unsigned short n = 0;
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 144);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  Con_FP_Expression con_y("0");
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk. If linearization succeeded, we model
  // the assignment Y = 0, invoking affine_form_image method.
  // FIXME: In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the octagons domain.
  // Then, we consider the intersection between these abstract domains.
  interval_store.affine_form_image(Y, FP_Linear_Form(tmp));
  if (con_y.linearize(interval_store, lf_abstract_store, lk))
    oc.affine_form_image(Y, lk);
  else
    oc.affine_form_image(Y, FP_Linear_Form(interval_store.get_interval(Y)));
  interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

  // This loop iterate until a fixed point is reached.
  do {

    // Iteration no. n+1.
    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;
    is_begin = interval_store;
    print_constraints(interval_store, "*** before loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    interval_store.affine_form_image(X, FP_Linear_Form(tmp));
    Con_FP_Expression con_x(-128, 128);
    if (con_x.linearize(interval_store, lf_abstract_store, lk))
      oc.affine_form_image(X, lk);
    else
      oc.affine_form_image(X, FP_Linear_Form(interval_store.get_interval(X)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    interval_store.affine_form_image(D, FP_Linear_Form(tmp));
    Con_FP_Expression con_d(0, 16);
    if (con_d.linearize(interval_store, lf_abstract_store, lk))
      oc.affine_form_image(D, lk);
    else
      oc.affine_form_image(D, FP_Linear_Form(interval_store.get_interval(D)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // S = Y;
    interval_store.affine_form_image(S, FP_Linear_Form(Y));
    Var_FP_Expression var_y(Y.id());
    if (var_y.linearize(interval_store, lf_abstract_store, ly))
      oc.affine_form_image(S, ly);
    else
      oc.affine_form_image(S, FP_Linear_Form(interval_store.get_interval(S)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // R = X - S;
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    interval_store.affine_form_image(R, FP_Linear_Form(X - S));
    if (x_dif_s.linearize(interval_store, lf_abstract_store, lr))
      oc.affine_form_image(R, lr);
    else
      oc.affine_form_image(R, FP_Linear_Form(interval_store.get_interval(R)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // Y = X;
    Var_FP_Expression var_x(X.id());
    interval_store.affine_form_image(Y, FP_Linear_Form(X));
    if (var_x.linearize(interval_store, lf_abstract_store, lx))
      oc.affine_form_image(Y, lx);
    else
      oc.affine_form_image(Y, FP_Linear_Form(interval_store.get_interval(Y)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // if (R <= -D)
    FP_Octagonal_Shape oc_then(oc);
    FP_Interval_Abstract_Store is_then(interval_store);
    is_then.refine_with_constraint(R <= -D);
    oc_then.refine_with_linear_form_inequality(lr, -lk);
    is_then.intersection_assign(FP_Interval_Abstract_Store(oc_then));

    // then Y = S - D;
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    is_then.affine_form_image(Y, FP_Linear_Form(S - D));
    if (s_dif_d.linearize(is_then, lf_abstract_store, ly))
      oc_then.affine_form_image(Y, ly);
    else
      oc_then.affine_form_image(Y, FP_Linear_Form(is_then.get_interval(Y)));
    is_then.intersection_assign(FP_Interval_Abstract_Store(oc_then));

    // else skip;
    interval_store.refine_with_constraint(R > -D);
    oc.refine_with_linear_form_inequality(-lk, lr);
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    oc_then = oc;
    is_then = interval_store;
    is_then.refine_with_constraint(R >= D);
    oc_then.refine_with_linear_form_inequality(lk, lr);
    is_then.intersection_assign(FP_Interval_Abstract_Store(oc_then));

    // then Y = S + D;
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    is_then.affine_form_image(Y, FP_Linear_Form(S + D));
    if (s_sum_d.linearize(is_then, lf_abstract_store, ly))
      oc_then.affine_form_image(Y, ly);
    else
      oc_then.affine_form_image(Y, FP_Linear_Form(is_then.get_interval(Y)));
    is_then.intersection_assign(FP_Interval_Abstract_Store(oc_then));

    // else skip;
    oc.refine_with_linear_form_inequality(lr, lk);
    interval_store.refine_with_constraint(R < D);
    oc.intersection_assign(FP_Octagonal_Shape(interval_store));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(oc));

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    oc.upper_bound_assign(oc_begin);
    interval_store.upper_bound_assign(is_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= M; Y >= -M}
    oc.limited_BHMZ05_extrapolation_assign(oc_begin, cs);
    interval_store.limited_CC76_extrapolation_assign(is_begin, cs);
    print_constraints(interval_store, "*** end loop ***");
    ++n;

  } while(is_begin != interval_store);

  tmp = interval_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return (tmp.lower() == -144 && tmp.upper() == 144);
}

// Tests rate limiter using polyhedra domain and linearization of
// floating point expressions.
// In order to improve the analysis, the interval domain is used
// in parallel with poyhedra domain.
bool
test07() {
  // Input signal.
  Variable X(0);
  // Maximum allowed for |R|.
  Variable D(1);
  // Output signal.
  Variable Y(2);
  // Last output.
  Variable S(3);
  // Actual rate.
  Variable R(4);

  FP_Interval_Abstract_Store interval_store(5);
  FP_Interval_Abstract_Store is_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  NNC_Polyhedron ph(5);
  NNC_Polyhedron ph_begin(5);
  unsigned short n = 0;
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  Constraint_System cs;
  Coefficient M;
  set_M(M, 144);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  Con_FP_Expression con_y("0");
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk. If linearization succeeded, we model
  // the assignment Y = 0, invoking affine_form_image method.
  // FIXME: In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the polyhedra domain.
  // Then, we consider the intersection between these abstract domains.
  interval_store.affine_form_image(Y, FP_Linear_Form(tmp));
  if (con_y.linearize(interval_store, lf_abstract_store, lk))
    ph.affine_form_image(Y, lk);
  else
    ph.affine_form_image(Y, FP_Linear_Form(interval_store.get_interval(Y)));
  interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

  // This loop iterate until a fixed point is reached.
  do {

    // Iteration no. n+1.
    nout << "*** n = " << n << " ***" << endl;
    ph_begin = ph;
    is_begin = interval_store;
    print_constraints(interval_store, "*** before loop ***");

    // X = [-128, 128];
    tmp.lower() = -128;
    tmp.upper() = 128;
    interval_store.affine_form_image(X, FP_Linear_Form(tmp));
    Con_FP_Expression con_x(-128, 128);
    if (con_x.linearize(interval_store, lf_abstract_store, lk))
      ph.affine_form_image(X, lk);
    else
      ph.affine_form_image(X, FP_Linear_Form(interval_store.get_interval(X)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // D = [0, 16];
    tmp.lower() = 0;
    tmp.upper() = 16;
    interval_store.affine_form_image(D, FP_Linear_Form(tmp));
    Con_FP_Expression con_d(0, 16);
    if (con_d.linearize(interval_store, lf_abstract_store, lk))
      ph.affine_form_image(D, lk);
    else
      ph.affine_form_image(D, FP_Linear_Form(interval_store.get_interval(D)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // S = Y;
    interval_store.affine_form_image(S, FP_Linear_Form(Y));
    Var_FP_Expression var_y(Y.id());
    if (var_y.linearize(interval_store, lf_abstract_store, ly))
      ph.affine_form_image(S, ly);
    else
      ph.affine_form_image(S, FP_Linear_Form(interval_store.get_interval(S)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // R = X - S;
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    interval_store.affine_form_image(R, FP_Linear_Form(X - S));
    if (x_dif_s.linearize(interval_store, lf_abstract_store, lr))
      ph.affine_form_image(R, lr);
    else
      ph.affine_form_image(R, FP_Linear_Form(interval_store.get_interval(R)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // Y = X;
    Var_FP_Expression var_x(X.id());
    interval_store.affine_form_image(Y, FP_Linear_Form(X));
    if (var_x.linearize(interval_store, lf_abstract_store, lx))
      ph.affine_form_image(Y, lx);
    else
      ph.affine_form_image(Y, FP_Linear_Form(interval_store.get_interval(Y)));
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // if (R <= -D)
    NNC_Polyhedron ph_then(ph);
    FP_Interval_Abstract_Store is_then(interval_store);
    is_then.refine_with_constraint(R <= -D);
    ph_then.refine_with_linear_form_inequality(lr, -lk);
    is_then.intersection_assign(FP_Interval_Abstract_Store(ph_then));

    // then Y = S - D;
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    is_then.affine_form_image(Y, FP_Linear_Form(S - D));
    if (s_dif_d.linearize(is_then, lf_abstract_store, ly))
      ph_then.affine_form_image(Y, ly);
    else
      ph_then.affine_form_image(Y, FP_Linear_Form(is_then.get_interval(Y)));
    is_then.intersection_assign(FP_Interval_Abstract_Store(ph_then));

    // else skip;
    interval_store.refine_with_constraint(R > -D);
    ph.generalized_refine_with_linear_form_inequality(-lk, lr, LESS_THAN);
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // LUB between then and else branches.
    ph.upper_bound_assign(ph_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    ph_then = ph;
    is_then = interval_store;
    is_then.refine_with_constraint(R >= D);
    ph_then.refine_with_linear_form_inequality(lk, lr);
    is_then.intersection_assign(FP_Interval_Abstract_Store(ph_then));

    // then Y = S + D;
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    is_then.affine_form_image(Y, FP_Linear_Form(S + D));
    if (s_sum_d.linearize(is_then, lf_abstract_store, ly))
      ph_then.affine_form_image(Y, ly);
    else
      ph_then.affine_form_image(Y, FP_Linear_Form(is_then.get_interval(Y)));
    is_then.intersection_assign(FP_Interval_Abstract_Store(ph_then));

    // else skip;
    ph.generalized_refine_with_linear_form_inequality(-lk, lr, LESS_THAN);
    interval_store.refine_with_constraint(R < D);
    interval_store.intersection_assign(FP_Interval_Abstract_Store(ph));

    // LUB between then and else branches.
    ph.upper_bound_assign(ph_then);
    interval_store.upper_bound_assign(is_then);
    print_constraints(interval_store, "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    ph.upper_bound_assign(ph_begin);
    interval_store.upper_bound_assign(is_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= M; Y >= -M}
    ph.limited_BHRZ03_extrapolation_assign(ph_begin, cs);
    interval_store.limited_CC76_extrapolation_assign(is_begin, cs);
    print_constraints(interval_store, "*** end loop ***");
    ++n;

  } while(is_begin != interval_store);

  tmp = interval_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return (tmp.lower() == -144 && tmp.upper() == 144);
}
*/

} // namespace

BEGIN_MAIN
  DO_TEST_F8(test01);
  DO_TEST_F8(test02);
  DO_TEST_F8(test03);
  DO_TEST_F64A(test04);

#define COND_float  PPL_CPP_EQ(PPL_CPP_FP_FORMAT(ANALYZER_FP_FORMAT), 1)
#define COND_double PPL_CPP_EQ(PPL_CPP_FP_FORMAT(ANALYZER_FP_FORMAT), 2)
#define COND_float_or_double PPL_CPP_OR(COND_float, COND_double)
#define PPL_CUSTOM_COND_32 \
  PPL_CPP_AND(PPL_CPP_EQ(PPL_CPP_LOGBITS, 5), COND_float)
#define PPL_CUSTOM_COND_64 \
  PPL_CPP_AND(PPL_CPP_EQ(PPL_CPP_LOGBITS, 6), COND_float_or_double)
#define PPL_CUSTOM_COND \
  PPL_CPP_OR(COND_F64, PPL_CPP_OR(PPL_CUSTOM_COND_32, PPL_CUSTOM_COND_64))

  COND_DO_TEST(PPL_CUSTOM_COND, test05);
/*
  COND_DO_TEST(PPL_CUSTOM_COND, test06);

  DO_TEST_F64(test07);
*/
END_MAIN
