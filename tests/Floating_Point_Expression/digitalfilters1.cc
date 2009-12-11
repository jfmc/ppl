/* Test Rate Limiter on differents abstract domains.
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

namespace {

using namespace Parma_Polyhedra_Library::IO_Operators;

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

// Tests rate limiter using intervals abstract domain
// and ignoring rounding errors.
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
  FP_Interval y(0);
  FP_Interval y_begin(1);
  FP_Interval_Abstract_Store as_begin;

  Constraint_System cs;
  PPL_DIRTY_TEMP_COEFFICIENT(M);
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);
  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  abstract_store.set_interval(Y, y);
  Box<FP_Interval> box(abstract_store);

  for(unsigned short n = 0; y != y_begin; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;
    y_begin = y;
    print_constraints(box, "*** begin loop ***");

    // X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    y.lower() = -128;
    y.upper() = 128;
    abstract_store.set_interval(X, y);

    y.lower() = 0;
    y.upper() = 16;
    abstract_store.set_interval(D, y);

    abstract_store.affine_image(S, Y);
    abstract_store.affine_image(R, X - S);
    abstract_store.affine_image(Y, X);

    // if (R <= -D) Y = S - D;
    FP_Interval_Abstract_Store as_then(abstract_store);
    as_then.refine_with_constraint(R <= -D);
    as_then.affine_image(Y, S - D);
    abstract_store.refine_with_constraint(R > -D);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_constraint(R >= D);
    as_then.affine_image(Y, S + D);
    abstract_store.refine_with_constraint(R > D);
    abstract_store.upper_bound_assign(as_then);

    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R >= D)  Y = S + D; ***");

    abstract_store.upper_bound_assign(as_begin);

    abstract_store.limited_CC76_extrapolation_assign(as_begin, cs);
    box = Box<FP_Interval>(abstract_store);
    print_constraints(box, "*** end loop ***");
    y = box.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return !y.is_bounded();
}

// Tests rate limiter using bounded differences abstract domain
// and ignoring rounding errors.
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

  FP_Interval_Abstract_Store abstract_store(5);
  FP_BD_Shape bd(abstract_store);
  FP_Interval y_begin(1);
  FP_Interval y(0);
  FP_BD_Shape bd_begin;

  Constraint_System cs;
  PPL_DIRTY_TEMP_COEFFICIENT(M);
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);
  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  bd.affine_image(Y, FP_Linear_Form(y));
  Box<FP_Interval> box(bd);

  for(unsigned short n = 0; y != y_begin; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    bd_begin = bd;
    y_begin = y;
    print_constraints(box, "*** begin loop ***");

    // X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    y.lower() = -128;
    y.upper() = 128;
    bd.affine_image(X, FP_Linear_Form(y));

    y.lower() = 0;
    y.upper() = 16;
    bd.affine_image(D, FP_Linear_Form(y));

    bd.affine_image(S, FP_Linear_Form(Y));
    bd.affine_image(R, FP_Linear_Form(X - S));
    bd.affine_image(Y, FP_Linear_Form(X));

    // if (R <= -D) Y = S - D;
    FP_BD_Shape bd_then(bd);
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    bd_then.affine_image(Y, FP_Linear_Form(S - D));

    bd.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                           FP_Linear_Form(R));
    bd.upper_bound_assign(bd_then);
    print_constraints(Box<FP_Interval>(bd),
         "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    bd_then = bd;
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    bd_then.affine_image(Y, FP_Linear_Form(S + D));

    bd.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));

    bd.upper_bound_assign(bd_then);
    print_constraints(Box<FP_Interval>(bd),
         "*** after if (R >= D)  Y = S + D; ***");

    bd.upper_bound_assign(bd_begin);

    bd.limited_BHMZ05_extrapolation_assign(bd_begin, cs);
    box = Box<FP_Interval>(bd);
    print_constraints(box, "*** end loop ***");
    y = box.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return !y.is_bounded();
}

// Tests rate limiter using octagons abstract domain
// and ignoring rounding errors.
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

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Octagonal_Shape oc(abstract_store);
  FP_Interval y(0);
  FP_Interval y_begin(1);
  FP_Octagonal_Shape oc_begin;

  Constraint_System cs;
  PPL_DIRTY_TEMP_COEFFICIENT(M);
  PPL_DIRTY_TEMP_COEFFICIENT(N);
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);
  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);
  div_2exp_assign_r(N, M, 1, ROUND_DOWN);
  // FIXME: By inserting the constraints Y <= M and Y >= -M, we obtain
  // Y + Y <= 2 * M = +inf and -Y - Y <= -2 * M = +inf.
  // For a more precise analysis, it is better to insert the
  // constraints Y <= N and Y >= -N, where N = M / 2.
  // However, we could take any value of N >= 136.
  cs.insert(Y <= N);
  cs.insert(Y >= -N);

  // Y = 0;
  oc.affine_image(Y, FP_Linear_Form(y));
  Box<FP_Interval> box(oc);

  for(unsigned short n = 0; y_begin != y; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;
    y_begin = y;
    print_constraints(box, "*** begin loop ***");

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    y.lower() = -128;
    y.upper() = 128;
    oc.affine_image(X, FP_Linear_Form(y));
    y.lower() = 0;
    y.upper() = 16;
    oc.affine_image(D, FP_Linear_Form(y));
    oc.affine_image(S, FP_Linear_Form(Y));
    oc.affine_image(R, FP_Linear_Form(X - S));
    oc.affine_image(Y, FP_Linear_Form(X));

    //if (R <= -D) Y = S - D;
    FP_Octagonal_Shape oc_then(oc);
    oc_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    oc_then.affine_image(Y, FP_Linear_Form(S - D));

    oc.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                           FP_Linear_Form(R));
    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc),
         "*** after if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    oc_then = oc;
    oc_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    oc_then.affine_image(Y, FP_Linear_Form(S + D));

    oc.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));

    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc),
         "*** after (R >= D)  Y = S + D; ***");

    oc.upper_bound_assign(oc_begin);

    oc.limited_BHMZ05_extrapolation_assign(oc_begin, cs);
    box = Box<FP_Interval>(oc);
    print_constraints(box, "*** end loop ***");
    y = box.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
}

// Tests rate limiter using polyhedra abstract domain
// and ignoring rounding errors.
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

  FP_Interval_Abstract_Store abstract_store(5);
  NNC_Polyhedron ph(abstract_store);
  FP_Interval y(0);
  FP_Interval y_begin(1);
  NNC_Polyhedron ph_begin;

  Constraint_System cs;
  PPL_DIRTY_TEMP_COEFFICIENT(M);
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
    -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
    * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                 - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);
  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();
  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);
  // FIXME: we could take any value of M >= 128.
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  // Y = 0;
  ph.affine_image(Y, FP_Linear_Form(y));
  Box<FP_Interval> box(ph);

  for(unsigned short n = 0; y_begin != y; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    ph_begin = ph;
    y_begin = y;
    print_constraints(box, "*** begin loop ***");

    // X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    y.lower() = -128;
    y.upper() = 128;
    ph.affine_image(X, FP_Linear_Form(y));
    y.lower() = 0;
    y.upper() = 16;
    ph.affine_image(D, FP_Linear_Form(y));
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(S, FP_Linear_Form(Y));
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(R, FP_Linear_Form(X - S));
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(Y, FP_Linear_Form(X));

    // if (R <= -D) Y = S - D;
    NNC_Polyhedron ph_then(ph);
    FP_Interval_Abstract_Store as_then(abstract_store);
    ph_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    ph_then.affine_image(Y, FP_Linear_Form(S - D));

    ph.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                           FP_Linear_Form(R));
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(ph),
         "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)  Y = S + D;
    ph_then = ph;
    as_then = abstract_store;
    ph_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    ph_then.affine_image(Y, FP_Linear_Form(S + D));

    ph.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(ph),
         "*** after if (R >= D)  Y = S + D; ***");

    ph.upper_bound_assign(ph_begin);

    ph.limited_BHRZ03_extrapolation_assign(ph_begin, cs);
    box = Box<FP_Interval>(ph);
    print_constraints(box, "*** end loop ***");
    y = box.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
}

// Tests rate limiter using bounded differences abstract domain and
// linearization of floating point expressions.
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

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval_Abstract_Store as_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  FP_BD_Shape bd(5);
  FP_BD_Shape bd_begin(5);
  FP_Interval y(0);
  FP_Interval y_begin(1);
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  PPL_DIRTY_TEMP_COEFFICIENT(M);
  // Here we compute the minimum value M between:
  //  - The biggest number representable in the analyzed format, and
  //  - The biggest number representable in the analyzer format.
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);

  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();

  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);
  // We have to check that Y not overflows.
  // FIXME: We could take any value of M >= 144.
  Constraint_System cs;
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  Con_FP_Expression con_y(0, 0);
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk.
  con_y.linearize(abstract_store, lf_abstract_store, lk);
  // Here we model the assignment Y = 0, invoking affine_image method.
  // FIXME: In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the bounded
  // differences domain.
  // Then, we consider the intersection between these abstract domains.
  bd.affine_image(Y, lk);
  abstract_store.set_interval(Y, tmp);

  // This loop iterate until the value of Y reaches a fixed point.
  for(unsigned short n = 0; y_begin != y; ++n) {

    // Iteration no. n+1: the abstract domains and the value of Y
    // are saved into the corresponding variables.
    nout << "*** n = " << n << " ***" << endl;
    bd_begin = bd;
    as_begin = abstract_store;
    y_begin = y;
    print_constraints(abstract_store, "*** before loop ***");

    // X = [-128, 128];
    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    bd.affine_image(X, lk);
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.set_interval(X, tmp);

    // D = [0, 16];
    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    bd.affine_image(D, lk);
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.set_interval(D, tmp);

    // Y = S;
    Var_FP_Expression var_y(Y.id());
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    bd.affine_image(S, ly);
    abstract_store.affine_image(S, Y);
    // Intersection between the values of the variables in
    // the BD_Shape and in the abstract store.
    abstract_store.intersection_assign(Box<FP_Interval>(bd));

    // R = X - S;
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    bd.affine_image(R, lr);
    abstract_store.affine_image(R, X - S);
    abstract_store.intersection_assign(Box<FP_Interval>(bd));

    // Y = X;
    Var_FP_Expression var_x(X.id());
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    bd.affine_image(Y, lx);
    abstract_store.affine_image(Y, X);
    abstract_store.intersection_assign(Box<FP_Interval>(bd));

    // if (R <= -D)
    FP_BD_Shape bd_then(bd);
    FP_Interval_Abstract_Store as_then(abstract_store);
    bd_then.refine_with_linear_form_inequality(lr, -lk);
    as_then.intersection_assign(Box<FP_Interval>(bd_then));

    // then Y = S - D;
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    bd_then.affine_image(Y, ly);
    as_then.affine_image(Y, S - D);
    as_then.intersection_assign(Box<FP_Interval>(bd_then));

    // else skip;
    bd.refine_with_linear_form_inequality(-lk, lr);
    abstract_store.intersection_assign(Box<FP_Interval>(bd));

    // LUB between then and else branches.
    bd.upper_bound_assign(bd_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    bd_then = bd;
    as_then = abstract_store;
    bd_then.refine_with_linear_form_inequality(lk, lr);
    as_then.intersection_assign(Box<FP_Interval>(bd_then));

    // then Y = S + D;
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    bd_then.affine_image(Y, ly);
    as_then.affine_image(Y, S + D);
    as_then.intersection_assign(Box<FP_Interval>(bd_then));

    // else skip;
    bd.refine_with_linear_form_inequality(lr, lk);
    abstract_store.intersection_assign(Box<FP_Interval>(bd));

    // LUB between then and else branches.
    bd.upper_bound_assign(bd_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    bd.upper_bound_assign(bd_begin);
    abstract_store.upper_bound_assign(as_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= M; Y >= -M}
    bd.limited_BHMZ05_extrapolation_assign(bd_begin, cs);
    abstract_store.limited_CC76_extrapolation_assign(as_begin, cs);

    print_constraints(abstract_store, "*** end loop ***");
    y = abstract_store.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
}

// Tests rate limiter using octagons abstract domain and
// linearization of floating point expressions.
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

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval_Abstract_Store as_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  FP_Octagonal_Shape oc(5);
  FP_Octagonal_Shape oc_begin(5);
  FP_Interval y(0);
  FP_Interval y_begin(1);
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  PPL_DIRTY_TEMP_COEFFICIENT(M);
  PPL_DIRTY_TEMP_COEFFICIENT(N);
  // Here we compute the minimum value M between:
  //  - The biggest number representable in the analyzed format, and
  //  - The biggest number representable in the analyzer format.
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);

  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();

  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);

  // FIXME: By inserting the constraints Y <= M and Y >= -M, we obtain
  // Y + Y <= 2 * M = +inf and -Y - Y <= -2 * M = +inf.
  // For a more precise analysis, it is better to insert the
  // constraints Y <= N and Y >= -N, where N = M / 2.
  // However, we could take any value of N such that 144 <= N <= M / 2.
  div_2exp_assign_r(N, M, 1, ROUND_DOWN);
  // We have to check that Y not overflows.
  Constraint_System cs;
  cs.insert(Y <= N);
  cs.insert(Y >= -N);

  Con_FP_Expression con_y(0, 0);
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk.
  con_y.linearize(abstract_store, lf_abstract_store, lk);
  // Here we model the assignment Y = 0, invoking affine_image method.
  // FIXME: In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the octagon
  // domain.
  // Then, we consider the intersection between these abstract domains.
  oc.affine_image(Y, lk);
  abstract_store.set_interval(Y, tmp);

  // This loop iterate until the value of Y reaches a fixed point.
  for(unsigned short n = 0; y_begin != y; ++n) {

    // Iteration no. n+1: the abstract domains and the value of Y
    // are saved into the corresponding variables.
    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;
    as_begin = abstract_store;
    y_begin = y;
    print_constraints(abstract_store, "*** before loop ***");

    // Y = [-128, 128].
    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(X, lk);
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.set_interval(X, tmp);

    // D = [0, 16].
    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(D, lk);
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.set_interval(D, tmp);

    // S = Y.
    Var_FP_Expression var_y(Y.id());
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    oc.affine_image(S, ly);
    abstract_store.affine_image(S, Y);
    // Intersection between the values of the variables in
    // the octagon and in the abstract store.
    abstract_store.intersection_assign(Box<FP_Interval>(oc));

    // R = X - S;
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    oc.affine_image(R, lr);
    abstract_store.affine_image(R, X - S);
    abstract_store.intersection_assign(Box<FP_Interval>(oc));

    // Y = X;
    Var_FP_Expression var_x(X.id());
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    oc.affine_image(Y, lx);
    abstract_store.affine_image(Y, X);
    abstract_store.intersection_assign(Box<FP_Interval>(oc));

    // if (R <= -D)
    FP_Octagonal_Shape oc_then(oc);
    FP_Interval_Abstract_Store as_then(abstract_store);
    oc_then.refine_with_linear_form_inequality(lr, -lk);
    as_then.intersection_assign(Box<FP_Interval>(oc_then));

    // then Y = S - D.
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    oc_then.affine_image(Y, ly);
    as_then.affine_image(Y, S - D);
    as_then.intersection_assign(Box<FP_Interval>(oc_then));

    // else skip.
    oc.refine_with_linear_form_inequality(-lk, lr);
    abstract_store.intersection_assign(Box<FP_Interval>(oc));

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    oc_then = oc;
    as_then = abstract_store;
    oc_then.refine_with_linear_form_inequality(lk, lr);
    as_then.intersection_assign(Box<FP_Interval>(oc_then));

    // then Y = S + D
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    oc_then.affine_image(Y, ly);
    as_then.affine_image(Y, S + D);
    as_then.intersection_assign(Box<FP_Interval>(oc_then));

    // else skip.
    oc.refine_with_linear_form_inequality(lr, lk);
    abstract_store.intersection_assign(Box<FP_Interval>(oc));

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    oc.upper_bound_assign(oc_begin);
    abstract_store.upper_bound_assign(as_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= N; Y >= -N}
    oc.limited_BHMZ05_extrapolation_assign(oc_begin, cs);
    abstract_store.limited_CC76_extrapolation_assign(as_begin, cs);

    print_constraints(abstract_store,"*** end loop ***");
    y = abstract_store.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
}

// Tests rate limiter using polyhedra abstract domain and
// linearization of floating point expressions.
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

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval_Abstract_Store as_begin(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  NNC_Polyhedron ph(5);
  NNC_Polyhedron ph_begin(5);
  FP_Interval y(0);
  FP_Interval y_begin(1);
  FP_Interval tmp(0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  PPL_DIRTY_TEMP_COEFFICIENT(M);
  // Here we compute the minimum value M between:
  //  - The biggest number representable in the analyzed format, and
  //  - The biggest number representable in the analyzer format.
  ANALYZER_FP_FORMAT max_analyzed =
    (2 - pow(2,
       -static_cast<ANALYZER_FP_FORMAT>(ANALYZED_FP_FORMAT::MANTISSA_BITS)))
       * pow(2, pow(2, ANALYZED_FP_FORMAT::EXPONENT_BITS)
                    - (ANALYZED_FP_FORMAT::EXPONENT_BIAS) - 2);

  ANALYZER_FP_FORMAT max_analyzer =
    std::numeric_limits<ANALYZER_FP_FORMAT>::max();

  max_analyzer = std::min(max_analyzer, max_analyzed);
  assign_r(M, max_analyzer, ROUND_DOWN);

  // We have to check that Y not overflows.
  // FIXME: We could take any value of M >= 144.
  Constraint_System cs;
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

  Con_FP_Expression con_y(0, 0);
  // The constant floating point expression con_y is linearized into
  // the interval linear form lk.
  con_y.linearize(abstract_store, lf_abstract_store, lk);
  // Here we model the assignment Y = 0, invoking affine_image method.
  // FIXME: In order to refine the analysis, all the transer function are
  // performed in parallel in the interval domain and in the poyhedra domain.
  // Then, we consider the intersection between these abstract domains.
  ph.affine_image(Y, lk);
  abstract_store.set_interval(Y, tmp);

  // This loop iterate until the value of Y reaches a fixed point.
  for(unsigned short n = 0; y_begin != y; ++n) {

    // Iteration no. n+1: the abstract domains and the value of Y
    // are saved into the corresponding variables.
    nout << "*** n = " << n << " ***" << endl;
    ph_begin = ph;
    as_begin = abstract_store;
    y_begin = y;
    print_constraints(abstract_store, "*** before loop ***");

    // X = [-128, 128];
    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    ph.affine_image(X, lk);
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.set_interval(X, tmp);

    // D = [0, 16];
    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    ph.affine_image(D, lk);
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.set_interval(D, tmp);

    // S = Y;
    Var_FP_Expression var_y(Y.id());
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    ph.affine_image(S, ly);
    abstract_store.affine_image(S, Y);
    // Intersection between the values of the variables in
    // the polyhedron and in the abstract store.
    abstract_store.intersection_assign(Box<FP_Interval>(ph));

    // R = X - S;
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    ph.affine_image(R, lr);
    abstract_store.affine_image(R, X - S);
    abstract_store.intersection_assign(Box<FP_Interval>(ph));

    // Y = X;
    Var_FP_Expression var_x(X.id());
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    ph.affine_image(Y, lx);
    abstract_store.affine_image(Y, X);
    abstract_store.intersection_assign(Box<FP_Interval>(ph));

    // if (R <= -D)
    NNC_Polyhedron ph_then(ph);
    FP_Interval_Abstract_Store as_then(abstract_store);
    ph_then.refine_with_linear_form_inequality(lr, -lk);
    as_then.intersection_assign(Box<FP_Interval>(ph_then));

    // then Y = S - D;
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    ph_then.affine_image(Y, ly);
    as_then.affine_image(Y, S - D);
    as_then.intersection_assign(Box<FP_Interval>(ph_then));

    // else skip;
    ph.refine_with_linear_form_inequality(-lk, lr);
    abstract_store.intersection_assign(Box<FP_Interval>(ph));

    // LUB between then and else branches.
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D)
    ph_then = ph;
    as_then = abstract_store;
    ph_then.refine_with_linear_form_inequality(lk, lr);
    as_then.intersection_assign(Box<FP_Interval>(ph_then));

    // then Y = S + D;
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    ph_then.affine_image(Y, ly);
    as_then.affine_image(Y, S + D);
    as_then.intersection_assign(Box<FP_Interval>(ph_then));

    // else skip;
    ph.refine_with_linear_form_inequality(lr, lk);
    abstract_store.intersection_assign(Box<FP_Interval>(ph));

    // LUB between then and else branches.
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store,
         "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    ph.upper_bound_assign(ph_begin);
    abstract_store.upper_bound_assign(as_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= M; Y >= -M}
    ph.limited_BHRZ03_extrapolation_assign(ph_begin, cs);
    abstract_store.limited_CC76_extrapolation_assign(as_begin, cs);

    print_constraints(abstract_store,"*** end loop ***");
    y = abstract_store.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
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
