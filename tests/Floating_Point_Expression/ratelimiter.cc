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

// Tests rate limiter using octagons abstract domain.
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
  FP_Linear_Form ls;

  // M represents the threshold: the smallest integer larger than
  // M0 computed in Miné's thèse
  PPL_DIRTY_TEMP_COEFFICIENT(M);
  assign_r(M, 145, ROUND_DOWN);

  Constraint_System cs;
  cs.insert(Y <= M);
  cs.insert(Y >= -M);

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
  Box<FP_Interval> box(oc);

  // This loop iterate until the value of Y reaches a fixed point.
  for(unsigned short n = 0; y_begin != y; ++n) {

    // Iteration no. n+1: the abstract domains and the value of Y
    // are saved into the corresponding variables.
    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;
    y_begin = y;
    print_constraints(Box<FP_Interval>(oc), "*** before loop ***");

    // X = [-128, 128].
    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(X, lk);

    // D = [0, 16].
    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(D, lk);

    // S = Y.
    Var_FP_Expression var_y(Y.id());
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    var_y.linearize(abstract_store, lf_abstract_store, ls);
    oc.affine_image(S, ly);

    // R = X - S.
    Var_FP_Expression* px = new Var_FP_Expression(X.id());
    Var_FP_Expression* ps = new Var_FP_Expression(S.id());
    Dif_FP_Expression x_dif_s(px, ps);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    oc.affine_image(R, lr);

    // Y = X.
    Var_FP_Expression var_x(X.id());
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    oc.affine_image(Y, lx);


    // if (R <= -D).
    FP_Octagonal_Shape oc_then(oc);
    oc_then.refine_with_linear_form_inequality(lr, -lk);

    // then Y = S - D.
    Var_FP_Expression* pd  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps2 = new Var_FP_Expression(S.id());
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    // Y <= S holds.
    oc_then.refine_with_linear_form_inequality(ly, ls);
    oc_then.affine_image(Y, ly);
    // else skip.
    oc.refine_with_linear_form_inequality(-lk, lr);

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc),
		      "*** after if (R <= -D) Y = S - D; ***");

    // if (R >= D).
    oc_then = oc;
    oc_then.refine_with_linear_form_inequality(lk, lr);
    // then Y = S + D.
    Var_FP_Expression* pd1  = new Var_FP_Expression(D.id());
    Var_FP_Expression* ps3  = new Var_FP_Expression(S.id());
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    // -S <= Y holds.
    oc_then.refine_with_linear_form_inequality(-ls, ly);
    oc_then.affine_image(Y, ly);
    
    // else skip.
    oc.refine_with_linear_form_inequality(lr, lk);

    // LUB between then and else branches.
    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc),
		      "*** after if (R >= D)  Y = S + D; ***");

    // LUB between the actual abstract domains and the corresponding
    // domains at the beginning of the loop.
    oc.upper_bound_assign(oc_begin);

    // Limited extrapolation: we enforce the satisfaction
    // of the constraint system cs = {Y <= N; Y >= -N}
    oc.limited_BHMZ05_extrapolation_assign(oc_begin, cs);
    box = Box<FP_Interval>(oc);
    print_constraints(box,"*** end loop ***");
    y = box.get_interval(Y);
  }

  nout << "*** Y in " << y << " ***" << endl;
  return y.is_bounded();
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
