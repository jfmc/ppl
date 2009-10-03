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

// tests rate limiter using intervals abstract domain
// and ignoring rounding errors.
bool
test01() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store as_begin;

  // Y = 0;
  abstract_store.set_interval(Y, tmp);

  unsigned short N = 5;
  for(unsigned short n = 0; n < N; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    as_begin = abstract_store;

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    tmp.lower() = -128;
    tmp.upper() = 128;
    abstract_store.set_interval(X, tmp);
    tmp.lower() = 0;
    tmp.upper() = 16;
    abstract_store.set_interval(D, tmp);
    abstract_store.set_interval(S, abstract_store.get_interval(Y));
    abstract_store.set_interval(R, abstract_store.get_interval(X)
                                  -abstract_store.get_interval(S));
    abstract_store.set_interval(Y, abstract_store.get_interval(X));

    //if (R <= -D) Y = S - D;
    FP_Interval_Abstract_Store as_then(abstract_store);
    as_then.refine_with_constraint(R <= -D);
    as_then.set_interval(Y, abstract_store.get_interval(S)
                           -abstract_store.get_interval(D));
    abstract_store.refine_with_constraint(R > -D);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store ,"*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    as_then = abstract_store;
    as_then.refine_with_constraint(R >= D);
    as_then.set_interval(Y, abstract_store.get_interval(S)
                          + abstract_store.get_interval(D));
    abstract_store.refine_with_constraint(R > D);
    abstract_store.upper_bound_assign(as_then);

    abstract_store.upper_bound_assign(as_then);
    print_constraints(abstract_store ,"*** if (R >= D)  Y = S + D; ***");
  }

  nout << "*** Y in [-128 - 16n, 128 + 16n] ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using bounded differences abstract domain
// and ignoring rounding errors.
bool
test02() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_BD_Shape bd(abstract_store);
  FP_Interval tmp(0);
  FP_BD_Shape bd_begin;

  // Y = 0;
  bd.affine_image(Y, FP_Linear_Form(tmp));

  for(unsigned short n = 0; n < 5; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    bd_begin = bd;

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    tmp.lower() = -128;
    tmp.upper() = 128;
    bd.affine_image(X, FP_Linear_Form(tmp));
    tmp.lower() = 0;
    tmp.upper() = 16;
    bd.affine_image(D, FP_Linear_Form(tmp));
    bd.affine_image(S, FP_Linear_Form(Y));
    bd.affine_image(R, FP_Linear_Form(X - S));
    bd.affine_image(Y, FP_Linear_Form(X));

    //if (R <= -D) Y = S - D;
    FP_BD_Shape bd_then(bd);
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D));
    bd_then.affine_image(Y, FP_Linear_Form(S - D));

    bd.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                           FP_Linear_Form(R));
    bd.upper_bound_assign(bd_then);
    print_constraints(Box<FP_Interval>(bd) ,
         "*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    bd_then = bd;
    bd_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    bd_then.affine_image(Y, FP_Linear_Form(S + D));

    bd.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));

    bd.upper_bound_assign(bd_then);
    print_constraints(Box<FP_Interval>(bd) ,
         "*** if (R >= D)  Y = S + D; ***");
  }

  nout << "*** Y in [-16 - 16n, 128 + 16n] ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using octagons abstract domain
// and ignoring rounding errors.
bool
test03() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Octagonal_Shape oc(abstract_store);
  FP_Interval tmp(0);
  FP_Octagonal_Shape oc_begin;

  // Y = 0;
  oc.affine_image(Y, FP_Linear_Form(tmp));

  for(unsigned short n = 0; oc_begin != oc; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    tmp.lower() = -128;
    tmp.upper() = 128;
    oc.affine_image(X, FP_Linear_Form(tmp));
    tmp.lower() = 0;
    tmp.upper() = 16;
    oc.affine_image(D, FP_Linear_Form(tmp));
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
    print_constraints(Box<FP_Interval>(oc) ,
         "*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    oc_then = oc;
    oc_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R));
    oc_then.affine_image(Y, FP_Linear_Form(S + D));

    oc.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D));

    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc) ,
         "*** if (R >= D)  Y = S + D; ***");
  }

  oc.refine_fp_interval_abstract_store(abstract_store);
  tmp = abstract_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using polyhedra abstract domain
// and ignoring rounding errors.
bool
test04() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  NNC_Polyhedron ph(abstract_store);
  FP_Interval tmp(0);
  NNC_Polyhedron ph_begin;

  // Y = 0;
  ph.affine_image(Y, FP_Linear_Form(tmp), abstract_store);

  for(unsigned short n = 0; ph_begin != ph; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    ph_begin = ph;

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    tmp.lower() = -128;
    tmp.upper() = 128;
    ph.affine_image(X, FP_Linear_Form(tmp), abstract_store);
    tmp.lower() = 0;
    tmp.upper() = 16;
    ph.affine_image(D, FP_Linear_Form(tmp), abstract_store);
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(S, FP_Linear_Form(Y), abstract_store);
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(R, FP_Linear_Form(X - S), abstract_store);
    abstract_store = Box<FP_Interval>(ph);
    ph.affine_image(Y, FP_Linear_Form(X), abstract_store);

    //if (R <= -D) Y = S - D;
    NNC_Polyhedron ph_then(ph);
    FP_Interval_Abstract_Store as_then(abstract_store);
    ph_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                              -FP_Linear_Form(D),
                                                        as_then);
    ph_then.affine_image(Y, FP_Linear_Form(S - D), as_then);

    ph.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                           FP_Linear_Form(R),
                                             abstract_store);
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(ph) ,
         "*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    ph_then = ph;
    as_then = abstract_store;
    ph_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                               FP_Linear_Form(R),
                                               as_then);
    ph_then.affine_image(Y, FP_Linear_Form(S + D), as_then);

    ph.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                          FP_Linear_Form(D),
                                          abstract_store);
    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(ph) ,
         "*** if (R >= D)  Y = S + D; ***");
  }

  tmp = abstract_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using octagons abstract domain and
// linearization of floating point expressions.
bool
test05() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  FP_Octagonal_Shape oc(abstract_store);
  FP_Interval tmp(0);
  FP_Octagonal_Shape oc_begin;
  Con_FP_Expression con_y(0, 0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  // Y = 0;
  con_y.linearize(abstract_store, lf_abstract_store, lk);
  oc.affine_image(Y, lk);

  FP_Interval threshold(-144);
  threshold.join_assign(144);

  for(unsigned short n = 0; n < 10 /* !tmp.contains(threshold) */; ++n) {

    nout << "*** n = " << n << " ***" << endl;
    oc_begin = oc;

    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(X, lk);

    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    oc.affine_image(D, lk);

    Var_FP_Expression var_y(2);
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    oc.affine_image(S, ly);

    Var_FP_Expression* px = new Var_FP_Expression(0);
    Var_FP_Expression* ps = new Var_FP_Expression(3);
    Dif_FP_Expression x_dif_s(px, ps);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    oc.affine_image(R, lr);

    Var_FP_Expression var_x(0);
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    oc.affine_image(Y, lx);

    //if (R <= -D) Y = S - D;
    FP_Octagonal_Shape oc_then(oc);
    oc_then.refine_with_linear_form_inequality(lr, -lk);

    Var_FP_Expression* pd  = new Var_FP_Expression(1);
    Var_FP_Expression* ps2 = new Var_FP_Expression(3);
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    oc_then.affine_image(Y, ly);

    oc.refine_with_linear_form_inequality(-lk, lr);

    oc.upper_bound_assign(oc_then);
    print_constraints(Box<FP_Interval>(oc) ,
         "*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    oc_then = oc;
    oc_then.refine_with_linear_form_inequality(lk, lr);

    Var_FP_Expression* pd1  = new Var_FP_Expression(1);
    Var_FP_Expression* ps3  = new Var_FP_Expression(3);
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    oc_then.affine_image(Y, ly);

    oc.refine_with_linear_form_inequality(lr, lk);

    oc.upper_bound_assign(oc_then);
    Box<FP_Interval> box(oc);
    print_constraints(box, "*** if (R >= D)  Y = S + D; ***");
    tmp = box.get_interval(Y);
  }

  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using polyhedra abstract domain and
// linearization of floating point expressions.
bool
test06() {
  //input signal
  Variable X(0);
  //maximum allowed for |R|
  Variable D(1);
  //output signal
  Variable Y(2);
  //last output
  Variable S(3);
  //actual rate
  Variable R(4);

  FP_Interval_Abstract_Store abstract_store(5);
  FP_Linear_Form_Abstract_Store lf_abstract_store;
  NNC_Polyhedron ph(abstract_store);
  FP_Interval tmp(0);
  NNC_Polyhedron ph_begin;
  Con_FP_Expression con_y(0, 0);
  FP_Linear_Form lx;
  FP_Linear_Form ly;
  FP_Linear_Form lr;
  FP_Linear_Form lk;

  // Y = 0;
  con_y.linearize(abstract_store, lf_abstract_store, lk);
  ph.affine_image(Y, lk, abstract_store);

  //FIXME: Can the threshold be more precise than [-144, 144]?
  FP_Interval threshold(-144);
  threshold.join_assign(144);

  for(unsigned short n = 0; !tmp.contains(threshold); ++n) {

    nout << "*** n = " << n << " ***" << endl;
    ph_begin = ph;

    //X = [-128, 128]; D = [0, 16]; S = Y; R = X - S; Y = X;
    Con_FP_Expression con_x(-128, 128);
    con_x.linearize(abstract_store, lf_abstract_store, lk);
    ph.affine_image(X, lk, abstract_store);

    Con_FP_Expression con_d(0, 16);
    con_d.linearize(abstract_store, lf_abstract_store, lk);
    ph.affine_image(D, lk, abstract_store);

    Var_FP_Expression var_y(2);
    abstract_store = Box<FP_Interval>(ph);
    var_y.linearize(abstract_store, lf_abstract_store, ly);
    ph.affine_image(S, ly, abstract_store);

    Var_FP_Expression* px = new Var_FP_Expression(0);
    Var_FP_Expression* ps = new Var_FP_Expression(3);
    Dif_FP_Expression x_dif_s(px, ps);
    abstract_store = Box<FP_Interval>(ph);
    x_dif_s.linearize(abstract_store, lf_abstract_store, lr);
    ph.affine_image(R, lr, abstract_store);

    Var_FP_Expression var_x(0);
    abstract_store = Box<FP_Interval>(ph);
    var_x.linearize(abstract_store, lf_abstract_store, lx);
    ph.affine_image(Y, lx, abstract_store);

    //if (R <= -D) Y = S - D;
    NNC_Polyhedron ph_then(ph);
    FP_Interval_Abstract_Store as_then(abstract_store);
    ph_then.refine_with_linear_form_inequality(lr, -lk, as_then);
    Var_FP_Expression* pd  = new Var_FP_Expression(1);
    Var_FP_Expression* ps2 = new Var_FP_Expression(3);
    Dif_FP_Expression s_dif_d(ps2, pd);
    s_dif_d.linearize(abstract_store, lf_abstract_store, ly);
    ph_then.affine_image(Y, ly, abstract_store);

    ph.refine_with_linear_form_inequality(-lk, lr, abstract_store);

    ph.upper_bound_assign(ph_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(ph) ,
         "*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    ph_then = ph;
    as_then = abstract_store;
    ph_then.refine_with_linear_form_inequality(lk, lr, as_then);

    Var_FP_Expression* pd1  = new Var_FP_Expression(1);
    Var_FP_Expression* ps3  = new Var_FP_Expression(3);
    Sum_FP_Expression s_sum_d(ps3, pd1);
    s_sum_d.linearize(abstract_store, lf_abstract_store, ly);
    ph_then.affine_image(Y, ly, as_then);

    ph.refine_with_linear_form_inequality(lr, lk, abstract_store);

    ph.upper_bound_assign(ph_then);
    Box<FP_Interval> box(ph);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(box, "*** if (R >= D)  Y = S + D; ***");
    tmp = box.get_interval(Y);
  }

  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
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
