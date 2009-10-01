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

// tests rate limiter using intervals abstract domain.
bool
test01() {
  Variable X(0); //input
  Variable D(1); //input
  Variable Y(2); //output
  Variable S(3); //last output
  Variable R(4); //actual rate
  FP_Interval_Abstract_Store abstract_store(5);
  FP_Interval tmp(0);
  FP_Interval_Abstract_Store as_begin;

  // Y = 0;
  abstract_store.set_interval(Y, tmp);

  unsigned int N = 5;
  for(unsigned int n = 0; n < N; ++n) {

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

  tmp = abstract_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
}

// tests rate limiter using bounded differences abstract domain.
bool
test02() {

  return true;
}

// tests rate limiter using octagons abstract domain and
bool
test03() {
  Variable X(0); //input
  Variable D(1); //input
  Variable Y(2); //output
  Variable S(3); //last output
  Variable R(4); //actual rate
  FP_Interval_Abstract_Store abstract_store(5);
  FP_Octagonal_Shape oc(abstract_store);
  FP_Interval tmp(0);
  FP_Octagonal_Shape oc_begin;

  // Y = 0;
  oc.affine_image(Y, FP_Linear_Form(tmp));

  for(unsigned int n = 0; oc_begin != oc; ++n) {

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
    FP_Interval_Abstract_Store as_then(abstract_store);
    oc_then.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                            -FP_Linear_Form(D));
    oc_then.affine_image(Y, FP_Linear_Form(S - D));

    oc.refine_with_linear_form_inequality(-FP_Linear_Form(D),
                                         FP_Linear_Form(R));
    oc.upper_bound_assign(oc_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(oc) ,"*** if (R <= -D) Y = S - D; ***");

    //if (R >= D)  Y = S + D;
    oc_then = oc;
    as_then = abstract_store;
    oc_then.refine_with_linear_form_inequality(FP_Linear_Form(D),
                                             FP_Linear_Form(R));
    oc_then.affine_image(Y, FP_Linear_Form(S + D));

    oc.refine_with_linear_form_inequality(FP_Linear_Form(R),
                                        FP_Linear_Form(D));

    oc.upper_bound_assign(oc_then);
    abstract_store.upper_bound_assign(as_then);
    print_constraints(Box<FP_Interval>(oc) ,"*** if (R >= D)  Y = S + D; ***");
  }

  oc.refine_fp_interval_abstract_store(abstract_store);
  tmp = abstract_store.get_interval(Y);
  nout << "*** Y in " << tmp << " ***" << endl;
  return tmp.is_bounded();
}

bool
test04() {

  return true;
}


} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  //DO_TEST(test02);
  DO_TEST(test03);
  //DO_TEST(test04);
END_MAIN
