/* Test Pointset_Powerset<Grid>.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
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

bool
test01() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence((x %= 1) / 3);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  Grid known_gr1(2);
  known_gr1.add_congruence((x %= 2) / 3);

  Grid known_gr2(2);
  known_gr2.add_congruence((x %= 0) / 3);

  Pointset_Powerset<Grid>::iterator i = ps.begin();
  bool ok = (i->element() == known_gr1);
  return (ok && (++i)->element() == known_gr2);
}

bool
test02() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence(x == 1);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  Grid known_gr(2);
  known_gr.add_congruence((x %= 0) / 1);

  Pointset_Powerset<Grid>::iterator i = ps.begin();
  return (i->element() == known_gr);
}

bool
test03() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x == 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence(x %= 1);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  return (ps.begin() == ps.end());
}

bool
test04() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence(y %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  Grid known_gr(2);
  known_gr.add_congruence((x %= 0) / 1);

  Pointset_Powerset<Grid>::iterator i = ps.begin();
  return (i->element() == known_gr);
}

bool
test05() {
  Variable x(0);
  Variable y(1);

  Pointset_Powerset<Grid> ps(2, EMPTY);

  Grid q(2);
  q.add_congruence(y %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  return (ps.begin() == ps.end());
}

bool
test06() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(y %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Pointset_Powerset<Grid> qs(2, EMPTY);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  ps.poly_difference_assign(qs);
  print_congruences(ps, "*** ps.poly_difference(qs) ***");

  Grid known_gr(p);

  Pointset_Powerset<Grid>::iterator i = ps.begin();
  return (i->element() == known_gr);
}

bool
test07() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence((x %= 1) / 3);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return ps.geometrically_covers(qs) && !qs.geometrically_covers(ps);
}

bool
test08() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence(x == 1);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return ps.geometrically_covers(qs) && !qs.geometrically_covers(ps);
}

bool
test09() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p);

  Grid q(2);
  q.add_congruence(y %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return !ps.geometrically_covers(qs) && !qs.geometrically_covers(ps);
}

bool
test10() {
  Variable x(0);
  Variable y(1);

  Pointset_Powerset<Grid> ps(2, EMPTY);

  Grid q(2);
  q.add_congruence(y %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return !ps.geometrically_covers(qs) && qs.geometrically_covers(ps);
}

bool
test11() {
  Variable x(0);
  Variable y(1);

  Grid p1(2);
  p1.add_congruence((x %= 0) / 2);
  Grid p2(2);
  p2.add_congruence((x %= 1) / 2);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p1);
  ps.add_disjunct(p2);

  Grid q(2);
  q.add_congruence(x %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return ps.geometrically_equals(qs);
}

bool
test12() {
  Variable x(0);
  Variable y(1);

  Grid p1(2);
  p1.add_congruence((x %= 0) / 3);
  Grid p2(2);
  p2.add_congruence((x %= 1) / 3);

  Pointset_Powerset<Grid> ps(2, EMPTY);
  ps.add_disjunct(p1);
  ps.add_disjunct(p2);

  Grid q(2);
  q.add_congruence(x %= 0);

  Pointset_Powerset<Grid> qs(2, EMPTY);
  qs.add_disjunct(q);

  print_congruences(ps, "*** ps ***");
  print_congruences(qs, "*** qs ***");

  return !ps.geometrically_equals(qs);
}

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
END_MAIN
