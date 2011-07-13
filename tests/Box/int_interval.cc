/* Test about operator bitwise on Integer Interval.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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
#include <complex>
#include <cmath>
#include <cstdio>

namespace {

template <typename I>
struct My_Interval {

  struct Integer_Real_Interval_Info_Policy {
    const_bool_nodef(store_special, false);
    const_bool_nodef(store_open, true);
    const_bool_nodef(cache_empty, true);
    const_bool_nodef(cache_singleton, true);
    const_bool_nodef(cache_normalized, false);
    const_int_nodef(next_bit, 0);
    const_bool_nodef(may_be_empty, false);
    const_bool_nodef(may_contain_infinity, false);
    const_bool_nodef(check_empty_result, false);
    const_bool_nodef(check_inexact, false);
  };

  typedef Interval_Restriction_None
  <Interval_Info_Bitset<unsigned int,
                        Integer_Real_Interval_Info_Policy> >
  Integer_Real_Interval_Info;

  typedef Interval<I, Integer_Real_Interval_Info> interval_type;
};

/*
  Test about the operations of intervals with the same sign
  and positive sign, bitwise operator are: OR, AND and XOR.
*/
template<typename I>
bool
test01() {
  typename My_Interval<I>::interval_type x_or;
  x_or = 2;
  typename My_Interval<I>::interval_type x_and;
  x_and = 2;
  typename My_Interval<I>::interval_type x_xor;
  x_xor = 2;
  typename My_Interval<I>::interval_type two;
  two = 2;
  typename My_Interval<I>::interval_type y;

  nout << "*** OR ***" << endl;
  for (int i = 0; i <= 50; ++i) {
    /*
      If i is odd:
        Compute: x_or | [i, (2^i)/2]
      Else
        Compute: x_or | 2^i
      Finally:
        x_or = x_or * y
    */
    mpz_class tmp(pow(2,i));
    y = tmp;
    if (!(i % 2)) {
      y /= two;
      y.join_assign(i);
    }
    if (x_or.lower() > x_or.upper() || y.lower() > y.upper())
      break;
    nout << "*** " << x_or << " | " << y << " ***" << endl;
    x_or |= y;
    nout << "*** approximate with ***" << endl << x_or << endl << endl;
    x_or *= y;
  }

  nout << endl << "*** AND ***" << endl;
  for (int i = 0; i <= 50; ++i) {
    /*
      If i is odd:
        Compute: x_and & [i/2, i]
      Else
        Compute: x_and & i
      Finally:
        x_and = x_and + y
    */
    y = i;
    if (!(i % 2)) {
      y /= two;
      y.join_assign(i);
    }
    if (x_and.lower() > x_and.upper() || y.lower() > y.upper())
      break;
    nout << "*** " << x_and << " & " << y << " ***" << endl;
    x_and &= y;
    nout << "*** approximate with ***" << endl << x_and << endl << endl;
    x_and += y;
  }

  nout << endl << "*** XOR ***" << endl;
  for (int i = 0; i <= 50; ++i) {
    /*
      If i is odd:
        Compute: x_xor ^ [i/2, i]
      Else
        Compute: x_xor ^ i
      Finally:
        x_xor = x_xor + y
    */
    y = i;
    if (!(i % 2)) {
      y /= two;
      y.join_assign(i);
    }
    if (x_xor.lower() > x_xor.upper() || y.lower() > y.upper())
      break;
    nout << "*** " << x_xor << " ^ " << y << " ***" << endl;
    x_xor ^= y;
    nout << "*** approximate with ***" << endl << x_xor << endl << endl;
    x_xor += y;
  }

  return true;
}

/*
  Test about the operations on the limits of the typename I,
  bitwise operator are: OR, AND and XOR.
*/
template <typename I>
bool test02() {
  typename My_Interval<I>::interval_type x;
  x = 2;
  typename My_Interval<I>::interval_type y;

  typename My_Interval<I>::interval_type x_or;

  typename My_Interval<I>::interval_type x_and;

  typename My_Interval<I>::interval_type x_xor;

  typename My_Interval<I>::interval_type max;

  typename My_Interval<I>::interval_type min;

  max = std::numeric_limits<I>::max();

  min = std::numeric_limits<I>::min();

  /*
    Compute: x_or = numeric_limits<I>::max() | numeric_limits<I>::max() -2
  */
  y = max;
  x_or = max;
  y -= x;
  x_or |= y;
  nout << "*** " << max << " | " << y << " *** "
       << endl << "*** result *** " << endl << x_or << endl;

  /*
    Compute: x_or = numeric_limits<I>::min() | numeric_limits<I>::max() - 2
  */
  x_or = min;
  x_or |= y;
  nout << "*** " << min << " | " << y << " ***"
       << endl << "*** result ***" << endl  << x_or << endl;

  /*
    Compute: x_or = numeric_limits<I>::min() | numeric_limits<I>::min()
  */
  x_or = min;
  x_or |= x_or;
  nout << "*** " << min << " | " << min << " ***"
       << endl << "*** result ***" << endl  << x_or << endl << endl;

  /*
    Compute: x_and = numeric_limits<I>::max() & numeric_limits<I>::max() -2
  */
  y = max;
  x_and = max;
  y -= x;
  x_and &= y;
  nout << "*** " << max << " & " << y << " *** "
       << endl << "*** result *** " << endl << x_and  << endl;

  /*
    Compute: x_and = numeric_limits<I>::min() & numeric_limits<I>::max() - 2
  */
  x_and = min;
  x_and &= y;
  nout << "*** " << min << " & " << y << " ***"
       << endl << "*** result ***" << endl  << x_and << endl;

  /*
    Compute: x_and = numeric_limits<I>::min() & numeric_limits<I>::min()
  */
  x_and = min;
  x_and &= x_and;
  nout << "*** " << min << " & " << min << " ***"
       << endl << "*** result ***" << endl  << x_and << endl << endl;

  /*
    Compute: x_xor = numeric_limits<I>::max() ^ numeric_limits<I>::max() -2
  */
  y = max;
  x_xor = max;
  y -= x;
  x_xor ^= y;
  nout << "*** " << max << " ^ " << y << " *** "
       << endl << "*** result *** " << endl << x_xor << endl;

  /*
    Compute: x_xor = numeric_limits<I>::min() ^ numeric_limits<I>::max() - 2
  */
  x_xor = min;
  x_xor ^= y;
  nout << "*** " << min << " ^ " << y << " ***"
       << endl << "*** result ***" << endl  << x_xor << endl;

  /*
    Compute: x_xor = numeric_limits<I>::min() ^ numeric_limits<I>::min()
  */
  x_xor = min;
  x_xor ^= x_xor;
  nout << "*** " << min << " ^ " << min << " ***"
       << endl << "*** result ***" << endl  << x_xor << endl;

  return true;
}

/*
  Test about the operations of intervals with discordant sign,
  bitwise operator are: OR, AND and XOR.
*/
template <typename I>
bool test03() {
  typename My_Interval<I>::interval_type x_or;
  x_or = 2;
  typename My_Interval<I>::interval_type x_and;
  x_and = 2;
  typename My_Interval<I>::interval_type x_xor;
  x_xor = 2;
  typename My_Interval<I>::interval_type two;
  two = 2;
  typename My_Interval<I>::interval_type y;

  typename My_Interval<I>::interval_type z;

  nout << "*** OR ***"<< endl;
  for (int i = 0; i <= 50; ++i) {
    /*
      Compute: x_or = x_or*two | -i*xor
    */
    y = -i;
    x_or *= two;
    y *= x_or;
    if (x_or.lower() > x_or.upper() || y.lower() > y.upper())
      break;
    nout << "*** " << x_or << " | " << y << " ***" << endl;
    x_or |= y;
    nout << "*** approximate with ***" << endl << x_or << endl << endl;
  }

  nout << endl <<"*** AND ***" << endl;
  for (int i = 1; i <= 10; ++i) {
    for (int j = 10; j >= 1; --j) {
      /*
	Compute: x_and = x_and & -[2^i, 2^j]
	Finally:
	  x_and = x_and + [2^i, 2^j]
      */
      y.assign(pow(2,j));
      y.join_assign(pow(2,i));
      y.neg_assign(y);
      if (x_and.lower() > x_and.upper() || y.lower() > y.upper())
	break;
      nout << "*** " << x_and << " & " << y << " ***" << endl;
      x_and &= y;
      nout << "*** approximate with ***" << endl << x_and << endl << endl;
      y.neg_assign(y);
      x_and += y;
    }
  }

  nout << endl <<"*** XOR ***" << endl;
  for (int i = 1; i <= 10; ++i){
    for (int j = 10; j >= 1; --j) {
      /*
	Compute: x_xor = x_and & -[2^i, 2^j]
	Finally:
	  x_xor = (x_xor + y) * y
      */
      y = i;
      y.join_assign(pow(2,j));
      y.neg_assign(y);
      if (x_xor.lower() > x_xor.upper() || y.lower() > y.upper())
	break;
      nout << "*** " << x_xor << " ^ " << y << " ***" << endl;
      x_xor ^= y;
      nout << "*** approximate with ***" << endl << x_xor << endl << endl;
      x_xor += y;
      x_xor *= y;
    }
  }

  return true;
}

/*
  Test about the operations of intervals with the same sign
  and negative sign, bitwise operator are: OR, AND and XOR.
*/
template <typename I>
bool test04() {
  typename My_Interval<I>::interval_type x_or;
  x_or = 2;
  typename My_Interval<I>::interval_type x_and;
  x_and = 2;
  typename My_Interval<I>::interval_type x_xor;
  x_xor = 2;
  typename My_Interval<I>::interval_type two;
  two = 2;
  typename My_Interval<I>::interval_type y;

  typename My_Interval<I>::interval_type z;

  nout << "*** OR ***"<< endl;
  for (int i = 1; i <= 50; ++i) {
    /*
      If -i*x_or >= 0:
        Compute: x_or = x_or*two | -(-i*x_or)
      Else
        Compute: x_or = x_or*two | (-i*x_or)
    */
    y = -i;
    x_or *= two;
    y *= x_or;
    if (y >= 0)
      y.neg_assign(y);
    if (x_or.lower() > x_or.upper() || y.lower() > y.upper())
      break;
    nout << "*** " << x_or << " | " << y << " ***" << endl;
    x_or |= y;
    nout << "*** approximate with ***" << endl << x_or << endl << endl;
  }

  nout << endl <<"*** AND ***" << endl;
  for (int i = 1; i <= 10; ++i) {
    for (int j = 10; j >= 1; --j) {
      /*
	If x_and >= 0:
          Compute: x_and = -x_and & [2^i, 2^j]
	Else
          Compute: x_and = x_and & [2^i, 2^j]
	Finally:
	  x_and = x_and + y;
      */
      y.assign(pow(2,i));
      y.join_assign(pow(2,j));
      y.neg_assign(y);
      if (x_and >= 0)
	x_and.neg_assign(x_and);
      if (x_and.lower() > x_and.upper() || y.lower() > y.upper())
	break;
      nout << "*** " << x_and << " & " << y << " ***" << endl;
      x_and &= y;
      nout << "*** approximate with ***" << endl << x_and << endl << endl;
      x_and += y;
    }
  }

  nout << endl <<"*** XOR ***" << endl;
  for (int i = 1; i <= 10; ++i){
    for (int j = 10; j >= 1; --j) {
      /*
	If x_and >= 0:
	  Compute: x_xor = -x_xor ^ -[i, 2^j]
	Finally:
	  If x_xor >= 0:
	     x_xor = -x_xor
	  x_xor = x_xor + y;
      */
      y = i;
      y.join_assign(pow(2,j));
      y.neg_assign(y);
      if (x_xor.lower() > x_xor.upper() || y.lower() > y.upper())
	break;
      nout << "*** " << x_xor << " ^ " << y << " ***" << endl;
      x_xor ^= y;
      nout << "*** approximate with ***" << endl << x_xor << endl << endl;
      if (x_xor >= 0)
	x_xor.neg_assign(x_xor);
      x_xor += y;
    }
  }

  return true;
}

/*
  Test about the operations of intervals with the same sign
  and positive sign, bitwise operator are: LEFT_SHIFT and RIGHT_SHIFT.
*/
template<typename I>
bool test05() {
  typename My_Interval<I>::interval_type x_lshift;

  typename My_Interval<I>::interval_type x_rshift;

  typename My_Interval<I>::interval_type x;

  for (int i = 10; i >= 0; --i) {
    for (int j = 10; j >= 0; --j) {
      /*
	Compute: x_lshift = [i, j] << [i, j]
      */
      x = i;
      x.join_assign(j);
      x_lshift = x;
      nout << "*** " << x_lshift << " << " << x << " ***" << endl;
      x_lshift << x;
      nout << "*** approximate with ***" << endl << x_lshift << endl << endl;
    }
  }

  for (int i = 10; i >= 0; --i) {
    for (int j = 10; j >= 0; --j) {
      /*
	Compute: x_rshift = [i, j] >> [i, j]
      */
      x = i;
      x.join_assign(j);
      x_rshift = x;
      nout << "*** " << x_rshift << " >> " << x << " ***" << endl;
      x_rshift >> x;
      nout << "*** approximate with ***" << endl << x_rshift << endl << endl;
    }
  }

  return true;
}

/*
  Test about the operations of intervals with the same sign
  and positive negative, bitwise operator are: LEFT_SHIFT and RIGHT_SHIFT.
*/
template<typename I>
bool test06() {
  typename My_Interval<I>::interval_type x_lshift;

  typename My_Interval<I>::interval_type x_rshift;

  typename My_Interval<I>::interval_type x;

  for (int i = 10; i >= 5; --i) {
    for (int j = 10; j >= 5; --j) {
      /*
	Compute: x_lshift = -[i, j] << -[i,j]
      */
      x = i;
      x.join_assign(j);
      x.neg_assign(x);
      x_lshift = x;
      nout << "*** " << x_lshift << " << " << x << " ***" << endl;
      x_lshift << x;
      nout << "*** approximate with ***" << endl << x_lshift << endl << endl;
    }
  }

  for (int i = 10; i >= 5; --i) {
    for (int j = 10; j >= 5; --j) {
      /*
	Compute: x_rshift = -[i, j] >> -[i,j]
      */
      x = i;
      x.join_assign(j);
      x.neg_assign(x);
      x_rshift = x;
      nout << "*** " << x_rshift << " >> " << x << " ***" << endl;
      x_rshift >> x;
      nout << "*** approximate with ***" << endl << x_rshift << endl << endl;
    }
  }

  return true;
}

/*
  Test about the operations of intervals with discordant sign,
  bitwise operator are: LEFT_SHIFT and RIGHT_SHIFT.
*/
template<typename I>
bool test07() {
  typename My_Interval<I>::interval_type x_lshift;

  typename My_Interval<I>::interval_type x_rshift;

  typename My_Interval<I>::interval_type x;

  for (int i = 10; i >= 5; --i) {
    for (int j = 10; j >= 5; --j) {
      /*
	If j is odd:
	  Compute: x_lshift = -[i,j] << [i,j]
	Else
	  Compute: x_lshift = [i,j] << -[i,j]
      */
      x = i;
      x.join_assign(j);
      x_lshift = x;
      if (j % 2)
	x_lshift.neg_assign(x_lshift);
      else
	x.neg_assign(x);
      nout << "*** " << x_lshift << " << " << x << " ***" << endl;
      x_lshift << x;
      nout << "*** approximate with ***" << endl << x_lshift << endl << endl;
    }
  }

  for (int i = 10; i >= 5; --i) {
    for (int j = 10; j >= 5; --j) {
      /*
	If j is odd:
	  Compute: x_rshift = -[i,j] >> [i,j]
	Else
	  Compute: x_rshift = [i,j] >> -[i,j]
      */
      x = i;
      x.join_assign(j);
      x_rshift = x;
      if (j % 2)
	x_rshift.neg_assign(x_rshift);
      else
	x.neg_assign(x);
      nout << "*** " << x_rshift << " >> " << x << " ***" << endl;
      x_rshift >> x;
      nout << "*** approximate with ***" << endl << x_rshift << endl << endl;
    }
  }

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01<unsigned int>);
  DO_TEST(test02<unsigned int>);
  DO_TEST(test03<unsigned int>);
  DO_TEST(test04<unsigned int>);
  DO_TEST(test05<unsigned int>);
  DO_TEST(test06<unsigned int>);
  DO_TEST(test07<unsigned int>);

  DO_TEST(test01<int>);
  DO_TEST(test02<int>);
  DO_TEST(test03<int>);
  DO_TEST(test04<int>);
  DO_TEST(test05<int>);
  DO_TEST(test06<int>);
  DO_TEST(test07<int>);

  DO_TEST(test01<long int>);
  DO_TEST(test02<long int>);
  DO_TEST(test03<long int>);
  DO_TEST(test04<long int>);
  DO_TEST(test05<long int>);
  DO_TEST(test06<long int>);
  DO_TEST(test07<long int>);
END_MAIN
