/* Header file for test programs.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include <stdexcept>

#ifdef DERIVED_TEST
#define C_Polyhedron NNC_Polyhedron
#endif

namespace Parma_Polyhedra_Library {

#if 0
typedef BD_Shape<E_Rational> TBD_Shape;
#else
typedef BD_Shape<Checked_Number<mpq_class, Extended_Number_Policy> > TBD_Shape;
//typedef BD_Shape<Checked_Number<int, Extended_Number_Policy> > TBD_Shape;

template <typename T, typename Policy>
bool is_plus_infinity(const Checked_Number<T, Policy>& x) {
  return x.classify(false, true, false) == VC_PLUS_INFINITY;
}

template <typename T, typename Policy>
bool is_nan(const Checked_Number<T, Policy>& x) {
  return x.classify(true, false, false) == VC_NAN;
}

template <typename T, typename Policy>
bool is_negative(const Checked_Number<T, Policy>& x) {
  return x.classify(false, false, true) == V_LT;
}

template <typename T, typename Policy>
bool is_nonnegative(const Checked_Number<T, Policy>& x) {
  return x.classify(false, false, true) == V_GE;
}

template <typename T, typename Policy>
bool exact_neg(Checked_Number<T, Policy>& to, const Checked_Number<T, Policy>& x) {
  return to.assign_neg(x, Rounding::IGNORE) == V_EQ;
}

template <typename To, typename To_Policy>
void div_round_up(Checked_Number<To, To_Policy>& to,
		  const Coefficient& x,
		  const Coefficient& y) {
  Rounding_State old;
  Checked_Number<To, To_Policy> nx;
  Rounding x_r(y < 0 ? Rounding::DOWN : Rounding::UP);
  x_r.internal_save<To>(old);
  nx.assign(x, x_r);
  Checked_Number<To, To_Policy> ny;
  Rounding y_r(x > 0 ? Rounding::DOWN : Rounding::UP);
  y_r.internal_install<To>();
  ny.assign(y, y_r);
  Rounding to_r(Rounding::UP);
  to_r.internal_install<To>();
  to.assign_div(nx, ny, to_r);
  to_r.internal_restore<To>(old);
}

template <typename T, typename Policy>
void add_round_up(Checked_Number<T, Policy>& to,
		  const Checked_Number<T, Policy>& x,
		  const Checked_Number<T, Policy>& y) {
  Rounding_State old;
  Rounding mode(Rounding::UP);
  mode.internal_save<T>(old);
  to.assign_add(x, y, mode);
  mode.internal_restore<T>(old);
}

template <typename T, typename Policy>
void add_round_down(Checked_Number<T, Policy>& to,
		    const Checked_Number<T, Policy>& x,
		    const Checked_Number<T, Policy>& y) {
  Rounding_State old;
  Rounding mode(Rounding::DOWN);
  mode.internal_save<T>(old);
  to.assign_add(x, y, mode);
  mode.internal_restore<T>(old);
}

template <typename T, typename Policy>
void negate_round_down(Checked_Number<T, Policy>& to,
		       const Checked_Number<T, Policy>& x) {
  Rounding_State old;
  Rounding mode(Rounding::DOWN);
  mode.internal_save<T>(old);
  to.assign_neg(x, mode);
  mode.internal_restore<T>(old);
}

template <typename T, typename Policy>
void numer_denom(const Checked_Number<T, Policy>& from,
		 Coefficient& num, Coefficient& den) {
  // FIXME!
  if (from.classify(true, true, false) != VC_NORMAL)
    abort();
  mpq_class q;
  Checked::assign<Checked::Transparent_Policy>(q, raw_value(from), Rounding::IGNORE);
  q.canonicalize();
  num = q.get_num();
  den = q.get_den();
}
#endif

}
