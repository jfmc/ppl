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
template <typename T, typename Policy>
inline bool
is_plus_infinity(const Checked_Number<T, Policy>& x) {
  return x.is_pinf();
}

template <typename T, typename Policy>
inline bool
is_nan(const Checked_Number<T, Policy>& x) {
  return x.is_nan();
}

template <typename T, typename Policy>
inline bool
is_negative(const Checked_Number<T, Policy>& x) {
  return x < 0;
}

template <typename T, typename Policy>
inline bool
is_nonnegative(const Checked_Number<T, Policy>& x) {
  return x >= 0;
}

template <typename T, typename Policy>
inline bool
exact_neg(Checked_Number<T, Policy>& to, const Checked_Number<T, Policy>& x) {
  return to.assign_neg(x, ROUND_IGNORE) == V_EQ;
}

template <typename To, typename To_Policy>
inline void
div_round_up(Checked_Number<To, To_Policy>& to,
		  const Coefficient& x,
		  const Coefficient& y) {
#if 0
  Rounding_State old;
  Checked_Number<To, To_Policy> nx;
  Rounding_Dir x_r(y < 0 ? ROUND_DOWN : ROUND_UP);
  rounding_save_internal<To>(x_r, old);
  nx.assign(x, x_r);
  Checked_Number<To, To_Policy> ny;
  Rounding_Dir y_r(x > 0 ? ROUND_DOWN : ROUND_UP);
  rounding_install_internal<To>(y_r);
  ny.assign(y, y_r);
  rounding_install_internal<To>(ROUND_UP);
  to.assign_div(nx, ny, ROUND_UP);
  rounding_restore_internal<To>(old, ROUND_UP);
#else
  Rounding_State old;
  Coefficient q;
  rounding_save_internal<Coefficient>(ROUND_UP, old);
  Result r = q.assign_div(x, y, ROUND_UP);
  rounding_restore_internal<Coefficient>(old, ROUND_UP);
  if (r == V_POS_OVERFLOW) {
    to = PLUS_INFINITY;
    return;
  }
  rounding_save_internal<To>(ROUND_UP, old);
  to.assign(q, ROUND_UP);
  rounding_restore_internal<To>(old, ROUND_UP);
#endif
}

template <typename T, typename Policy>
inline void
add_round_up(Checked_Number<T, Policy>& to,
	     const Checked_Number<T, Policy>& x,
	     const Checked_Number<T, Policy>& y) {
  Rounding_State old;
  rounding_save_internal<T>(ROUND_UP, old);
  to.assign_add(x, y, ROUND_UP);
  rounding_restore_internal<T>(old, ROUND_UP);
}

template <typename T, typename Policy>
inline void
add_round_down(Checked_Number<T, Policy>& to,
	       const Checked_Number<T, Policy>& x,
	       const Checked_Number<T, Policy>& y) {
  Rounding_State old;
  rounding_save_internal<T>(ROUND_DOWN, old);
  to.assign_add(x, y, ROUND_DOWN);
  rounding_restore_internal<T>(old, ROUND_UP);
}

template <typename T, typename Policy>
inline void
negate_round_down(Checked_Number<T, Policy>& to,
		  const Checked_Number<T, Policy>& x) {
  Rounding_State old;
  rounding_save_internal<T>(ROUND_DOWN, old);
  to.assign_neg(x, ROUND_DOWN);
  rounding_restore_internal<T>(old, ROUND_UP);
}

template <typename T, typename Policy>
inline void
numer_denom(const Checked_Number<T, Policy>& from,
	    Coefficient& num, Coefficient& den) {
  // FIXME!
  if (from.is_nan() || from.is_minf() || from.is_pinf())
    abort();
  mpq_class q;
  Checked::assign<Checked::Transparent_Policy>(q, raw_value(from), ROUND_IGNORE);
  q.canonicalize();
  num = q.get_num();
  den = q.get_den();
}

typedef BD_Shape<Checked_Number<mpq_class, Extended_Number_Policy> > TBD_Shape;
//typedef BD_Shape<Checked_Number<int, Extended_Number_Policy> > TBD_Shape;
#endif

}
