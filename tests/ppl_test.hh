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
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

/* Turn token S into a string: stringify(x + y) => "x + y"  */
#define stringify(s) #s

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
  Result r = Checked::div<Check_Overflow_Policy>(raw_value(q), raw_value(x), raw_value(y), ROUND_UP);
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

//typedef BD_Shape<Checked_Number<mpq_class, Extended_Number_Policy> > TBD_Shape;
typedef BD_Shape<Checked_Number<int, Extended_Number_Policy> > TBD_Shape;
#endif

//! Look for variation in \p a.
/*!
  Return <CODE>true</CODE> if \p a contains variations from
  consistency, else return <CODE>false</CODE>.  Variation can be found
  via the OK method, or via a comparison between \p a and an object
  created from the ASCII dump of \p a.

  \p T must provide:
    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);
  and there must be a:
    bool operator==(const T& x, const T& y);
*/
template <typename T>
static bool
find_variation_template(T& a) {
  using namespace Parma_Polyhedra_Library::IO_Operators;

  if (a.OK() == false) {
    nout << "OK() failed\nASCII dump:" << endl;
    a.ascii_dump(nout);
    return true;
  }

  /* FIX In some PPL classes (e.g. Congruence) the simple constructors
     are private. */
  //T b;
  T b(a);
  stringstream dump;
  a.ascii_dump(dump);
  b.ascii_load(dump);
  if (b == a)
    return false;

  nout << "b loaded from a's ASCII dump should equal a\n"
       << "a's ASCII dump:\n" << dump.str()
       << "b's ASCII dump:\n";
  b.ascii_dump(nout);

  return true;
}

}
