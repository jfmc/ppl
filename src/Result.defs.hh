/* Result enum and supporting function declarations.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Result_defs_hh
#define PPL_Result_defs_hh 1

namespace Parma_Polyhedra_Library {

enum Result {

  VC_MASK = 48,

  //! Ordinary result class.
  VC_NORMAL = 0,

  //! The computed result is inexact and rounded up.
  V_LT = 1,

  //! The computed result is inexact and rounded down.
  V_GT = 2,

  //! The computed result is exact.
  V_EQ = 4,

  //! The computed result is inexact.
  V_NE = V_LT | V_GT,

  //! The computed result may be inexact and rounded up.
  V_LE = V_EQ | V_LT,

  //! The computed result may be inexact and rounded down.
  V_GE = V_EQ | V_GT,

  //! The computed result may be inexact.
  V_LGE = V_LT | V_EQ | V_GT,

  //! Negative infinity result class
  VC_MINUS_INFINITY = 16,

  //! A negative overflow occurred.
  V_NEG_OVERFLOW = VC_MINUS_INFINITY | V_GT,

  //! Positive infinity result classe
  VC_PLUS_INFINITY = 32,

  //! A positive overflow occurred.
  V_POS_OVERFLOW = VC_PLUS_INFINITY | V_LT,

  //! Not a number result class.
  VC_NAN = 48,

  //! Conversion from unknown string.
  V_CVT_STR_UNK = 49,

  //! x / 0.
  V_DIV_ZERO = 50,

  //! +/-inf + -/+inf.
  V_INF_ADD_INF = 51,

  //! +/-inf / -/+inf.
  V_INF_DIV_INF = 52,

  //! +/-inf mod x.
  V_INF_MOD = 53,

  //! +/-inf * 0.
  V_INF_MUL_ZERO = 54,

  //! +/-inf - +/-inf.
  V_INF_SUB_INF = 55,

  //! x mod 0.
  V_MOD_ZERO = 56,

  //! sqrt(-x).
  V_SQRT_NEG = 57,

  //! Unknown result due to intermediate negative overflow.
  V_UNKNOWN_NEG_OVERFLOW = 58,

  //! Unknown result due to intermediate positive overflow.
  V_UNKNOWN_POS_OVERFLOW = 59,

  //! Unordered comparison
  V_UNORD_COMP = 60
};

bool is_special(Result r);
Result classify(Result r);
Result sign(Result r);

} // namespace Parma_Polyhedra_Library

#include "Result.inlines.hh"

#endif // !defined(PPL_Result_defs_hh)
