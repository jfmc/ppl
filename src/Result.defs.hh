/* Result info
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

  V_NORMAL = 0,

  // Special results.

  //! \brief
  //! The exact result does not exist (i.e., the operation does not make
  //! sense for the given operands) or is unknown (i.e., the operands do
  //! not encode enough information to give a mean.
  V_UNKNOWN = 16,

  V_MINUS_INFINITY = 32,

  V_PLUS_INFINITY = 48,

  V_TYPE_MASK = 48,

  //! \brief
  //! The exact result is outside the considered numeric domain
  //! (e.g., sqrt(-1)).
  V_DOMAIN = V_UNKNOWN | V_NE,

  //! A negative overflow occurred.
  V_NEG_OVERFLOW = V_MINUS_INFINITY | V_GT,

  //! A positive overflow occurred.
  V_POS_OVERFLOW = V_PLUS_INFINITY | V_LT,

};

bool is_special(Result r);
Result type(Result r);
Result sign(Result r);

} // namespace Parma_Polyhedra_Library

#include "Result.inlines.hh"

#endif // !defined(PPL_Result_defs_hh)

