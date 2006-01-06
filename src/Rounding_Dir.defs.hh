/* Declaration of Rounding_Dir and related functions.
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

#ifndef PPL_Rounding_defs_hh
#define PPL_Rounding_defs_hh 1

#include "Result.defs.hh"
#include "fpu.defs.hh"

namespace Parma_Polyhedra_Library {

enum Rounding_Dir {
  //! Round toward \f$-\infty\f$.
  ROUND_DOWN = FPU_DOWNWARD,

  //! Round toward \f$+\infty\f$.
  ROUND_UP = FPU_UPWARD,

  //! Rounding is delegated to lower level. Result info is evaluated lazily.
  ROUND_IGNORE = -1,
  ROUND_NATIVE = ROUND_IGNORE,

  //! Rounding is not needed: client code must ensure the operation is exact.
  ROUND_NOT_NEEDED = -2,

  ROUND_DIRECT = ROUND_UP,
  ROUND_INVERSE = ROUND_DOWN
};

/*! \brief
  Returns the inverse rounding mode of \p dir,
  <CODE>ROUND_IGNORE</CODE> being the inverse of itself.
*/
Rounding_Dir inverse(Rounding_Dir dir);

} // namespace Parma_Polyhedra_Library

#include "Rounding_Dir.inlines.hh"

#endif // !defined(PPL_Float_defs_hh)

