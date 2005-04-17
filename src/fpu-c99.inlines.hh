/* C99 Floating point unit related functions.
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

#include <fenv.h>

#ifdef FE_TONEAREST
#define FPU_TONEAREST FE_TONEAREST
#endif
#ifdef FE_UPWARD
#define FPU_UPWARD FE_UPWARD
#endif
#ifdef FE_DOWNWARD
#define FPU_DOWNWARD FE_DOWNWARD
#endif
#ifdef FE_TOWARDZERO
#define FPU_TOWARDZERO FE_TOWARDZERO
#endif

namespace Parma_Polyhedra_Library {

inline int
fpu_get_rounding_direction() {
  return fegetround();
}

inline void
fpu_set_rounding_direction(int dir) {
  fesetround(dir);
}

inline int
fpu_save_rounding_direction(int dir) {
  int old = fegetround();
  fesetround(dir);
  return old;
}

inline void
fpu_reset_inexact() {
  feclearexcept(FE_INEXACT);
}

inline int
fpu_save_rounding_direction_reset_inexact(int dir) {
  fpu_reset_inexact();
  return fpu_save_rounding_direction(dir);
}

inline void
fpu_restore_rounding_direction(int dir) {
  fesetround(dir);
}

inline int
fpu_check_inexact() {
  return fetestexcept(FE_INEXACT) != 0;
}

} // namespace Parma_Polyhedra_Library
