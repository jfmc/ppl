/* Rounding mode.
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

#include "Float.defs.hh"

#define use_fpu_rounding(Type) (Float<Type>::fpu_related)

namespace Parma_Polyhedra_Library {

static Rounding_Dir current_rounding_dir;

inline Rounding_Dir
rounding_direction(Rounding_Dir dir) {
  return dir == ROUND_CURRENT ? current_rounding_dir : dir;
}

template <typename To>
inline void
rounding_install_internal(Rounding_Dir dir) {
  if (use_fpu_rounding(To) && dir != ROUND_IGNORE)
    fpu_save_rounding_direction(dir);
}

template <typename To>
inline void
rounding_save_internal(Rounding_Dir dir, Rounding_State& old) {
  if (use_fpu_rounding(To) && dir != ROUND_IGNORE)
    old.fpu_dir = fpu_save_rounding_direction(dir);
}

template <typename To>
inline void
rounding_restore_internal(const Rounding_State& old, Rounding_Dir dir) {
  if (use_fpu_rounding(To) && dir != ROUND_IGNORE)
    fpu_restore_rounding_direction(old.fpu_dir);
}

template <typename To>
inline void
rounding_install(Rounding_Dir dir) {
  assert(dir != ROUND_CURRENT);
  rounding_install_internal<To>(dir);
  current_rounding_dir = dir;
}

template <typename To>
inline void
rounding_save(Rounding_Dir dir, Rounding_State& old) {
  assert(dir != ROUND_CURRENT);
  old.dir = current_rounding_dir;
  rounding_save_internal<To>(dir, old);
  current_rounding_dir = dir;
}

template <typename To>
inline void
rounding_restore(const Rounding_State& old, Rounding_Dir dir) {
  assert(current_rounding_dir == dir);
  rounding_restore_internal<To>(old, dir);
  current_rounding_dir = old.dir;
}

} // namespace Parma_Polyhedra_Library
