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

#ifndef PPL_Rounding_defs_hh
#define PPL_Rounding_defs_hh 1

#include "Result.defs.hh"
#include "fpu.defs.hh"
#include "Rounding.types.hh"

namespace Parma_Polyhedra_Library {

enum Rounding_Dir {
  ROUND_DOWN = FPU_DOWNWARD,
  ROUND_UP = FPU_UPWARD,
  ROUND_IGNORE = -1,
  ROUND_CURRENT = -2
};

class Rounding_State {
public:
  Rounding_Dir dir;
  int fpu_dir;
};

Rounding_Dir rounding_direction(Rounding_Dir dir);

template <typename To>
void rounding_install_internal(Rounding_Dir dir);
template <typename To>
void rounding_save_internal(Rounding_Dir dir, Rounding_State& old);
template <typename To>
void rounding_restore_internal(const Rounding_State& old, Rounding_Dir dir);

template <typename To>
void rounding_install(Rounding_Dir dir);
template <typename To>
void rounding_save(Rounding_Dir dir, Rounding_State& old);
template <typename To>
void rounding_restore(const Rounding_State& old, Rounding_Dir dir);

} // namespace Parma_Polyhedra_Library

#include "Rounding.inlines.hh"

#endif // !defined(PPL_Float_defs_hh)

