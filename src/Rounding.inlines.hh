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

static Rounding_State current_rounding;

inline
Rounding::Rounding()
  : dir(Rounding::CURRENT) {
}

inline
Rounding::Rounding(Direction d)
  : dir(d) {
}

inline void
Rounding::set_direction(Rounding::Direction d) {
  assert(d != Rounding::CURRENT);
  dir = d;
}

inline Rounding::Direction
Rounding::direction() const {
  if (dir != CURRENT) {
    assert(dir == current_rounding.dir);
    return dir;
  }
  return current_rounding.dir;
}

inline
Rounding_State::Rounding_State()
  : dir(Rounding::IGNORE) {
}

inline
Rounding_State::~Rounding_State()
{
  assert(this != &current_rounding || dir == Rounding::IGNORE);
}

template <typename To>
inline void
Rounding::save(Rounding_State& current) {
  assert(dir != Rounding::CURRENT);
  if (use_fpu_rounding(To) && dir != Rounding::IGNORE)
    current.fpu_dir = fpu_save_rounding_direction(dir);
  current.dir = current_rounding.dir;
  current_rounding.dir = dir;
}

template <typename To>
inline void
Rounding::restore(const Rounding_State& state) {
  assert(current_rounding.dir == dir);
  if (use_fpu_rounding(To))
    fpu_restore_rounding_direction(state.fpu_dir);
  current_rounding.dir = state.dir;
}

} // namespace Parma_Polyhedra_Library
