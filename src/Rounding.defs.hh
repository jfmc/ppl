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

class Rounding {
public:
  enum Direction {
    DOWN = FPU_DOWNWARD,
    UP = FPU_UPWARD,
    IGNORE = -1,
    CURRENT = -2
  };
  Rounding();
  Rounding(Direction d);
  void set_direction(Direction d);
  Direction direction() const;
  template <typename To>
  void internal_install() const;
  template <typename To>
  void internal_save(Rounding_State& current) const;
  template <typename To>
  void internal_restore(const Rounding_State& state) const;
  template <typename To>
  void install() const;
  template <typename To>
  void save(Rounding_State& current) const;
  template <typename To>
  void restore(const Rounding_State& state) const;
private:
  Direction dir;
};

class Rounding_State {
public:
  Rounding_State();
  ~Rounding_State();
private:
  int fpu_dir;
  Rounding::Direction dir;
  friend class Rounding;
};

} // namespace Parma_Polyhedra_Library

#include "Rounding.inlines.hh"

#endif // !defined(PPL_Float_defs_hh)

