/* Weightwatch and associated classes' declaration and inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Watchdog Library (PWL).

The PWL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PWL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PWL_Weightwatch_defs_hh
#define PWL_Weightwatch_defs_hh 1

#include "Weightwatch.types.hh"
#include "Handler.types.hh"
#include "Pending_List.defs.hh"
#include <cassert>

namespace Parma_Watchdog_Library {

typedef unsigned long long Weight;

//! A watchdog for computational weight.
class Weightwatch {
public:
  template <typename Flag_Base, typename Flag>
  Weightwatch(int units, const Flag_Base* volatile& holder, Flag& flag);

  Weightwatch(int units, void (*function)());
  ~Weightwatch();
  static void add(unsigned int units, unsigned int iterations);
  static void check();

private:
  typedef Pending_List<Weight> WW_Pending_List;

  bool expired;
  const Handler& handler;
  WW_Pending_List::Iterator pending_position;

private:
  // Just to prevent their use.
  Weightwatch(const Weightwatch&);
  Weightwatch& operator=(const Weightwatch&);

  //! The ordered queue of pending weight thresholds.
  static WW_Pending_List pending;

  //! Current weight.
  static Weight weight_so_far;

  // Handle the addition of a new weight threshold.
  static WW_Pending_List::Iterator new_weight_threshold(int units,
						      const Handler& handler,
						      bool& expired_flag);

  // Handle the removal of a weight threshold.
  void remove_weight_threshold(WW_Pending_List::Iterator position);

};

} // namespace Parma_Watchdog_Library

#include "Weightwatch.inlines.hh"

#endif // !defined(PWL_Watchdog_defs_hh)

