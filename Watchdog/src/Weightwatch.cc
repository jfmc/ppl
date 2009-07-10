/* Weightwatch and associated classes' implementation (non-inline functions).
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

#include <pwl-config.h>

#include "Weightwatch.defs.hh"

#include <iostream>
#include <stdexcept>
#include <string>
#include <string.h>

namespace PWL = Parma_Watchdog_Library;

// The ordered queue of pending weight thresholds.
PWL::Weightwatch::WW_Pending_List PWL::Weightwatch::pending;

PWL::Weight PWL::Weightwatch::weight_so_far = 0;

PWL::Weightwatch::WW_Pending_List::Iterator
PWL::Weightwatch::new_weight_threshold(int units,
				       const Handler& handler,
				       bool& expired_flag) {
  assert(units > 0);
  if (weight_so_far == 0)
    weight_so_far = 1;
  return pending.insert(weight_so_far + units, handler, expired_flag);
}

void
PWL::Weightwatch::remove_weight_threshold(WW_Pending_List::Iterator position) {
  pending.erase(position);
  if (pending.empty())
    weight_so_far = 0;
}

PWL::Weightwatch::~Weightwatch() {
  if (!expired) {
    remove_weight_threshold(pending_position);
  }
  delete &handler;
}

