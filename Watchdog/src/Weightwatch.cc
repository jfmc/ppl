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

namespace Parma_Watchdog_Library {

// The ordered queue of pending weight thresholds.
Weightwatch::WW_Pending_List Weightwatch::pending;

Weight* Weightwatch::current_weight_ptr = 0;
void (**Weightwatch::check_hook_ptr)(void) = 0;

#ifndef NDEBUG
Weight Weightwatch::previous_weight = 0;
#endif

Weightwatch::WW_Pending_List::Iterator
Weightwatch::new_weight_threshold(int units,
				       const Handler& handler,
				       bool& expired_flag) {
  assert(units > 0);
  assert(current_weight_ptr);
  *check_hook_ptr = Weightwatch::check;
  return pending.insert(*current_weight_ptr + units, handler, expired_flag);
}

Weightwatch::WW_Pending_List::Iterator
Weightwatch::remove_weight_threshold(WW_Pending_List::Iterator position) {
  Weightwatch::WW_Pending_List::Iterator i =  pending.erase(position);
  if (pending.empty())
    *check_hook_ptr = 0;
  return i;
}

Weightwatch::~Weightwatch() {
  if (!expired)
    remove_weight_threshold(pending_position);
  delete &handler;
}

void
Weightwatch::check() {
  assert(current_weight_ptr);
  WW_Pending_List::Iterator i = pending.begin();
  assert(i != pending.end());
  assert(*current_weight_ptr - previous_weight < 1U << (sizeof(Weight)*8-1));
#ifndef NDEBUG
  previous_weight = *current_weight_ptr;
#endif
  while (*current_weight_ptr - i->deadline() < 1U << (sizeof(Weight)*8-1)) {
    i->handler().act();
    i->expired_flag() = true;
    i = remove_weight_threshold(i);
    if (i == pending.end())
      break;
  }
}

void
Weightwatch::initialize(Weight& current_weight, void (*&check_hook)(void)) {
  current_weight_ptr = &current_weight;
  check_hook_ptr = &check_hook;
  check_hook = 0;
#ifndef NDEBUG
  previous_weight = current_weight;
#endif
}

}
