/* Weightwatch and associated classes'.
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

#ifndef PWL_Weightwatch_templates_hh
#define PWL_Weightwatch_templates_hh 1

namespace Parma_Watchdog_Library {

template <typename Threshold, typename Get, typename Compare>
typename Weightwatch<Threshold, Get, Compare>::WW_Pending_List::Iterator
Weightwatch<Threshold, Get, Compare>::add_threshold(Threshold threshold,
						    const Handler& handler,
						    bool& expired_flag) {
  *initialize.check_hook_ptr = Weightwatch::check;
  return initialize.pending.insert(threshold, handler, expired_flag);
}

template <typename Threshold, typename Get, typename Compare>
typename Weightwatch<Threshold, Get, Compare>::WW_Pending_List::Iterator
Weightwatch<Threshold, Get, Compare>::remove_threshold(typename WW_Pending_List::Iterator position) {
  typename WW_Pending_List::Iterator i;
  i = initialize.pending.erase(position);
  if (initialize.pending.empty())
    *initialize.check_hook_ptr = 0;
  return i;
}

template <typename Threshold, typename Get, typename Compare>
Weightwatch<Threshold, Get, Compare>::~Weightwatch() {
  if (!expired)
    remove_threshold(pending_position);
  delete &handler;
}

template <typename Threshold, typename Get, typename Compare>
void
Weightwatch<Threshold, Get, Compare>::check() {
  typename WW_Pending_List::Iterator i = initialize.pending.begin();
  assert(i != initialize.pending.end());
  const Threshold& current = Get()();
  while (!Compare()(current, i->deadline())) {
    i->handler().act();
    i->expired_flag() = true;
    i = remove_threshold(i);
    if (i == initialize.pending.end())
      break;
  }
}

template <typename Threshold, typename Get, typename Compare>
Weightwatch<Threshold, Get, Compare>::Initialize::Initialize(void (*&check_hook)(void)) {
  check_hook_ptr = &check_hook;
  check_hook = 0;
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Weightwatch_templates_hh)
