/* Pending_List class implementation: inline functions.
   Copyright (C) 2002-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Watchdog Library (PWL).

The PWL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PWL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see
http://www.cs.unipr.it/Software/ . */

#ifndef PWL_Pending_List_inlines_hh
#define PWL_Pending_List_inlines_hh 1

namespace Parma_Watchdog_Library {

inline
Pending_List::Pending_List()
  : active_list(),
    free_list() {
  assert(OK());
}

inline
Pending_List::~Pending_List() {
}

inline Pending_List::Iterator
Pending_List::begin() {
  return active_list.begin();
}

inline Pending_List::Iterator
Pending_List::end() {
  return active_list.end();
}

inline bool
Pending_List::empty() const {
  return active_list.empty();
}

inline Pending_List::Iterator
Pending_List::erase(Iterator position) {
  assert(!empty());
  Iterator next = active_list.erase(position);
  free_list.push_back(*position);
  assert(OK());
  return next;
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Pending_List_inlines_hh)
