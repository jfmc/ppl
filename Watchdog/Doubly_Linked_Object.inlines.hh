/* Doubly_Linked_Object class implementation: inline functions.
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

#ifndef PWL_Doubly_Linked_Object_inlines_hh
#define PWL_Doubly_Linked_Object_inlines_hh 1

namespace Parma_Watchdog_Library {

inline
Doubly_Linked_Object::Doubly_Linked_Object() {
}

inline
Doubly_Linked_Object::Doubly_Linked_Object(Doubly_Linked_Object* f,
					   Doubly_Linked_Object* b)
  : next(f),
    prev(b) {
}

inline void
Doubly_Linked_Object::insert_before(Doubly_Linked_Object& y) {
  y.next = this;
  y.prev = prev;
  prev->next = &y;
  prev = &y;
}

inline void
Doubly_Linked_Object::insert_after(Doubly_Linked_Object& y) {
  y.next = next;
  y.prev = this;
  next->prev = &y;
  next = &y;
}

inline Doubly_Linked_Object*
Doubly_Linked_Object::erase() {
  next->prev = prev;
  prev->next = next;
  return next;
}

inline
Doubly_Linked_Object::~Doubly_Linked_Object() {
  erase();
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Doubly_Linked_Object_inlines_hh)
