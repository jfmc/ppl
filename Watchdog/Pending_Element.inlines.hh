/* Pending_Element class implementation: inline functions.
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

#ifndef PWL_Pending_Element_inlines_hh
#define PWL_Pending_Element_inlines_hh 1

namespace Parma_Watchdog_Library {

inline
Pending_Element::Pending_Element(const Time& deadline,
				 const Handler& handler,
				 bool& expired_flag)
  : d(deadline), p_h(&handler), p_f(&expired_flag) {
  assert(OK());
}

inline void
Pending_Element::assign(const Time& deadline,
			const Handler& handler,
			bool& expired_flag) {
  d = deadline;
  p_h = &handler;
  p_f = &expired_flag;
  assert(OK());
}

inline const Time&
Pending_Element::deadline() const {
  return d;
}

inline const Handler&
Pending_Element::handler() const {
  return *p_h;
}

inline bool&
Pending_Element::expired_flag() const {
  return *p_f;
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Pending_Element_inlines_hh)
