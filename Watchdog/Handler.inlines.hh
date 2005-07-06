/* Handler and derived classes' implementation: inline functions.
   Copyright (C) 2002-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PWL_Handler_inlines_hh
#define PWL_Handler_inlines_hh 1

namespace Parma_Watchdog_Library {

inline
Handler::~Handler() {
}

template <typename Flag_Base, typename Flag>
Handler_Flag<Flag_Base, Flag>::Handler_Flag(const Flag_Base* volatile& holder,
					    Flag& flag)
  : h(holder), f(flag) {
}

template <typename Flag_Base, typename Flag>
void
Handler_Flag<Flag_Base, Flag>::act() const {
  if (h == 0 || static_cast<const Flag&>(*h).priority() < f.priority())
    h = &f;
}

inline
Handler_Function::Handler_Function(void (*function)())
  : f(function) {
}

inline void
Handler_Function::act() const {
  (*f)();
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Handler_inlines_hh)
