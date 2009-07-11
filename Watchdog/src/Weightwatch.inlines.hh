/* Weightwatch and associated classes' implementation: inline functions.
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

#ifndef PWL_Weightwatch_inlines_hh
#define PWL_Weightwatch_inlines_hh 1

#include <stdexcept>

#include "Handler.defs.hh"

namespace Parma_Watchdog_Library {

template <typename Threshold, typename Get, typename Compare>
template <typename Flag_Base, typename Flag>
Weightwatch<Threshold, Get, Compare>::Weightwatch(const Threshold& threshold, const Flag_Base* volatile& holder, Flag& flag)
  : expired(false),
    handler(*new Handler_Flag<Flag_Base, Flag>(holder, flag)) {
  if (!Compare()(threshold, Get()()))
    throw std::invalid_argument("Weightwatch constructor called with a"
				" threshold already reached");
  pending_position = add_threshold(threshold, handler, expired);
}

template <typename Threshold, typename Get, typename Compare>
inline
Weightwatch<Threshold, Get, Compare>::Weightwatch(const Threshold& threshold, void (*function)())
  : expired(false), handler(*new Handler_Function(function)) {
  if (!Compare()(threshold, Get()()))
    throw std::invalid_argument("Weightwatch constructor called with a"
				" threshold already reached");
  pending_position = add_threshold(threshold, handler, expired);
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Weightwatch_inlines_hh)
