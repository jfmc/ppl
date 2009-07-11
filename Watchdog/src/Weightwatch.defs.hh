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

//! A watchdog for thresholds exceeding.
template <typename Threshold, typename Get, typename Compare>
class Weightwatch {
public:
  template <typename Flag_Base, typename Flag>
  Weightwatch(const Threshold& threshold, const Flag_Base* volatile& holder, Flag& flag);

  Weightwatch(const Threshold& threshold, void (*function)());
  ~Weightwatch();

private:
  typedef Pending_List<Threshold, Compare> WW_Pending_List;

  bool expired;
  const Handler& handler;
  typename WW_Pending_List::Iterator pending_position;

  // Just to prevent their use.
  Weightwatch(const Weightwatch&);
  Weightwatch& operator=(const Weightwatch&);

  struct Initialize {
    Initialize(void (*&check_hook)(void));
    //! The ordered queue of pending thresholds.
    WW_Pending_List pending;

    //! Pointer to check hook.
    void (**check_hook_ptr)(void);
  };
  static Initialize initialize;

  // Handle the addition of a new threshold.
  static typename WW_Pending_List::Iterator
  add_threshold(Threshold threshold,
		const Handler& handler,
		bool& expired_flag);

  // Handle the removal of a threshold.
  static typename WW_Pending_List::Iterator
  remove_threshold(typename WW_Pending_List::Iterator position);

  //! Check threshold exceeding.
  static void check();

};

} // namespace Parma_Watchdog_Library

#include "Weightwatch.inlines.hh"
#include "Weightwatch.templates.hh"

#endif // !defined(PWL_Watchdog_defs_hh)

