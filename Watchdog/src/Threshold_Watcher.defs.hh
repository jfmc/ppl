/* Threshold_Watcher and associated classes' declaration and inline functions.
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

#ifndef PWL_Threshold_Watcher_defs_hh
#define PWL_Threshold_Watcher_defs_hh 1

#include "Threshold_Watcher.types.hh"
#include "Handler.types.hh"
#include "Pending_List.defs.hh"
#include <cassert>

namespace Parma_Watchdog_Library {

//! A watchdog for thresholds exceeding.
template <typename Traits>
class Threshold_Watcher {
public:
  template <typename Flag_Base, typename Flag>
  Threshold_Watcher(const typename Traits::Delta& delta, const Flag_Base* volatile& holder, Flag& flag);

  Threshold_Watcher(const typename Traits::Delta& delta, void (*function)());
  ~Threshold_Watcher();

private:
  typedef Pending_List<Traits> WW_Pending_List;

  bool expired;
  const Handler& handler;
  typename WW_Pending_List::Iterator pending_position;

  // Just to prevent their use.
  Threshold_Watcher(const Threshold_Watcher&);
  Threshold_Watcher& operator=(const Threshold_Watcher&);

  struct Initialize {
    Initialize(int) {
    }
    //! The ordered queue of pending thresholds.
    WW_Pending_List pending;
  };
  static Initialize initialize;

  // Handle the addition of a new threshold.
  static typename WW_Pending_List::Iterator
  add_threshold(typename Traits::Threshold threshold,
		const Handler& handler,
		bool& expired_flag);

  // Handle the removal of a threshold.
  static typename WW_Pending_List::Iterator
  remove_threshold(typename WW_Pending_List::Iterator position);

  //! Check threshold reaching.
  static void check();

};

} // namespace Parma_Watchdog_Library

#include "Threshold_Watcher.inlines.hh"
#include "Threshold_Watcher.templates.hh"

#endif // !defined(PWL_Threshold_Watcher_defs_hh)

