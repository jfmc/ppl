/* Watchdog and associated classes' implementation (non-inline functions).
   Copyright (C) 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

For the most up-to-date information see the CS@Parma software
site: http://www.cs.unipr.it/Software/ . */

#include <config.h>

#include "Watchdog.defs.hh"

#include <csignal>
#include <iostream>

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <ctime>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <ctime>
# endif
#endif


namespace PWL = Parma_Watchdog_Library;

using std::cerr;
using std::endl;

// Pass this to getitimer.
itimerval PWL::Watchdog::current_timer_status;

// Pass this to setitimer.
itimerval PWL::Watchdog::signal_once;

// Last time value we set the timer to.
PWL::Watchdog::Time PWL::Watchdog::last_time_requested;

// Records the time elapsed since last fresh start.
PWL::Watchdog::Time PWL::Watchdog::time_so_far;

// The ordered queue of pending watchdog events.
PWL::Watchdog::Pending PWL::Watchdog::pending;

// Whether the alarm clock is running.
volatile bool PWL::Watchdog::alarm_clock_running = false;

// Whether we are changing data which are also changed by the signal handler.
volatile bool PWL::Watchdog::in_critical_section = false;

void
PWL::Watchdog::get_timer(Time& time) {
  getitimer(ITIMER_PROF, &current_timer_status);
  time.set(current_timer_status.it_value.tv_sec,
	   current_timer_status.it_value.tv_usec);
}

void
PWL::Watchdog::set_timer(const Time& time) {
  if (time.seconds() == 0 && time.microseconds() == 0)
    abort();
  last_time_requested = time;
  signal_once.it_value.tv_sec = time.seconds();
  signal_once.it_value.tv_usec = time.microseconds();
  setitimer(ITIMER_PROF, &signal_once, 0);
}

void
PWL::Watchdog::stop_timer() {
  signal_once.it_value.tv_sec = 0;
  signal_once.it_value.tv_usec = 0;
  setitimer(ITIMER_PROF, &signal_once, 0);
}

void
PWL::Watchdog::handle_timeout(int) {
  if (in_critical_section)
    reschedule();
  else {
    time_so_far += last_time_requested;
    if (!pending.empty()) {
      Pending::iterator i(pending.begin());
      Pending::iterator in;
      do {
	(*i).handler->act();
	*((*i).p_expired_flag) = true;
	in = i;
	++in;
	pending.erase(i);
	i = in;
      } while (i != pending.end() && (*i).deadline <= time_so_far);
      if (pending.empty())
	alarm_clock_running = false;
      else
	set_timer((*pending.begin()).deadline - time_so_far);
    }
    else
      alarm_clock_running = false;
  }
}

PWL::Watchdog::Pending::iterator
PWL::Watchdog::insert_pending(const Time& deadline,
			      const Handler* handler,
			      bool* p_expired) {
  Pending::iterator i(pending.begin());
  Pending::iterator pend(pending.end());
  while (i != pend && (*i).deadline < deadline)
    ++i;
  return pending.insert(i, Pending::value_type(deadline, handler, p_expired));
}

PWL::Watchdog::Pending::iterator
PWL::Watchdog::new_watchdog_event(int units,
				  const Handler* handler,
				  bool* p_expired) {
  assert(units > 0);
  Pending::iterator position;
  Time deadline(units);
  if (!alarm_clock_running) {
    position = insert_pending(deadline, handler, p_expired);
    time_so_far.reset();
    set_timer(deadline);
    alarm_clock_running = true;
  }
  else {
    Time time_to_shoot;
    get_timer(time_to_shoot);
    Time elapsed_time(last_time_requested);
    elapsed_time -= time_to_shoot;
    Time current_time(time_so_far);
    current_time += elapsed_time;
    Time real_deadline(deadline);
    real_deadline += current_time;
    position = insert_pending(real_deadline, handler, p_expired);
    if (deadline < time_to_shoot) {
      time_so_far = current_time;
      set_timer(deadline);
    }
  }
  return position;
}

void
PWL::Watchdog::remove_watchdog_event(Pending::iterator position) {
  assert(!pending.empty());
  if (position == pending.begin()) {
    Pending::iterator next(position);
    ++next;
    if (next != pending.end()) {
      Time first_deadline((*position).deadline);
      Time next_deadline((*next).deadline);
      if (first_deadline != next_deadline) {
	Time time_to_shoot;
	get_timer(time_to_shoot);
	Time elapsed_time(last_time_requested);
	elapsed_time -= time_to_shoot;
	time_so_far += elapsed_time;
	next_deadline -= first_deadline;
	time_to_shoot += next_deadline;
	set_timer(time_to_shoot);
      }
    }
    else {
      stop_timer();
      alarm_clock_running = false;
    }
  }
  pending.erase(position);
}

PWL::Watchdog::~Watchdog() {
  if (!expired) {
    in_critical_section = true;
    remove_watchdog_event(pending_position);
    in_critical_section = false;
  }
  //  delete handler;
}

void
PWL::Watchdog::initialize() {
  signal_once.it_interval.tv_sec = 0;
  signal_once.it_interval.tv_usec = 0;

  sigset_t mask;
  sigemptyset(&mask);

  struct sigaction s;
  s.sa_handler = handle_timeout;
  s.sa_mask = mask;
  s.sa_flags = 0;  // Was SA_ONESHOT: why?

  int r = sigaction(SIGPROF, &s, 0);
  if (r) {
    cerr << "sigaction failed: " << r << endl;
    exit(1);
  }
}

PWL::Watchdog::Time PWL::Watchdog::reschedule_time(1);
