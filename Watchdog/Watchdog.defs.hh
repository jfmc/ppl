/* Watchdog and associated classes' declaration and inline functions.
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

#ifndef _Watchdog_defs_hh
#define _Watchdog_defs_hh 1

#include "Watchdog.types.hh"
#include <list>
#include <cassert>

// FIXME
#include <sys/time.h>
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

namespace Parma_Watchdog_Library {

//! A watchdog timer.
class Watchdog {

private:

  //! A class for representing and manipulationg positive time intervals.
  class Time {
  private:
    int secs;
    int microsecs;

  public:
    //! Zero seconds.
    Time();

    explicit Time(unsigned int units);

    void set(int s, int m);

    void reset();

    int seconds() const;

    int microseconds() const;

    Time& operator +=(const Time& y);

    Time& operator -=(const Time& y);
  };

  friend Time Parma_Watchdog_Library::operator+(const Time& x, const Time& y);

  friend Time Parma_Watchdog_Library::operator-(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator==(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator!=(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator<(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator<=(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator>(const Time& x, const Time& y);

  friend bool Parma_Watchdog_Library::operator>=(const Time& x, const Time& y);

  // Different kinds of handler for the watchdog events.
  class Handler {
  public:
    virtual void act() const = 0;
  };

  template <typename Flag_Base, typename Flag>
  class Handler_Flag : virtual public Handler {
  private:
    const Flag_Base* volatile* holder;
    Flag& flag;
  public:
    Handler_Flag(const Flag_Base* volatile* h, Flag& f)
      : holder(h), flag(f)
    { }
    void act() const {
      if (*holder == 0
	  || static_cast<const Flag*>(*holder)->priority() < flag.priority())
	*holder = &flag;
    }
  };

  class Handler_Function : virtual public Handler {
  private:
    void (*function)();
  public:
    Handler_Function(void (*f)())
      : function(f)
    { }
    void act() const {
      (*function)();
    }
  };

  // The pending watchdog events.
  class Pending_Element {
  public:
    Time deadline;
    const Handler* handler;
    bool* p_expired_flag;
    Pending_Element(const Time& d, const Handler* h, bool* p)
      : deadline(d), handler(h), p_expired_flag(p)
    { }
  };

  typedef std::list<Pending_Element> Pending;

  bool expired;
  const Handler* handler;
  Pending::iterator pending_position;

public:
  static void initialize();

  template <typename Flag_Base, typename Flag>
  Watchdog(int units, const Flag_Base* volatile* holder, Flag& flag);

  Watchdog(int units, void (*function)());
  ~Watchdog();

private:
  // Just to prevent their use.
  Watchdog(const Watchdog&);
  Watchdog& operator=(const Watchdog&);

  // Pass this to getitimer.
  static itimerval current_timer_status;
  // Get the timer value.
  static void get_timer(Time& time);
  // Pass this to setitimer.
  static itimerval signal_once;
  // Last time value we set the timer to.
  static Time last_time_requested;
  // Set the timer value.
  static void set_timer(const Time& time);
  // Stops the timer.
  static void stop_timer();
  // Quick reschedule to avoid race conditions.
  static void reschedule();
  // Used by the above.
  static Time reschedule_time;
  // Records the time elapsed since last fresh start.
  static Time time_so_far;
  // The ordered queue of pending watchdog events.
  static Pending pending;
  // The actual signal handler.
  static void handle_timeout(int);
  // Inserts a new watchdog even at the right place in pending.
  static Pending::iterator insert_pending(const Time& deadline,
					  const Handler* handler,
					  bool* p_expired);
  // Handle the addition of a new watchdog event.
  static Pending::iterator new_watchdog_event(int units,
					      const Handler* handler,
					      bool* p_expired);
  // Handle the removal of a watchdog event.
  void remove_watchdog_event(Pending::iterator position);
  // Whether the alarm clock is running.
  static volatile bool alarm_clock_running;
  // Whether we are changing data which is also changed by the signal handler.
  static volatile bool in_critical_section;
};

} // namespace Parma_Watchdog_Library

#include "Watchdog.inlines.hh"

#endif
