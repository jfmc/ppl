/* Watchdog and associated classes' declaration and inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
site http://www.cs.unipr.it/Software/ . */

#ifndef _Watchdog_defs_hh
#define _Watchdog_defs_hh 1

namespace Parma_Watchdog_Library {
  class Watchdog;
  class Timeout;
}

#include <list>
#include <cassert>
#include <sys/time.h>

//! A base class for timeout exceptions.
class Parma_Watchdog_Library::Timeout {
public:
  virtual int priority() const = 0;
};

//! A watchdog timer.
class Parma_Watchdog_Library::Watchdog {
private:
  // Positive times only here!
  class Time {
  public:
    int seconds;
    int microseconds;

    Time()
      : seconds(0), microseconds(0) {
    }

    explicit Time(int units)
      : seconds(units / 100), microseconds((units * 10000) % 1000000) {
      assert(units >= 0);
    }

    void reset() {
      seconds = microseconds = 0;
    }

    Time& operator +=(const Time& y) {
      int secs = seconds + y.seconds;
      int usecs = microseconds + y.microseconds;
      if (usecs >= 1000000) {
	++secs;
	usecs %= 1000000;
      }
      seconds = secs;
      microseconds = usecs;
      return *this;
    }

    Time& operator -=(const Time& y) {
      int secs = seconds - y.seconds;
      int usecs = microseconds - y.microseconds;
      if (usecs < 0) {
	--secs;
	usecs += 1000000;
      }
      if (secs < 0)
	secs = usecs = 0;
      seconds = secs;
      microseconds = usecs;
      return *this;
    }

    friend Time operator -(const Time& x, const Time& y) {
      Time z(x);
      z -= y;
      return z;
    }

    friend bool operator ==(const Time& x, const Time& y) {
      return x.seconds == y.seconds && x.microseconds == y.microseconds;
    }

    friend bool operator !=(const Time& x, const Time& y) {
      return !(x == y);
    }

    friend bool operator <(const Time& x, const Time& y) {
      return x.seconds < y.seconds
	|| (x.seconds == y.seconds && x.microseconds < y.microseconds);
    }

    friend bool operator <=(const Time& x, const Time& y) {
      return x < y || x == y;
    }

    friend bool operator >(const Time& x, const Time& y) {
      return y < x;
    }

    friend bool operator >=(const Time& x, const Time& y) {
      return y <= x;
    }
  };

  // Different kinds of handler for the watchdog events.
  class Handler {
  public:
    virtual void act() const = 0;
  };

  class Handler_Flag : virtual public Handler {
  private:
    Timeout& exception;
    volatile Timeout** flag;
  public:
    Handler_Flag(Timeout& e,
		 volatile Timeout** f)
      : exception(e), flag(f)
    { }
    void act() const {
      *flag = &exception;
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

  Watchdog(int units,
	   Timeout& exception,
	   volatile Timeout** flag);
  Watchdog(int units,
	   void (*function)());
  ~Watchdog();

private:
  // Just to prevent their use.
  Watchdog(const Watchdog&);
  Watchdog& operator = (const Watchdog&);

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

namespace Parma_Watchdog_Library {

inline void
Watchdog::reschedule() {
  set_timer(reschedule_time);
}

} // namespace Parma_Watchdog_Library

#endif
