/* Watchdog and associated classes' implementation: inline functions.
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

#ifndef PWL_Watchdog_inlines_hh
#define PWL_Watchdog_inlines_hh 1

namespace Parma_Watchdog_Library {

template <typename Flag_Base, typename Flag>
inline
Watchdog::Handler_Flag<Flag_Base,
		       Flag>::Handler_Flag(const Flag_Base* volatile* h,
					   Flag& f)
			 : holder(h), flag(f) {
}

template <typename Flag_Base, typename Flag>
inline void
Watchdog::Handler_Flag<Flag_Base, Flag>::act() const {
  if (*holder == 0
      || static_cast<const Flag*>(*holder)->priority() < flag.priority())
    *holder = &flag;
}

inline
Watchdog::Handler_Function::Handler_Function(void (*f)())
  : function(f) {
}

inline void
Watchdog::Handler_Function::act() const {
  (*function)();
}

inline
Watchdog::Pending_Element::Pending_Element(const Time& d,
					   const Handler* h,
					   bool* p)
  : deadline(d), handler(h), p_expired_flag(p) {
}

inline
Watchdog::Time::Time()
  : secs(0), microsecs(0) {
}

inline
Watchdog::Time::Time(unsigned int units)
  // FIXME: overflow is possible here!
  : secs(units / 100), microsecs((units * 10000) % 1000000) {
}

inline void
Watchdog::Time::set(int s, int m) {
    secs = s;
    microsecs = m;
  }

inline void
Watchdog::Time::reset() {
  secs = microsecs = 0;
}

inline int
Watchdog::Time::seconds() const {
  return secs;
}

inline int
Watchdog::Time::microseconds() const {
  return microsecs;
}

inline Watchdog::Time&
Watchdog::Time::operator+=(const Time& y) {
  int r_secs = secs + y.secs;
  int r_microsecs = microsecs + y.microsecs;
  if (r_microsecs >= 1000000) {
    ++r_secs;
    r_microsecs %= 1000000;
  }
  secs = r_secs;
  microsecs = r_secs;
  return *this;
}

inline Watchdog::Time&
Watchdog::Time::operator-=(const Time& y) {
  int r_secs = secs - y.secs;
  int r_microsecs = microsecs - y.microsecs;
  if (r_microsecs < 0) {
    --r_secs;
    r_microsecs += 1000000;
  }
  if (r_secs < 0)
    r_secs = r_microsecs = 0;
  secs = r_secs;
  microsecs = r_microsecs;
  return *this;
}

inline Watchdog::Time
Watchdog::Time::operator+(const Time& y) const {
  Time z = *this;
  z += y;
  return z;
}

inline Watchdog::Time
Watchdog::Time::operator-(const Time& y) const {
  Time z = *this;
  z -= y;
  return z;
}

inline bool
Watchdog::Time::operator==(const Time& y) const {
  return seconds() == y.seconds() && microseconds() == y.microseconds();
}

inline bool
Watchdog::Time::operator!=(const Time& y) const {
  return !(*this == y);
}

inline bool
Watchdog::Time::operator<(const Time& y) const {
  return seconds() < y.seconds()
    || (seconds() == y.seconds() && microseconds() < y.microseconds());
}

inline bool
Watchdog::Time::operator<=(const Time& y) const {
  return *this < y || *this == y;
}

inline bool
Watchdog::Time::operator>(const Time& y) const {
  return y < *this;
}

inline bool
Watchdog::Time::operator>=(const Time& y) const {
  return y <= *this;
}

inline void
Watchdog::reschedule() {
  set_timer(reschedule_time);
}

template <typename Flag_Base, typename Flag>
inline
Watchdog::Watchdog(int units, const Flag_Base* volatile* holder, Flag& flag)
  : expired(false),
    handler(new Handler_Flag<Flag_Base, Flag>(holder, flag)) {
  in_critical_section = true;
  pending_position = new_watchdog_event(units, handler, &expired);
  in_critical_section = false;
}

inline
Watchdog::Watchdog(int units, void (*function)())
  : expired(false), handler(new Handler_Function(function)) {
  in_critical_section = true;
  pending_position = new_watchdog_event(units, handler, &expired);
  in_critical_section = false;
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Watchdog_inlines_hh)
