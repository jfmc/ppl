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

namespace Parma_Watchdog_Library {

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
operator+(const Watchdog::Time& x, const Watchdog::Time& y) {
  Watchdog::Time z = x;
  z += y;
  return z;
}

inline Watchdog::Time
operator-(const Watchdog::Time& x, const Watchdog::Time& y) {
  Watchdog::Time z = x;
  z -= y;
  return z;
}

inline bool
operator==(const Watchdog::Time& x, const Watchdog::Time& y) {
  return x.seconds() == y.seconds() && x.microseconds() == y.microseconds();
}

inline bool
operator!=(const Watchdog::Time& x, const Watchdog::Time& y) {
  return !(x == y);
}

inline bool
operator<(const Watchdog::Time& x, const Watchdog::Time& y) {
  return x.seconds() < y.seconds()
    || (x.seconds() == y.seconds() && x.microseconds() < y.microseconds());
}

inline bool
operator<=(const Watchdog::Time& x, const Watchdog::Time& y) {
  return x < y || x == y;
}

inline bool
operator>(const Watchdog::Time& x, const Watchdog::Time& y) {
  return y < x;
}

inline bool
operator>=(const Watchdog::Time& x, const Watchdog::Time& y) {
  return y <= x;
}

inline void
Watchdog::reschedule() {
  set_timer(reschedule_time);
}

} // namespace Parma_Watchdog_Library
