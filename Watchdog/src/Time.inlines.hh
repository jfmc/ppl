/* Time class implementation: inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PWL_Time_inlines_hh
#define PWL_Time_inlines_hh 1

#include <cassert>

namespace Parma_Watchdog_Library {

inline
Time::Time()
  : secs(0), microsecs(0) {
  assert(OK());
}

inline
Time::Time(unsigned long hundredths_of_a_second)
  : secs(hundredths_of_a_second / HSECS_IN_SEC),
    microsecs((hundredths_of_a_second % HSECS_IN_SEC)
              * (MUSECS_IN_SEC/HSECS_IN_SEC)) {
  assert(OK());
}

inline
Time::Time(unsigned long s, unsigned long m)
  : secs(s),
    microsecs(m) {
  if (microsecs >= MUSECS_IN_SEC) {
    secs += microsecs / MUSECS_IN_SEC;
    microsecs %= MUSECS_IN_SEC;
  }
  assert(OK());
}

inline unsigned long
Time::seconds() const {
  return secs;
}

inline unsigned long
Time::microseconds() const {
  return microsecs;
}

inline Time&
Time::operator+=(const Time& y) {
  unsigned long r_secs = secs + y.secs;
  unsigned long r_microsecs = microsecs + y.microsecs;
  if (r_microsecs >= MUSECS_IN_SEC) {
    ++r_secs;
    r_microsecs %= MUSECS_IN_SEC;
  }
  secs = r_secs;
  microsecs = r_microsecs;
  assert(OK());
  return *this;
}

inline Time&
Time::operator-=(const Time& y) {
  long r_secs = secs - y.secs;
  long r_microsecs = microsecs - y.microsecs;
  if (r_microsecs < 0) {
    --r_secs;
    r_microsecs += MUSECS_IN_SEC;
  }
  if (r_secs < 0)
    r_secs = r_microsecs = 0;
  secs = r_secs;
  microsecs = r_microsecs;
  assert(OK());
  return *this;
}

inline Time
operator+(const Time& x, const Time& y) {
  Time z = x;
  z += y;
  return z;
}

inline Time
operator-(const Time& x, const Time& y) {
  Time z = x;
  z -= y;
  return z;
}

inline bool
operator==(const Time& x, const Time& y) {
  assert(x.OK() && y.OK());
  return x.seconds() == y.seconds() && y.microseconds() == y.microseconds();
}

inline bool
operator!=(const Time& x, const Time& y) {
  assert(x.OK() && y.OK());
  return !(x == y);
}

inline bool
operator<(const Time& x, const Time& y) {
  assert(x.OK() && y.OK());
  return x.seconds() < y.seconds()
    || (x.seconds() == y.seconds() && x.microseconds() < y.microseconds());
}

inline bool
operator<=(const Time& x, const Time& y) {
  return x < y || x == y;
}

inline bool
operator>(const Time& x, const Time& y) {
  return y < x;
}

inline bool
operator>=(const Time& x, const Time& y) {
  return y <= x;
}

} // namespace Parma_Watchdog_Library

#endif // !defined(PWL_Time_inlines_hh)
