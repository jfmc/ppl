/* Definitions of simple functions for printing timings.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>

#include "timings.hh"
#include <cassert>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <cstring>
#include <cerrno>

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

using namespace std;

// To save the time when start_clock is called.
static struct timeval saved_ru_utime;

void
start_clock() {
  struct rusage rsg;
  if (getrusage(RUSAGE_SELF, &rsg) != 0) {
    cerr << "getrusage failed: " << strerror(errno) << endl;
    exit(1);
  }
  else
    saved_ru_utime = rsg.ru_utime;
}

void
print_clock(ostream& s) {
  struct rusage rsg;
  if (getrusage(RUSAGE_SELF, &rsg) != 0) {
    cerr << "getrusage failed: " << strerror(errno) << endl;
    exit(1);
  }
  else {
    time_t current_secs = rsg.ru_utime.tv_sec;
    time_t current_usecs = rsg.ru_utime.tv_usec;
    time_t saved_secs = saved_ru_utime.tv_sec;
    time_t saved_usecs = saved_ru_utime.tv_usec;
    time_t secs;
    time_t hsecs;
    if (current_usecs < saved_usecs) {
      hsecs = (((1000000 + current_usecs) - saved_usecs) + 5000) / 10000;
      secs = (current_secs - saved_secs) -1;
    }
    else {
      hsecs = ((current_usecs - saved_usecs) + 5000) / 10000;
      secs = current_secs - saved_secs;
    }
    assert(hsecs >= 0 && hsecs < 100 && secs >= 0);
    int fill_char = s.fill();
    s << secs << "." << setfill('0') << setw(2) << hsecs;
    s.fill(fill_char);
  }
}
