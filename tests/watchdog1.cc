/* Test the timeout facility of the library.
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

#include "ppl_test.hh"
#include "pwl_install.hh"
#include "timings.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static void
compute_open_hypercube_generators(dimension_type dimension) {
  NNC_Polyhedron hypercube(dimension);
  for (dimension_type i = 0; i < dimension; ++i) {
    Variable x(i);
    hypercube.add_constraint(x > 0);
    hypercube.add_constraint(x < 1);
  }
  (void) hypercube.generators();
}

class myTimeout : virtual public exception,
		  public Parma_Polyhedra_Library::Throwable {
public:
  const char* what() const throw() {
    return "myTimeout in watchdog1.cc";
  }
  void throw_me() const {
    throw *this;
  }
  int priority() const {
    return 0;
  }
  myTimeout() {
  }
};

static myTimeout t;

static bool
timed_compute_open_hypercube_generators(dimension_type dimension,
					int hundredth_secs) {
  try {
    Parma_Watchdog_Library::Watchdog
      w(hundredth_secs, &abandon_exponential_computations, t);
#if NOISY
    start_clock();
#endif
    compute_open_hypercube_generators(dimension);
    abandon_exponential_computations = 0;
    return true;
  }
  catch (const myTimeout& e) {
    abandon_exponential_computations = 0;
#if NOISY
    cout << e.what() << " after ";
    print_clock(cout);
    cout << " s" << endl;
#endif
    return false;
  }
  catch (...) {
    exit(1);
  }
  // Should never get here.
  return false;
}

#define INIT_TIME 20

int
main() TRY {
  set_handlers();

  // Find a dimension that cannot be computed with a INIT_TIME timeout.
  dimension_type dimension = 0;
  do {
    ++dimension;
#if NOISY
    cout << "Trying dimension " << dimension << endl;
#endif
  }
  while (timed_compute_open_hypercube_generators(dimension, INIT_TIME));

  // Now find an upper bound to the time necessary to compute it.
  int upper_bound = INIT_TIME;
  do {
    upper_bound *= 2;
#if NOISY
    cout << "Trying upper bound " << upper_bound << endl;
#endif
  }
  while (!timed_compute_open_hypercube_generators(dimension, upper_bound));

  // Search the "exact" time.
  int lower_bound = upper_bound/2;
  do {
    int test_time = (lower_bound+upper_bound)/2;
#if NOISY
    cout << "Probing " << test_time << endl;
#endif
    if (timed_compute_open_hypercube_generators(dimension, test_time))
      upper_bound = test_time;
    else
      lower_bound = test_time;
  } while (upper_bound-lower_bound > 4);

#if NOISY
  cout << "Estimated time for dimension " << dimension
       << ": " << (lower_bound+upper_bound)/2 << " 100th of sec" << endl;
#endif

  return 0;
}
CATCH
