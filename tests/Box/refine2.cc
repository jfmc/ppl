/* Test Box::refine(const Constraint_System&) with instances that may
   require a watchdog timer.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"
#include "pwl.hh"

namespace {

class Timeout : virtual public std::exception,
		public Parma_Polyhedra_Library::Throwable {
public:
  const char* what() const throw() {
    return "Timeout in refine1.cc";
  }

  void throw_me() const {
    throw *this;
  }

  int priority() const {
    return 0;
  }

  Timeout() {
  }

  ~Timeout() throw() {
  }
};

Timeout t;

bool
test01() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= -5);
  cs.insert(A <= 5);
  cs.insert(A == B);
  cs.insert(A == 2*B);

  print_constraints(cs, "*** cs ***");

  TBox box(2);

  bool ok = false;

  typedef TBox::interval_type::boundary_type boundary_type;
  if (std::numeric_limits<boundary_type>::is_exact
      && !std::numeric_limits<boundary_type>::is_integer) {
    // With interval boundaries made of rational numbers, this
    // refinement instance does not terminate: we use a watchdog timer.
    try {
      // Set a 0.1 seconds timeout.
      Parma_Watchdog_Library::Watchdog
	w(10, abandon_expensive_computations, t);

      box.refine(cs);

      // We should never get here.
      abandon_expensive_computations = 0;
      nout << "unexpected termination" << endl;
      ok = false;
    }
    catch (const Timeout&) {
      abandon_expensive_computations = 0;
      nout << "timeout, as expected" << endl;
      ok = true;
    }
    catch (...) {
      nout << "unexpected exception" << endl;
      ok = false;
    }
  }
  else {
    // With interval boundaries other than rational numbers, this instance
    // of refinement terminates rather quickly: no timer is necessary.
    box.refine(cs);
#if 0
    Rational_Box known_result(2);
    known_result.add_constraint(A >= 0);
    known_result.add_constraint(B == 5);
    known_result.add_constraint(B - A <= 5);

    ok = (Rational_Box(box1) == known_result);

    print_constraints(known_result, "*** known_result ***");
#endif
  }

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
