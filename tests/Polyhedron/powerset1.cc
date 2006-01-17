/* Test Powerset<CS>.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include <algorithm>
#include <set>

namespace {

class Fcaibvp;

bool operator==(const Fcaibvp& x, const Fcaibvp& y);
bool operator!=(const Fcaibvp& x, const Fcaibvp& y);

std::ostream& operator<<(std::ostream& s, const Fcaibvp& x);

// A class for representing Finite Conjunctions of Attribute
// Independent Boolean Variable Properties.
class Fcaibvp {
private:
  typedef Variable::Compare Compare;
  typedef std::set<Variable, Compare> Set;

  Set set;

public:
  Fcaibvp()
    : set() {
  }

  explicit Fcaibvp(const Variable& x)
    : set() {
    set.insert(x);
  }

  memory_size_type total_memory_in_bytes() const {
    return 1;
  }

  bool is_top() const {
    return set.empty();
  }

  bool is_bottom() const {
    return false;
  }

  bool definitely_entails(const Fcaibvp& y) const{
    const Fcaibvp& x = *this;
    return std::includes(x.set.begin(), x.set.end(),
			 y.set.begin(), y.set.end(),
			 Compare());
  }

  void upper_bound_assign(const Fcaibvp& y) {
    set.insert(y.set.begin(), y.set.end());
  }

  void meet_assign(const Fcaibvp& y) {
    Fcaibvp& x = *this;
    Fcaibvp z;
    std::set_intersection(x.set.begin(), x.set.end(),
			  y.set.begin(), y.set.end(),
			  std::inserter(z.set, z.set.begin()),
			  Compare());
    std::swap(x, z);
  }

  bool OK() const {
    return true;
  }

  friend std::ostream& operator<<(std::ostream& s, const Fcaibvp& x);
};

std::ostream&
operator<<(std::ostream& s, const Fcaibvp& x) {
  s << "{";
  for (Fcaibvp::Set::const_iterator i = x.set.begin(),
	 x_end = x.set.end(); i != x_end; ++i) {
    const Variable& v = *i;
#if 0
    // FIXME: what is wrong with this?
    using namespace Parma_Polyhedra_Library::IO_Operators;
    s << v;
#else
    Parma_Polyhedra_Library::IO_Operators::operator<<(s, v);
#endif
    if (i != x_end)
      s << ", ";
  }
  s << "}";
  return s;
}

bool
operator==(const Fcaibvp& x, const Fcaibvp& y) {
  return x.definitely_entails(y) && y.definitely_entails(x);
}

bool
operator!=(const Fcaibvp& x, const Fcaibvp& y) {
  return !(x == y);
}

} // namespace

int
main() TRY {
  set_handlers();

  // Use every public Powerset method.

  typedef Powerset<Fcaibvp> PS;

  Variable A(0);

  PS ps1;
  ps1.add_disjunct(Fcaibvp(A));

  PS ps2 = ps1;

  if (ps2 != ps1 || !(ps2 == ps1))
    exit(1);

  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << "ps1:" << std::endl << ps1 << std::endl;

  Fcaibvp d(A);
  PS ps3(d);

  if (!ps1.definitely_entails(ps3))
    exit(1);

  if (ps3.is_top())
    exit(1);

  if (ps1.is_bottom())
    exit(1);

  nout << "Total memory: " << ps3.total_memory_in_bytes() << std::endl
       << "External memory: " << ps3.external_memory_in_bytes() << std::endl;

  if (!ps3.OK())
    exit(1);

  ps3.omega_reduce();

  if (ps3.size() == 0)
    exit(1);

  if (ps3.empty())
    exit(1);

  // Iterator.
  dimension_type count = 0;
  for (PS::iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    exit(1);

  // Constant iterator.
  count = 0;
  for (PS::const_iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    exit(1);

  // Reverse iterator.
  count = 0;
  for (PS::reverse_iterator i = ps3.rbegin(); i != ps3.rend(); ++i)
    ++count;
  if (count != 1)
    exit(1);

#if 0
  // Constant reverse iterator.
  count = 0;
  for (PS::const_reverse_iterator i = ps3.rbegin(); i != ps3.rend(); ++i)
    ++count;
  if (count != 1)
    exit(1);
#endif

  // Omega iterator typedef.
  count = 0;
  for (PS::omega_iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    exit(1);

  ps2 = ps3;
  PS ps_empty;
  ps2.drop_disjunct(ps2.begin());
  if (ps2 != ps_empty)
    exit(1);

  ps2 = ps3;
  ps2.drop_disjuncts(ps2.begin(),ps2.end());
  if (ps2 != ps_empty)
    exit(1);

  ps2 = ps3;
  ps2.clear();
  if (ps2 != ps_empty)
    exit(1);

  ps3.swap(ps2);
  ps3.swap(ps2);
  if (ps3 != ps1 || ps2 != ps_empty)
    exit(1);

  ps2 = ps_empty;
  ps2.least_upper_bound_assign(ps3);
  if (ps2 != ps3)
    exit(1);

  ps2 = ps_empty;
  ps2.upper_bound_assign(ps3);
  if (ps2 != ps3)
    exit(1);

  Variable B(1);
  ps2 = ps1;
  ps2.meet_assign(ps3);
  if (ps2 != ps3)
    exit(1);

  ps3.collapse();
  if (ps3.size() != 1)
    exit(1);

  return 0;
}
CATCH
