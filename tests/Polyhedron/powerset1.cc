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
    Fcaibvp& x = *this;
    Fcaibvp z;
    std::set_intersection(x.set.begin(), x.set.end(),
			  y.set.begin(), y.set.end(),
			  std::inserter(z.set, z.set.begin()),
			  Compare());
    std::swap(x, z);
  }

  void meet_assign(const Fcaibvp& y) {
    set.insert(y.set.begin(), y.set.end());
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
	 x_end = x.set.end(); i != x_end; ) {
    const Variable& v = *i++;
#if 0
    // GCC 3.3.3 does not accept this.
    using IO_Operators::operator<<;
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

// Uses every public Powerset method.
bool
test01() {
  typedef Powerset<Fcaibvp> PS;

  Variable A(0);

  PS ps1;
  ps1.add_disjunct(Fcaibvp(A));

  PS ps2 = ps1;

  if (ps2 != ps1 || !(ps2 == ps1))
    return false;

#if 0
  // GCC 3.3.3 does not accept this.
  using IO_Operators::operator<<;
  nout << "ps1:" << endl << ps1 << endl;
#else
  nout << "ps1:" << endl;
  Parma_Polyhedra_Library::IO_Operators::operator<<(nout, ps1);
  nout << endl;
#endif

  Fcaibvp d(A);
  PS ps3(d);

  if (!ps1.definitely_entails(ps3))
    return false;

  if (ps3.is_top())
    return false;

  if (ps1.is_bottom())
    return false;

  nout << "Total memory: " << ps3.total_memory_in_bytes() << endl
       << "External memory: " << ps3.external_memory_in_bytes() << endl;

  ps3.omega_reduce();

  if (ps3.size() == 0)
    return false;

  if (ps3.empty())
    return false;

  // Iterator.
  dimension_type count = 0;
  for (PS::iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    return false;

  // Constant iterator.
  count = 0;
  for (PS::const_iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    return false;

  // Reverse iterator.
  count = 0;
  for (PS::reverse_iterator i = ps3.rbegin(); i != ps3.rend(); ++i)
    ++count;
  if (count != 1)
    return false;

  // Constant reverse iterator.
  count = 0;
  for (PS::const_reverse_iterator i = ps3.rbegin(),
	 ps3_rend = ps3.rend(); i != ps3_rend; ++i)
    ++count;
  if (count != 1)
    return false;

  // Omega iterator typedef.
  count = 0;
  for (PS::omega_iterator i = ps3.begin(); i != ps3.end(); ++i)
    ++count;
  if (count != 1)
    return false;

  ps2 = ps3;
  PS ps_empty;
  ps2.drop_disjunct(ps2.begin());
  if (ps2 != ps_empty)
    return false;

  ps2 = ps3;
  ps2.drop_disjuncts(ps2.begin(),ps2.end());
  if (ps2 != ps_empty)
    return false;

  ps2 = ps3;
  ps2.clear();
  if (ps2 != ps_empty)
    return false;

  ps3.swap(ps2);
  ps3.swap(ps2);
  if (ps3 != ps1 || ps2 != ps_empty)
    return false;

  ps2 = ps_empty;
  ps2.least_upper_bound_assign(ps3);
  if (ps2 != ps3)
    return false;

  ps2 = ps_empty;
  ps2.upper_bound_assign(ps3);
  if (ps2 != ps3)
    return false;

  Variable B(1);
  ps2 = ps1;
  ps2.meet_assign(ps3);
  if (ps2 != ps3)
    return false;

  ps3.collapse();
  if (ps3.size() != 1)
    return false;

  return true;
}

bool
test02() {
  Variable X(0);
  Variable Y(1);

  Fcaibvp XY(X);
  XY.meet_assign(Fcaibvp(Y));

  Powerset<Fcaibvp> ps;

  ps.add_disjunct(Fcaibvp(X));
  ps.add_disjunct(XY);
  return ps.OK();
}

bool
test03() {
  Variable X(0);
  Variable Y(1);

  Fcaibvp XY(X);
  XY.meet_assign(Fcaibvp(Y));

  Powerset<Fcaibvp> ps;

  ps.add_disjunct(XY);
  ps.add_disjunct(Fcaibvp(X));
  return ps.OK();
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
END_MAIN
