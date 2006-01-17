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

namespace PPL = Parma_Polyhedra_Library;
namespace PPL_IO =  PPL::IO_Operators;

using namespace PPL_IO;

//namespace {

// A class for representing Finite Conjunctions of Attribute
// Independent Boolean Variable Properties.
class Fcaibvp : private std::set<PPL::Variable, PPL::Variable::Compare> {
private:
  typedef PPL::Variable::Compare Compare;
  typedef std::set<PPL::Variable, Compare> Base;

public:

  // The constructors are only required for this test.

  Fcaibvp()
    : Base() {
  }

  Fcaibvp(const PPL::Variable& x)
    : Base() {
    insert(x);
  }

  memory_size_type total_memory_in_bytes() const {
    return 1;
  }

  bool is_top() const {
    return Base::empty();
  }

  bool is_bottom() const {
    return false;
  }

  bool definitely_entails(const Fcaibvp& y) const{
    const Fcaibvp& x = *this;
    return std::includes(x.begin(), x.end(), y.begin(), y.end(),
			 Compare());
  }

  void upper_bound_assign(const Fcaibvp& y) {
    Base::insert(y.begin(), y.end());
  }

  void meet_assign(const Fcaibvp& y) {
    Fcaibvp& x = *this;
    Fcaibvp z;
    // FIXME: Why does this need public set parent?
#if 0
    std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
			  std::inserter(z, z.begin()),
			  Compare());
#endif
    std::swap(x, z);
  }

  bool OK() const {
    return true;
  }

  friend bool operator==(const Fcaibvp& x, const Fcaibvp& y);
  friend std::ostream& operator<<(std::ostream& s, const Fcaibvp& x);
};

std::ostream&
operator<<(std::ostream& s, const Fcaibvp& x) {
  s << "{";
  for (Fcaibvp::const_iterator i = x.begin(),
	 x_end = x.end(); i != x_end; ++i) {
    const PPL::Variable& v = *i;
    using namespace Parma_Polyhedra_Library::IO_Operators;
    s << v;
    if (i != x_end)
      s << ", ";
  }
  s << "}";
  return s;
}

bool
operator==(const Fcaibvp& x, const Fcaibvp& y) {
  return x.size() == y.size();
}

//} // namespace

typedef PPL::Powerset<Fcaibvp> PS;

int main() TRY {
  set_handlers();

  // Use every public Powerset method.

  PPL::Variable A(0);

  PS ps1;
  ps1.add_disjunct(Fcaibvp(A));

  PS ps2 = ps1;

  if (ps2 != ps1 || !(ps2 == ps1))
    exit(1);

  nout << "ps1:" << std::endl << ps1 << std::endl;

  Fcaibvp d(A);
  PS ps3(d);
  PS ps3;

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
  for (PS::iterator i = ps3.begin();
       i != ps3.end();
       ++i)
    ++count;
  if (count != 1)
    exit(1);

  // Constant iterator.
  count = 0;
  for (PS::const_iterator i = ps3.begin();
       i != ps3.end();
       ++i)
    ++count;
  if (count != 1)
    exit(1);

  // Reverse iterator.
  count = 0;
  for (PS::reverse_iterator i = ps3.rbegin();
       i != ps3.rend();
       ++i)
    ++count;
  if (count != 1)
    exit(1);

#if 0
  // FIXME: Add operator!= for Powerset::const_reverse_iterator?
  // Constant reverse iterator.
  count = 0;
  for (PS::const_reverse_iterator i = ps3.rbegin();
       i != ps3.rend();
       ++i)
    ++count;
  if (count != 1)
    exit(1);
#endif

  // Omega iterator typedef.
  count = 0;
  for (PS::omega_iterator i = ps3.begin();
       i != ps3.end();
       ++i)
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

  PPL::Variable B(1);
  ps2 = ps1;
  ps2.meet_assign(ps3);
#if 0 // FIXME: Requires working Fcaibvp::meet_assign.
  if (ps2 != ps3)
    exit(1);
#endif

  ps3.collapse();
  if (ps3.size() != 1)
    exit(1);

  return 0;
}
CATCH
