/* Test Powerset<CS>.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef NOISY 
#define NOISY 0
#endif

namespace PPL = Parma_Polyhedra_Library;
namespace PPL_IO =  PPL::IO_Operators;

//namespace {

// A class for representing Finite Conjunctions of Attribute Independent
// Boolean Variable Properties.
class Fcaibvp : public std::set<PPL::Variable, PPL::Variable::Compare> {
private:
  typedef PPL::Variable::Compare Compare;
  typedef std::set<PPL::Variable, Compare> Base;

public:
  Fcaibvp()
    : Base() {
  }

  Fcaibvp(const PPL::Variable& x)
    : Base() {
    insert(x);
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
    std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
			  std::inserter(z, z.begin()),
			  Compare());
    std::swap(x, z);
  }

  bool OK() const {
    return true;
  }
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
  s << "{";
  return s;
}

//} // namespace

int main() TRY {
  set_handlers();
  PPL::Variable A(0);

  PPL::Powerset<Fcaibvp> ps1;
  ps1.add_disjunct(Fcaibvp(A));

  PPL::Powerset<Fcaibvp> ps2;

  ps1.least_upper_bound_assign(ps2);

  ps1.meet_assign(ps2);

  //std::cout << ps1 << std::endl;

  return 0;
}
CATCH
