/* Test Powerset<D>.
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

using namespace IO_Operators;

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

  explicit Fcaibvp(const Variables_Set& y)
    : set(y) {
  }

  Fcaibvp(const Fcaibvp& y, unsigned offset)
    : set() {
    for (Set::const_iterator i = y.set.begin(),
	   y_set_end = y.set.end(); i != y_set_end; ++i)
      set.insert(Variable(i->id() + offset));
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

  void difference_assign(const Fcaibvp& y) {
    Fcaibvp& x = *this;
    Fcaibvp z;
    std::set_difference(x.set.begin(), x.set.end(),
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
#if 0 // Old compilers may not understand the following.
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

} // namespace

typedef Ask_Tell<Fcaibvp> DEF_Formula;

namespace {

void
shift_rename_add(const DEF_Formula& p,
		 dimension_type offset,
		 DEF_Formula& q) {
  for (DEF_Formula::const_iterator i = p.begin(),
	 p_end = p.end(); i != p_end; ++i)
    q.add_pair(Fcaibvp(i->ask(), offset), Fcaibvp(i->tell(), offset));
}

void
remove_dimensions(DEF_Formula& f, const Variables_Set& to_remove) {
  f.normalize();
  DEF_Formula g;
  for (DEF_Formula::const_iterator i = f.begin(),
	 f_end = f.end(); i != f_end; ++i) {
    DEF_Formula h = f;
    const Fcaibvp& ask = i->ask();
    Fcaibvp projected_ask = ask;
    projected_ask.difference_assign(Fcaibvp(to_remove));
    h.add_pair(Fcaibvp(), projected_ask);
    if (h.definitely_entails(DEF_Formula(Fcaibvp(), ask))) {
      Fcaibvp projected_tell = i->tell();
      projected_tell.difference_assign(Fcaibvp(to_remove));
      g.add_pair(projected_ask, projected_tell);
    }
  }
  std::swap(f, g);
}

} // namespace

namespace {

bool test01() {
  Variable X(0);
  Variable Y(1);
  Variable Z(2);

  Fcaibvp XY(X);
  XY.meet_assign(Fcaibvp(Y));
  Fcaibvp XZ(X);
  XZ.meet_assign(Fcaibvp(Z));
  Fcaibvp XYZ(XY);
  XYZ.meet_assign(Fcaibvp(Z));

  DEF_Formula a1;
  a1.add_pair(Fcaibvp(), Fcaibvp(Z));
  a1.add_pair(XZ, XYZ);

  DEF_Formula a2;
  a2.add_pair(Fcaibvp(X), XY);

  DEF_Formula a = a1;
  a.upper_bound_assign(a2);

#if 0
  nout << "a1 = " << a1 << endl
       << "a2 = " << a2 << endl
       << "a = " << a << endl;
#endif

  // FIXME
  return true;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);

  // This is the base case:
  // append(A,B,C) :- A = [], B = C.
  DEF_Formula base;
  base.add_pair(Fcaibvp(), Fcaibvp(A));
  base.add_pair(Fcaibvp(B), Fcaibvp(C));
  base.add_pair(Fcaibvp(C), Fcaibvp(B));

  //nout << "*** base ***" << endl << base << endl;

  // This is the inductive case:
  // append(A,B,C) :- A = [D|E], B = F, C = [D|G], append(E,F,G).
  DEF_Formula inductive;
  Fcaibvp D_E(D);
  D_E.meet_assign(Fcaibvp(E));
  inductive.add_pair(Fcaibvp(A), D_E);
  inductive.add_pair(D_E, Fcaibvp(A));
  inductive.add_pair(Fcaibvp(B), Fcaibvp(F));
  inductive.add_pair(Fcaibvp(F), Fcaibvp(B));
  Fcaibvp D_G(D);
  D_G.meet_assign(Fcaibvp(G));
  inductive.add_pair(Fcaibvp(C), D_G);
  inductive.add_pair(D_G, Fcaibvp(C));

  //nout << "*** inductive ***" << endl << inductive << endl;

  // Initialize the fixpoint iteration.
  DEF_Formula current = base;

  //nout << "*** start ***" << endl << current << endl;

  // Contains the polyhedron computed at the previous iteration.
  DEF_Formula previous;
  do {
    previous = current;
    current = inductive;
    shift_rename_add(previous, 4, current);

    //nout << "*** after shift_rename_add ***" << endl << current << endl;

    Variables_Set dimensions_to_remove;
    // Deliberately inserted out of order (!).
    dimensions_to_remove.insert(D);
    dimensions_to_remove.insert(F);
    dimensions_to_remove.insert(E);
    dimensions_to_remove.insert(G);
    remove_dimensions(current, dimensions_to_remove);

    //nout << "*** after remove_dimensions ***" << endl << current << endl;

    current.upper_bound_assign(previous);

    //nout << "*** after upper_bound_assign ***" << endl << current << endl;

  } while (current != previous);

  DEF_Formula expected;
  Fcaibvp A_B(A);
  A_B.meet_assign(Fcaibvp(B));
  expected.add_pair(Fcaibvp(C), A_B);
  expected.add_pair(A_B, Fcaibvp(C));

  //nout << "*** expected ***" << endl << expected << endl;

  return current == expected;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN

