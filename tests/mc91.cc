/* Test the constraint systems' constructions with McCarthy's 91 function.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

class Poly : public C_Polyhedron {
public:
  Poly()
    : C_Polyhedron() {
  }
  Poly(const Poly& y)
    : C_Polyhedron(y) {
  }
  Poly(unsigned int dimension)
    : C_Polyhedron(dimension) {
  }
  Poly& operator=(const Poly& y) {
    C_Polyhedron::operator=(y);
    return *this;
  }
  friend std::ostream& operator<<(std::ostream& s, const Poly& x);
};

std::ostream&
operator<<(std::ostream& s, const Poly& x) {
  const Parma_Polyhedra_Library::ConSys& cs = x.constraints();
  Parma_Polyhedra_Library::ConSys::const_iterator i = cs.begin();
  Parma_Polyhedra_Library::ConSys::const_iterator cs_end = cs.end();
  s << "{ ";
  while (i != cs_end) {
    s << *i++;
    if (i != cs_end)
      s << ", ";
  }
  s << " }";
  return s;
}

typedef Determinate<Poly> DCS;

typedef PowerSet<DCS> PCS;

typedef AskTell<DCS> ACS;

//typedef AskTell<AskTell<DCS> > AACS;
//typedef AskTell<PowerSet<PowerSet<DCS> > > APPCS;

int
main() try {
  Variable x(0);
  Variable y(1);

  Poly ph1(2);
  ph1.add_constraint(x <= 101);
  ph1.add_constraint(y == 91);

  Poly ph2(2);
  ph2.add_constraint(x >= 102);
  ph2.add_constraint(y == x-10);

  DCS d1(ph1);
  cout << d1 << endl;

  DCS d2(ph2);
  cout << d2 << endl;

  DCS d3 = d1;
  d3 += d2;
  cout << d3 << endl;

  PCS p1;
  p1.inject(d1);
  cout << p1 << endl;

  PCS p2;
  p2.inject(d2);
  cout << p2 << endl;

  p1 += p2;
  cout << p1 << endl;

  p1 *= p2;
  cout << p1 << endl;

  ACS a1;
  a1.inject(d1, d2);
  cout << a1 << endl;

  Poly top(2);
  Poly y_91(2);
  y_91.add_constraint(y == 91);

  ACS a2;
  a2.inject(top, y_91);
  cout << a2 << endl;
}
catch(std::exception& e) {
  cerr << "EXCEPTION!!!" << endl
       << e.what() << endl;
}
