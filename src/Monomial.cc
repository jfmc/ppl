/* Monomial class implementation (non-inline functions).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#include <ppl-config.h>
#include "Monomial.defs.hh"
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

bool
PPL::Monomial::OK() const {
  return coefficient() != 0 || term() == Term::one();
}

/*! \relates Parma_Polyhedra_Library::Monomial */
void
PPL::pow_assign(Monomial& m, const dimension_type n) {
  Monomial acc = Monomial::one();
  for (dimension_type i = n; i-- > 0; )
    acc *= m;
  m.swap(acc);
}

void
PPL::Monomial::ascii_dump(std::ostream& s) const {
  s << "m( " << coefficient() << ' ';
  term().ascii_dump(s);
  s << " )";
}

PPL_OUTPUT_DEFINITIONS(Monomial);

bool
PPL::Monomial::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "m(")
    return false;

  if (!(s >> coefficient_ref()))
    return false;

  if (!term_ref().ascii_load(s))
    return false;

  if (!(s >> str) || (str != ")"))
    return false;

  // Check for well-formedness.
  assert(OK());
  return true;
}

/*! \relates Parma_Polyhedra_Library::Monomial */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Monomial& m) {
  const Coefficient& c = m.coefficient();
  const Term& t = m.term();
  if (c == 0 || t == Term::one())
    s << c;
  else {
    if (c != Coefficient_one())
      s << c << '*';
    s << t;
  }
  return s;
}
