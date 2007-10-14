/* Polynomial_Constraint class implementation (non-inline functions).
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
#include "Polynomial_Constraint.defs.hh"
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Polynomial_Constraint::throw_dimension_incompatible(const char* method,
					      const char* name_var,
					      const Term& t) const {
  std::ostringstream s;
  s << "PPL::Polynomial_Constraint::" << method << ":" << std::endl
    << "this->space_dimension() == " << space_dimension() << ", "
    << name_var << ".space_dimension() == " << t.space_dimension() << ".";
  throw std::invalid_argument(s.str());
}

bool
PPL::Polynomial_Constraint::is_tautological() const {
  const int constant_term_coefficient_sign
    = sgn(poly.constant_term_coefficient());
  switch (rel) {
  case Polynomial_Constraint::EQUALITY:
    if (constant_term_coefficient_sign != 0)
      return false;
    break;
  case Polynomial_Constraint::NONSTRICT_INEQUALITY:
    if (constant_term_coefficient_sign < 0)
      return false;
    break;
  case Polynomial_Constraint::STRICT_INEQUALITY:
    if (constant_term_coefficient_sign <= 0)
      return false;
    break;
  }
  Polynomial::Map::const_iterator i = poly.map.begin();
  Polynomial::Map::const_iterator p_end = poly.map.end();
  if (i != p_end) {
    if (i->first != Term::one())
      return false;
    else {
      ++i;
      if (i != p_end && i->first != Term::one())
	return false;
    }
  }
  return true;
}

bool
PPL::Polynomial_Constraint::is_inconsistent() const {
  const int constant_term_coefficient_sign
    = sgn(poly.constant_term_coefficient());
  switch (rel) {
  case Polynomial_Constraint::EQUALITY:
    if (constant_term_coefficient_sign == 0)
      return false;
    break;
  case Polynomial_Constraint::NONSTRICT_INEQUALITY:
    if (constant_term_coefficient_sign >= 0)
      return false;
    break;
  case Polynomial_Constraint::STRICT_INEQUALITY:
    if (constant_term_coefficient_sign > 0)
      return false;
    break;
  }
  Polynomial::Map::const_iterator i = poly.map.begin();
  Polynomial::Map::const_iterator p_end = poly.map.end();
  if (i != p_end) {
    if (i->first != Term::one())
      return false;
    else {
      ++i;
      if (i != p_end && i->first != Term::one())
	return false;
    }
  }
  return true;
}

/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s,
			      const Polynomial_Constraint& pc) {
  bool first = true;
  const Polynomial& p = pc.poly;
  for (Polynomial::const_iterator i = p.begin(),
	 p_end = p.end(); i != p_end; ++i) {
    const Term& t = i->term();
    if (t == Term::one())
      continue;

    Coefficient ev = i->coefficient();
    assert(ev != 0);
    if (!first) {
      if (ev > 0)
	s << " + ";
      else {
	s << " - ";
	neg_assign(ev);
      }
    }
    else
      first = false;
    if (ev == -1)
      s << "-";
    else if (ev != 1)
      s << ev << "*";
    s << i->term();
  }
  if (first)
    s << Coefficient_zero();
  const char* relation_symbol = 0;
  switch (pc.relation()) {
  case Polynomial_Constraint::EQUALITY:
    relation_symbol = " = ";
    break;
  case Polynomial_Constraint::NONSTRICT_INEQUALITY:
    relation_symbol = " >= ";
    break;
  case Polynomial_Constraint::STRICT_INEQUALITY:
    relation_symbol = " > ";
    break;
  }
  Coefficient ctc = p.constant_term_coefficient();
  neg_assign(ctc);
  s << relation_symbol << ctc;
  return s;
}

/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s,
			      const Polynomial_Constraint::Relation& t) {
  const char* n = 0;
  switch (t) {
  case Polynomial_Constraint::EQUALITY:
    n = "EQUALITY";
    break;
  case Polynomial_Constraint::NONSTRICT_INEQUALITY:
    n = "NONSTRICT_INEQUALITY";
    break;
  case Polynomial_Constraint::STRICT_INEQUALITY:
    n = "STRICT_INEQUALITY";
    break;
  }
  s << n;
  return s;
}

bool
PPL::Polynomial_Constraint::OK() const {
  if (!poly.OK())
    return false;

  if (rel != Polynomial_Constraint::EQUALITY
      && rel != Polynomial_Constraint::NONSTRICT_INEQUALITY
      && rel != Polynomial_Constraint::STRICT_INEQUALITY) {
#ifndef NDEBUG
    std::cerr << "Polynomial_Constraint has invalid relation "
	      << int(rel) << "."
	      << std::endl;
#endif
    return false;
  }

  // Normalization check.
  Polynomial_Constraint tmp = *this;
  tmp.strong_normalize();
  if (tmp != *this) {
#ifndef NDEBUG
    std::cerr << "Polynomial_Constraint is not strongly normalized as it should be."
	      << std::endl;
#endif
    return false;
  }

  // All tests passed.
  return true;
}
