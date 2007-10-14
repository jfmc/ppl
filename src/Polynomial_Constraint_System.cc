/* Polynomial_Constraint_System class implementation (non-inline functions).
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
#include "Polynomial_Constraint_System.defs.hh"
#include "Polynomial_Constraint_System.inlines.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

bool
PPL::Polynomial_Constraint_System::has_strict_inequalities() const {
  for (const_iterator i = begin(), pcs_end = end(); i != pcs_end; ++i)
    if (i->is_strict_inequality())
      return true;
  return false;
}

PPL::dimension_type
PPL::Polynomial_Constraint_System::space_dimension() const {
  dimension_type d = 0;
  for (const_iterator i = begin(), pcs_end = end(); i != pcs_end; ++i) {
    dimension_type e = i->space_dimension();
    if (e > d)
      d = e;
  }
  return d;
}

PPL::dimension_type
PPL::Polynomial_Constraint_System::num_inequalities() const {
  dimension_type n = 0;
  for (const_iterator i = begin(), pcs_end = end(); i != pcs_end; ++i)
    if (!i->is_equality())
      ++n;
  return n;
}

void
PPL::Polynomial_Constraint_System::ascii_dump(std::ostream&) const {
  // FIXME!!!
  abort();
#if 0
  const Polynomial_Constraint_System& x = *this;
  const dimension_type x_num_constraints = x.num_constraints();
  s << x_num_constraints 
    << "\n";
  for (dimension_type i = 0; i < x_num_rows; ++i) {
    const Polynomial_Constraint& c = x[i];
    for (dimension_type j = 0; j < x_num_columns; ++j)
      s << c[j] << ' ';
    switch (c.type()) {
    case Polynomial_Constraint::EQUALITY:
      s << "=";
      break;
    case Polynomial_Constraint::NONSTRICT_INEQUALITY:
      s << ">=";
      break;
    case Polynomial_Constraint::STRICT_INEQUALITY:
      s << ">";
      break;
    }
    s << "\n";
  }
#endif
}

bool
PPL::Polynomial_Constraint_System::ascii_load(std::istream&) {
  // FIXME!!!
  abort();
#if 0
  std::string str;
  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
      return false;
  resize_no_copy(nrows, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  set_sorted(str == "(sorted)");
  dimension_type index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> index))
    return false;
  set_index_first_pending_row(index);

  Polynomial_Constraint_System& x = *this;
  for (dimension_type i = 0; i < x.num_rows(); ++i) {
    for (dimension_type j = 0; j < x.num_columns(); ++j)
      if (!(s >> x[i][j]))
	return false;

    if (!(s >> str))
      return false;
    if (str == "=")
      x[i].set_is_equality();
    else
      x[i].set_is_inequality();

    // Checking for equality of actual and declared types.
    switch (x[i].type()) {
    case Polynomial_Constraint::EQUALITY:
      if (str == "=")
	continue;
      break;
    case Polynomial_Constraint::NONSTRICT_INEQUALITY:
      if (str == ">=")
	continue;
      break;
    case Polynomial_Constraint::STRICT_INEQUALITY:
      if (str == ">")
	continue;
      break;
    }
    // Reaching this point means that the input was illegal.
    return false;
  }
#endif
  // Check for well-formedness.
  assert(OK());
  return true;
}

bool
PPL::Polynomial_Constraint_System::OK() const {
  for (const_iterator i = begin(), pcs_end = end(); i != pcs_end; ++i)
    if (!i->OK())
      return false;

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Polynomial_Constraint_System */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s,
			      const Polynomial_Constraint_System& cs) {
  Polynomial_Constraint_System::const_iterator i = cs.begin();
  const Polynomial_Constraint_System::const_iterator cs_end = cs.end();
  if (i == cs_end)
    s << "true";
  else {
    while (i != cs_end) {
      s << *i++;
      if (i != cs_end)
	s << ", ";
    }
  }
  return s;
}
