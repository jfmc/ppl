/* Status class implementation (non-inline functions).
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

#include <config.h>

#include "Status.defs.hh"

#include <iostream>
#include <string>
#include <cassert>

namespace PPL = Parma_Polyhedra_Library;

// These are the keywords that indicate the individual assertions.
static const std::string zero_dim_univ = "ZE";
static const std::string empty = "EM";
static const std::string consys_min = "CM";
static const std::string gensys_min = "GM";
static const std::string consys_upd = "CS";
static const std::string gensys_upd = "GS";
static const std::string sat_c = "SC";
static const std::string sat_g = "SG";
static const char yes = '+';
static const char no = '-';
static const char sep = ' ';

/*! \relates Parma_Polyhedra_Library::Status */
std::ostream&
PPL::operator<<(std::ostream& s, const Status& u) {
  s << (u.test_zero_dim_univ() ? yes : no) << zero_dim_univ << sep
    << (u.test_empty() ? yes : no) << empty << sep
    << sep
    << (u.test_c_minimized() ? yes : no) << consys_min << sep
    << (u.test_g_minimized() ? yes : no) << gensys_min << sep
    << sep
    << (u.test_c_up_to_date() ? yes : no) << consys_upd << sep
    << (u.test_g_up_to_date() ? yes : no) << gensys_upd << sep
    << sep
    << (u.test_sat_c_up_to_date() ? yes : no) << sat_c << sep
    << (u.test_sat_g_up_to_date() ? yes : no) << sat_g << sep;
  return s;
}

/*!
  Reads a keyword from the input string.
  Returns <CODE>true</CODE> if the assertion corresponding to the keyword
  has to be added to the conjunction, <CODE>false</CODE> otherwise.
*/
static bool
get_field(std::istream& s, const std::string&
#ifndef NDEBUG
	  keyword
#endif
	  ) {
  std::string str;
  s >> str;
  assert(str.length() == 1 + keyword.length());
  assert(str.substr(1) == keyword);
  assert(str[0] == yes || str[0] == no);
  return str[0] == yes;
}

/*! \relates Parma_Polyhedra_Library::Status */
std::istream&
PPL::operator>>(std::istream& s, Status& u) {
  if (get_field(s, zero_dim_univ))
    u.set_zero_dim_univ();

  if (get_field(s, empty))
    u.set_empty();

 get_field(s, consys_min) ?
    u.set_c_minimized() :
    u.reset_c_minimized();

 get_field(s, gensys_min) ?
    u.set_g_minimized() :
    u.reset_g_minimized();

  get_field(s, consys_upd) ?
    u.set_c_up_to_date() :
    u.reset_c_up_to_date();

  get_field(s, gensys_upd) ?
    u.set_g_up_to_date() :
    u.reset_g_up_to_date();

  get_field(s, sat_c) ?
    u.set_sat_c_up_to_date() :
    u.reset_sat_c_up_to_date();

  get_field(s, sat_g) ?
    u.set_sat_g_up_to_date() :
    u.reset_sat_g_up_to_date();

  return s;
}

bool
PPL::Status::OK() const {
  if (test_zero_dim_univ())
    // Zero-dim universe is ok.
    return true;

  if (test_empty()) {
    // The empty flag is incompatible with any other one.
    Status copy = *this;
    copy.reset_empty();
    return copy.test_zero_dim_univ();
  }

  if ((test_sat_c_up_to_date() || test_sat_g_up_to_date())
      && !(test_c_up_to_date() && test_g_up_to_date()))
    // If a saturation matrix is up-to-date, constraints and
    // generators have to be both up-to-date.
    return false;

  if (test_c_minimized() && !test_c_up_to_date())
    // If constraints are minimized they must be up-to-date.
    return false;

  if (test_g_minimized() && !test_g_up_to_date())
    // If generators are minimized they must be up-to-date.
    return false;

  // Any other case is ok.
  return true;
}
