/* Checked_Number class implementation
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "Checked_Number.defs.hh"

namespace Parma_Polyhedra_Library {

void
Checked_Number_Default_Policy::bad_result(Result r) {
  switch (r) {
  case V_NEG_OVERFLOW:
    throw std::overflow_error("Negative overflow.");
  case V_UNKNOWN_NEG_OVERFLOW:
    throw std::overflow_error("Unknown result due to negative overflow.");
  case V_POS_OVERFLOW:
    throw std::overflow_error("Positive overflow.");
  case V_UNKNOWN_POS_OVERFLOW:
    throw std::overflow_error("Unknown result due to positive overflow.");
  case V_CVT_STR_UNK:
    throw std::domain_error("Invalid numeric string.");
  case V_DIV_ZERO:
    throw std::domain_error("Division by zero.");
  case V_MOD_ZERO:
    throw std::domain_error("Modulo by zero.");
  case V_SQRT_NEG:
    throw std::domain_error("Square root of negative number.");
  case V_LT:
  case V_LE:
  case V_GT:
  case V_GE:
  case V_NE:
  case V_LGE:
    throw std::logic_error("Unexpected inexact computation.");
    break;
  default:
    throw std::logic_error("Unexpected result.");
    break;
  }
}

} // namespace Parma_Polyhedra_Library

