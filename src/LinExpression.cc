/* LinExpression class implementation (non-inline functions).
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "LinExpression.defs.hh"

namespace Parma_Polyhedra_Library {

LinExpression
operator +(const LinExpression& e1, const LinExpression& e2) {
  size_t e1_size = e1.size();
  size_t e2_size = e2.size();
  size_t min_size;
  size_t max_size;
  const LinExpression* p_e_max;
  if (e1_size > e2_size) {
    min_size = e2_size;
    max_size = e1_size;
    p_e_max = &e1;
  }
  else {
    min_size = e1_size;
    max_size = e2_size;
    p_e_max = &e2;
  }

  LinExpression r(max_size, false);
  size_t i = max_size;
  while (i > min_size) {
    --i;
    r[i] = (*p_e_max)[i];
  }
  while (i > 0) {
    --i;
    r[i] = e1[i] + e2[i];
  }

  return r;
}


LinExpression
operator +(const Integer& n, const LinExpression& e) {
  LinExpression r(e);
  r[0] += n;
  return r;
}


LinExpression
operator -(const LinExpression& e) {
  LinExpression r(e);
  for (size_t i = e.size(); i-- > 0; )
    r[i].negate();
  return r;
}


LinExpression
operator -(const LinExpression& e1, const LinExpression& e2) {
  size_t e1_size = e1.size();
  size_t e2_size = e2.size();
  if (e1_size > e2_size) {
    LinExpression r(e1_size, false);
    size_t i = e1_size;
    while (i > e2_size) {
      --i;
      r[i] = e1[i];
    }
    while (i > 0) {
      --i;
      r[i] = e1[i] - e2[i];
    }
    return r;
  }
  else {
    LinExpression r(e2_size, false);
    size_t i = e2_size;
    while (i > e1_size) {
      --i;
      r[i] = -e2[i];
    }
    while (i > 0) {
      --i;
      r[i] = e1[i] - e2[i];
    }
    return r;
  }
}


LinExpression
operator -(const Integer& n, const LinExpression& e) {
  LinExpression r(e);
  for (size_t i = e.size(); i-- > 0; )
    r[i].negate();
  r[0] += n;

  return r;
}


LinExpression
operator *(const Integer& n, const LinExpression& e) {
  LinExpression r(e);
  for (size_t i = e.size(); i-- > 0; )
    r[i] *= n;
  return r;
}


LinExpression&
operator +=(LinExpression& e1, const LinExpression& e2) {
  size_t e1_size = e1.size();
  size_t e2_size = e2.size();
  if (e1_size >= e2_size)
    for (size_t i = e2_size; i-- > 0; )
      e1[i] += e2[i];
  else {
    LinExpression e(e2);
    for (size_t i = e1_size; i-- > 0; )
      e[i] += e1[i];
    std::swap(e1, e);
  }
  return e1;
}


LinExpression&
operator +=(LinExpression& e, const Variable& v) {
  size_t e_size = e.size();
  size_t vpos = v.id() + 1;
  if (e_size <= vpos) {
    LinExpression new_e(e, vpos+1);
    std::swap(e, new_e);
  }
  ++e[vpos];
  return e;
}

} // namespace Parma_Polyhedra_Library
