/* Definitions of global objects.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "globals.hh"
#include "Constraint.defs.hh"
#include "Generator.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::Integer* PPL::tmp_Integer;

const PPL::Throwable* volatile PPL::abandon_exponential_computations = 0;


/*! \relates Parma_Polyhedra_Library::Row */
const PPL::Integer&
PPL::operator*(const Constraint& x, const Generator& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size());
  tmp_Integer[0] = 0;
  for (dimension_type i = x.size(); i-- > 0; ) {
    // The following two lines optimize the computation
    // of tmp_Integer[0] += x[i] * y[i].
    tmp_Integer[1] = x[i] * y[i];
    tmp_Integer[0] += tmp_Integer[1];
  }
  return tmp_Integer[0];
}


/*! \relates Parma_Polyhedra_Library::Row */
const PPL::Integer&
PPL::reduced_scalar_product(const Constraint& x, const Generator& y) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  assert(!x.is_necessarily_closed());
  assert(x.size() - 1 <= y.size());
  tmp_Integer[0] = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; ) {
    // The following two lines optimize the computation
    // of tmp_Integer[0] += x[i] * y[i].
    tmp_Integer[1] = x[i] * y[i];
    tmp_Integer[0] += tmp_Integer[1];
  }
  return tmp_Integer[0];
}
