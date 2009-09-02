/* PIP_Problem class implementation: non-inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "PIP_Problem.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

/*! \relates Parma_Polyhedra_Library::PIP_Problem */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const PIP_Problem& /*p*/) {
 return s;
}

void
PPL::PIP_Problem::Rational_Matrix::normalize() {
  //FIXME
}

PPL::PIP_Problem::PIP_Problem(dimension_type dim)
  : external_space_dim(dim),
    internal_space_dim(0),
    tableau(),
    status(PARTIALLY_SATISFIABLE),
    initialized(false),
    input_cs(),
    first_pending_constraint(0),
    parameters() {
  // Check for space dimension overflow.
  if (dim > max_space_dimension())
    throw std::length_error("PPL::PIP_Problem:: PIP_Problem(dim, first, "
                            "last, p_vars):\n"
                            "dim exceeds the maximum allowed"
                            "space dimension.");
  PPL_ASSERT(OK());
}

PPL::PIP_Problem::PIP_Problem(const PIP_Problem &y)
  : external_space_dim(y.external_space_dim),
    internal_space_dim(y.internal_space_dim),
    tableau(y.tableau),
    status(y.status),
    initialized(y.initialized),
    input_cs(y.input_cs),
    first_pending_constraint(y.first_pending_constraint),
    parameters(y.parameters) {
  PPL_ASSERT(OK());
}

PPL::PIP_Problem_Status
PPL::PIP_Problem::solve() const {
  //FIXME
  return OPTIMIZED_PIP_PROBLEM;
}


bool
PPL::PIP_Problem::OK() const {
  //FIXME
  return true;
}
