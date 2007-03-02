/* Box class implementation (non-inline functions).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>
#include "Box.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \relates Parma_Polyhedra_Library::BD_Shape */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
bool
PPL::extract_interval_constraint(const Constraint& c,
				 const dimension_type c_space_dim,
				 dimension_type& c_num_vars,
				 dimension_type& c_only_var,
				 Coefficient& c_coeff) {
  // Check for preconditions.
  assert(c.space_dimension() == c_space_dim);
  assert(c_num_vars == 0 && c_only_var == 0);
  // Collect the non-zero components of `c'.
  for (dimension_type i = c_space_dim; i-- > 0; )
    if (c.coefficient(Variable(i)) != 0)
      if (c_num_vars == 0) {
	c_only_var = i+1;
	++c_num_vars;
      }
      else
	// Constraint `c' is not an interval constraint.
	return false;

  if (c_num_vars != 0)
    c_coeff = -c.coefficient(Variable(c_only_var-1));

  return true;
}
