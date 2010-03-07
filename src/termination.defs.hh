/* Utilities for termination analysis: declarations.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_termination_defs_hh
#define PPL_termination_defs_hh 1

#include "Generator.defs.hh"
#include "C_Polyhedron.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename PSET>
bool
termination_test_MS(const PSET& pset);

template <typename PSET>
bool
one_affine_ranking_function_MS(const PSET& pset, Generator& mu);

template <typename PSET>
void
all_affine_ranking_functions_MS(const PSET& pset, C_Polyhedron& mu_space);

template <typename PSET>
bool
termination_test_PR(const PSET& pset);

template <typename PSET>
bool
one_affine_ranking_function_PR(const PSET& pset, Generator& mu);

template <typename PSET>
void
all_affine_ranking_functions_PR(const PSET& pset, C_Polyhedron& mu_space);

} // namespace Parma_Polyhedra_Library

#include "termination.templates.hh"

#endif // !defined(PPL_termination_defs_hh)
