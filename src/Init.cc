/* Init class implementation (non-inline functions and static variables).
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

#include "Init.defs.hh"
#include "globals.hh"
#include "Variable.defs.hh"

#include "statistics.hh"
#if PPL_STATISTICS
#include <iostream>
#endif

namespace PPL = Parma_Polyhedra_Library;

unsigned int PPL::Init::count = 0;

#if PPL_STATISTICS
PPL::bhrz03_statistics* PPL::statistics = 0;
#endif

extern "C" void
set_GMP_memory_allocation_functions(void)
#if CXX_SUPPORTS_ATTRIBUTE_WEAK
  __attribute__((weak));

void
set_GMP_memory_allocation_functions(void) {
}
#else
  ;
#endif

PPL::Init::Init() {
  // Only when the first Init object is constructed...
  if (count++ == 0) {
    // ... the GMP memory allocation functions are set, ...
    set_GMP_memory_allocation_functions();
    // ... then memory is allocated for tmp_Integer, ...
    tmp_Integer = new Integer[6];
    // ... and the default output function for Variable objects is set.
    Variable::set_output_function(Variable::default_output_function);
#if PPL_STATISTICS
    statistics = new bhrz03_statistics;
    statistics->reason.zero_dim_or_empty = 0;
    statistics->reason.poly_dim = 0;
    statistics->reason.lin_space_dim = 0;
    statistics->reason.num_constraints = 0;
    statistics->reason.num_points = 0;
    statistics->reason.zero_coord_rays = 0;
    statistics->reason.equal = 0;

    statistics->technique.delay = 0;
    statistics->technique.nop = 0;
    statistics->technique.combining_constraints = 0;
    statistics->technique.evolving_points = 0;
    statistics->technique.evolving_rays = 0;
    statistics->technique.h79 = 0;
#endif //#if PPL_STATISTICS
  }
}

PPL::Init::~Init() {
  // Only when the last Init object is destroyed...
  if (--count == 0) {
    // ... tmp_Integer is also destroyed.
    delete[] tmp_Integer;
#if PPL_STATISTICS
    std::cerr << "bhrz03-reasons("
	      << "ZDE=" << statistics->reason.zero_dim_or_empty << "-"
	      << "DIM=" << statistics->reason.poly_dim << "-"
	      << "LIN=" << statistics->reason.lin_space_dim << "-"
	      << "CONS=" << statistics->reason.num_constraints << "-"
	      << "PNTS=" << statistics->reason.num_points << "-"
	      << "RAYS=" << statistics->reason.zero_coord_rays << "-"
	      << "SAME=" << statistics->reason.equal << ")" << std::endl;
    std::cerr << "bhrz03-techniques("
	      << "DELAY=" << statistics->technique.delay << "-"
	      << "NOP=" << statistics->technique.nop << "-"
	      << "AC=" << statistics->technique.combining_constraints << "-"
	      << "EP=" << statistics->technique.evolving_points << "-"
	      << "ER=" << statistics->technique.evolving_rays << "-"
	      << "H79=" << statistics->technique.h79 << ")" << std::endl;
    delete statistics;
#endif //#if PPL_STATISTICS
  }
}
