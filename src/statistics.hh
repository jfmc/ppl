/* Declarations of objects used for statistics.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_statistics_hh
#define PPL_statistics_hh 1

#ifndef PPL_STATISTICS
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_defines
  \brief
  Enables the gathering of some statistical data.

  Currently, when set, will collect a few information about
  the usage of the heuristic techniques adopted in the
  \ref BHRZ03_widening "BHRZ03-widening" operator.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define PPL_STATISTICS 0
#endif

#if PPL_STATISTICS
namespace Parma_Polyhedra_Library {

//! A few statistics gathered for optimization and/or debugging purposes.
struct bhrz03_statistics {
  struct bhrz03_reason {
    int zero_dim_or_empty;
    int poly_dim;
    int lin_space_dim;
    int num_constraints;
    int num_points;
    int zero_coord_rays;
    int equal;
  } reason;
  struct bhrz03_technique {
    int delay;
    int nop;
    int combining_constraints;
    int evolving_points;
    int evolving_rays;
    int h79;
  } technique;
};

extern bhrz03_statistics* statistics;

} // namespace Parma_Polyhedra_Library
#endif // PPL_STATISTICS

#endif // !defined(PPL_statistics_hh)
