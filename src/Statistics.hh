/* Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Statistics_hh
#define PPL_Statistics_hh 1

#ifndef PPL_STATISTICS
#define PPL_STATISTICS 0
#endif

#if PPL_STATISTICS
namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A few statistics gathered for optimization and/or debugging purposes.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
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

extern bhrz03_statistics *statistics;

}
#endif // PPL_STATISTICS

#endif //!defined PPL_Statistics_hh

