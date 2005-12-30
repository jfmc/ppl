/* Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_LP_Problem_types_hh
#define PPL_LP_Problem_types_hh 1

namespace Parma_Polyhedra_Library {

//! Possible outcomes of the LP_Problem solver.
enum LP_Problem_Status {
  //! The problem is unfeasible.
  UNFEASIBLE_LP_PROBLEM,
  //! The problem is unbounded.
  UNBOUNDED_LP_PROBLEM,
  //! The problem has an optimal solution.
  OPTIMIZED_LP_PROBLEM
};

class LP_Problem;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_LP_Problem_types_hh)
