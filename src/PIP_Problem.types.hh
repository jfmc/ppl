/* Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_PIP_Problem_types_hh
#define PPL_PIP_Problem_types_hh 1

#include <vector>

namespace Parma_Polyhedra_Library {

//! Possible outcomes of the PIP_Problem solver.
/*! \ingroup PPL_CXX_interface */
enum PIP_Problem_Status {
  //! The problem is unfeasible.
  UNFEASIBLE_PIP_PROBLEM,
  //! The problem has an optimal solution.
  OPTIMIZED_PIP_PROBLEM
};

//! Possible name values for PIP_Problem control parameters.
/*! \ingroup PPL_CXX_interface */
enum PIP_Problem_Control_Parameter_Name {
  //! Cutting strategy
  PIP_CUTTING_STRATEGY,

  /*! Number of different possible values of
     PIP_Problem_Control_Parameter_Name enumeration. */
  PIP_PROBLEM_CONTROL_PARAMETER_NAME_SIZE
};

//! Possible values for PIP_Problem control parameters.
/*! \ingroup PPL_CXX_interface */
enum PIP_Problem_Control_Parameter_Value {
  //! Choose the first non-integer row
  PIP_CUTTING_STRATEGY_FIRST,
  //! Choose row which generates the deepest cut
  PIP_CUTTING_STRATEGY_DEEPEST,

  /*! Number of different possible values of
     PIP_Problem_Control_Parameter_Value enumeration. */
  PIP_PROBLEM_CONTROL_PARAMETER_VALUE_SIZE
};

class PIP_Problem;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_PIP_Problem_types_hh)
