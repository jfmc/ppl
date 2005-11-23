/* Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_globals_types_hh
#define PPL_globals_types_hh 1

#include <cstddef>

namespace Parma_Polyhedra_Library {

//! An unsigned integral type for representing space dimensions.
typedef size_t dimension_type;

//! An unsigned integral type for representing memory size in bytes.
typedef size_t memory_size_type;

//! Kinds of degenerate abstract elements.
enum Degenerate_Element {
  //! The universe element, i.e., the whole vector space.
  UNIVERSE,
  //! The empty element, i.e., the empty set.
  EMPTY
};

//! Relation symbols.
enum Relation_Symbol {
  //! Less than.
  LESS_THAN,
  //! Less than or equal to.
  LESS_THAN_OR_EQUAL,
  //! Equal to.
  EQUAL,
  //! Greater than or equal to.
  GREATER_THAN_OR_EQUAL,
  //! Greater than.
  GREATER_THAN
};

//! Complexity pseudo-classes.
enum Complexity_Class {
  //! Worst-case polynomial complexity.
  POLYNOMIAL_COMPLEXITY,
  //! Worst-case exponential complexity but typically polynomial behavior.
  SIMPLEX_COMPLEXITY,
  //! Any complexity.
  ANY_COMPLEXITY
};

//! Possible optimization kinds.
enum Optimization_Kind {
  //! Minimization is requested.
  MINIMIZATION,
  //! Maximiation is requested.
  MAXIMIZATION
};

//! Possible outcomes of a simplex solver.
enum Simplex_Status {
  //! The problem is unfeasible.
  UNFEASIBLE_PROBLEM,
  //! The problem is unbounded.
  UNBOUNDED_PROBLEM,
  //! The problem has been solved.
  SOLVED_PROBLEM
};

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_globals_types_hh)
