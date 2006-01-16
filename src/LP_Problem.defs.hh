/* LP_Problem class declaration.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_LP_Problem_defs_hh
#define PPL_LP_Problem_defs_hh 1

#include "LP_Problem.types.hh"
#include "globals.types.hh"
#include "Row.defs.hh"
#include "Matrix.defs.hh"
#include "Constraint_System.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.types.hh"
#include "Generator.defs.hh"
#include <vector>
#include <map>
#include <iosfwd>

//! A Linear Programming problem.
class Parma_Polyhedra_Library::LP_Problem {
public:
  //! Default constructor: builds a trivial LP problem.
  /*!
    The trivial LP problem requires to maximize the objective function
    \f$0\f$ on the zero-dimensional vector space under no constraints
    at all: the origin of the vector space is the optimal solution.
  */
  LP_Problem();

  /*! \brief
    Builds an LP problem from the constraint system \p cs, the objective
    function \p obj and optimization mode \p mode.

    \param cs
    The constraint system defining the feasible region for the LP problem.

    \param obj
    The objective function for the LP problem (optional argument with
    default value \f$0\f$).

    \param mode
    The optimization mode (optional argument with default value
    <CODE>MAXIMIZATION</CODE>).

    \exception std::invalid_argument
    Thrown if the constraint system contains any strict inequality
    or if the space dimension of the objective function is strictly
    greater than the space dimension of the constraint system.
  */
  explicit LP_Problem(const Constraint_System& cs,
		      const Linear_Expression& obj = Linear_Expression::zero(),
		      Optimization_Mode mode = MAXIMIZATION);

  //! Ordinary copy-constructor.
  LP_Problem(const LP_Problem& y);

  //! Destructor.
  ~LP_Problem();

  //! Assignment operator.
  LP_Problem& operator=(const LP_Problem& y);

  //! Returns the maximum space dimension a LP_Problem can handle.
  static dimension_type max_space_dimension();

  //! Returns the space dimension of the current LP problem.
  dimension_type space_dimension() const;

  //! Returns the constraints defining the current feasible region.
  const Constraint_System& constraints() const;

  //! Returns the current objective function.
  const Linear_Expression& objective_function() const;

  //! Returns the current optimization mode.
  Optimization_Mode optimization_mode() const;

  //! Resets \p *this to be equal to the trivial LP problem.
  void clear();

  /*! \brief
    Adds a copy of constraint \p c to the current LP problem,
    increasing the number of space dimensions if needed.

    \exception std::invalid_argument
    Thrown if the constraint \p c is a strict inequality.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of the constraints in \p cs to the current LP problem,
    increasing the number of space dimensions if needed.

    \exception std::invalid_argument
    Thrown if the constraint system \p cs contains any strict inequality.
  */
  void add_constraints(const Constraint_System& cs);

  //! Sets the objective function to \p obj.
  /*!
    \exception std::invalid_argument
    Thrown if the space dimension of \p obj is strictly greater than
    the space dimension of \p *this.
  */
  void set_objective_function(const Linear_Expression& obj);

  //! Sets the optimization mode to \p mode.
  void set_optimization_mode(Optimization_Mode mode);

  //! Checks satisfiability of \p *this.
  /*!
    \return
    <CODE>true</CODE> if and only if the LP problem is satisfiable.
  */
  bool is_satisfiable() const;

  //! Optimizes the current LP problem using the primal simplex algorithm.
  /*!
    \return
    An LP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible, unbounded or optimized problem).
  */
  LP_Problem_Status solve() const;

  /*! \brief
    Sets \p num and \p den so that \f$\frac{num}{den}\f$ is the result
    of evaluating the objective function on \p evaluating_point.

    \param evaluating_point
    The point on which the objective function will be evaluated.

    \param num
    On exit will contain the numerator of the evaluated value.

    \param den
    On exit will contain the denominator of the evaluated value.

    \exception std::invalid_argument
    Thrown if \p *this and \p evaluating_point are dimension-incompatible
    or if the generator \p evaluating_point is not a point.
  */
  void evaluate_objective_function(const Generator& evaluating_point,
				   Coefficient& num,
				   Coefficient& den) const;

  //! Returns a feasible point for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if the LP problem is not satisfiable.
  */
  const Generator& feasible_point() const;

  //! Returns an optimal point for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the LP problem is unbounded or not satisfiable.
  */
  const Generator& optimizing_point() const;

  /*! \brief
    Sets \p num and \p den so that \f$\frac{num}{den}\f$ is
    the solution of the optimization problem.

    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the LP problem is unbounded or not satisfiable.
  */
  void optimal_value(Coefficient& num, Coefficient& den) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  PPL_OUTPUT_DECLARATIONS;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    \ref ascii_dump) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Swaps \p *this with \p y.
  void swap(LP_Problem& y);

private:
  //! The matrix encoding the current feasible region in tableau form.
  Matrix tableau;
  //! The working cost function.
  Row working_cost;
  //! The current basic solution.
  std::vector<dimension_type> base;
  //! A mapping between original variables and split ones.
  /*!
    Contains all the pairs (i, j) such that Variable(i) (that was not found
    to be constrained in sign) has been split into two nonnegative variables.
    The "positive" one is represented again by Variable(i), and
    the "negative" one is represented by Variable(j).
  */
  std::map<dimension_type, dimension_type> dim_map;

  //! An enumerated type describing the internal status of the LP problem.
  enum Status {
    //! The LP problem has not been solved yet.
    UNSOLVED,
    //! The LP problem is unsatisfiable.
    UNSATISFIABLE,
    //! The LP problem is satisfiable; a feasible solution has been computed.
    SATISFIABLE,
    //! The LP problem is unbounded; a feasible solution has been computed.
    UNBOUNDED,
    //! The LP problem is optimized; an optimal solution has been computed.
    OPTIMIZED,
    /*! \brief
      The feasible region of the LP problem has been changed by adding
      new constraints; a feasible solution for the old constraints has
      been computed.
    */
    PARTIALLY_SATISFIABLE
  };

  //! The internal state of the LP problem.
  Status status;

  //! The constraint system describing the feasible region.
  Constraint_System input_cs;

  //! The objective function to be optimized.
  Linear_Expression input_obj_function;

  //! The optimization mode requested.
  Optimization_Mode opt_mode;

  //! The last successfully computed feasible or optimizing point.
  Generator last_generator;

  /*! \brief
    Optimizes the current LP problem using the second phase of the
    primal simplex algorithm.
  */
  void second_phase();

  /*! \brief
    Assigns to \p this->tableau a simplex tableau representing the
    current LP problem, inserting into \p this->dim_map the information
    that is required to recover the original LP problem.

    \return
    <CODE>UNFEASIBLE_LP_PROBLEM</CODE> if the constraint system contains
    any trivially unfeasible constraint (tableau was not computed);
    <CODE>UNBOUNDED_LP_PROBLEM</CODE> if the problem is trivially unbounded
    (the computed tableau contains no constraints);
    <CODE>OPTIMIZED_LP_PROBLEM></CODE> if the problem is neither trivially
    unfeasible nor trivially unbounded (the tableau was computed successfully).
  */
  LP_Problem_Status compute_tableau();

  /*! \brief
    Checks for optimality and, if it does not hold, computes the column
    index of the variable entering the base of the LP problem.
    Implemented with anti-cycling rule.

    \return
    The column index of the variable that enters the base. If no such
    variable exists, optimality was achieved and <CODE>0</CODE> is retuned.
  */
  dimension_type get_entering_var_index() const;

  /*! \brief
    Computes the row index of the variable exiting the base
    of the LP problem. Implemented with anti-cycling rules.

    \return
    The row index of the variable exiting the base.

    \param entering_var_index
    The column index of the variable entering the base.
  */
  dimension_type
  get_exiting_base_index(dimension_type entering_var_index) const;

  //! Linearly combines \p x with \p y so that <CODE>*this[k]</CODE> is 0.
  /*!
    \param x
    The Row that will be combined with \p y object.

    \param y
    The Row that will be combined with \p x object.

    \param k
    The position of \p *this that have to be \f$0\f$.

    Computes a linear combination of \p x and \p y having
    the element of index \p k equal to \f$0\f$. Then it assigns
    the resulting Linear_Row to \p x and normalizes it.
  */
  static void linear_combine(Row& x, const Row& y, const dimension_type k);

  /*! \brief
    Swaps two variables in base during the simplex algorithm,
    performing the needed linear combinations.

    \param entering_var_index
    The index of the variable entering the base.

    \param exiting_base_index
    The index of the row exiting the base.
  */
  void swap_base(const dimension_type entering_var_index,
		 const dimension_type exiting_base_index);

  /*! \brief
    Checks for optimality and, if it does not hold, computes the column
    index of the variable entering the base of the LP problem.

    \return
    The column index of the variable that enters the base. If no such
    variable exists, optimality was achieved and <CODE>0</CODE> is retuned.

    To compute the entering_index, the steepest edge algorithm chooses
    the index `j' such that \f$\frac{d_{j}}{\|\Delta x^{j} \|}\f$ is the
    largest in absolute value, where
    \f[
      \|\Delta x^{j} \|
        = \left(
            1+\sum_{i=1}^{m} \alpha_{ij}^2
          \right)^{\frac{1}{2}}.
    \f]
    Recall that, due to the Integer implementation of the algorithm, our
    tableau doesn't contain the ``real'' \f$\alpha\f$ values, but these
    can be computed dividing the value of the cofficient by the value of
    the variable in base. Obviously the result may not be an Integer, so
    we will proceed in another way: the following code will compute the
    lcm of all the variables in base to get the good ``weight'' of each
    Coefficient of the tableau.
  */
  dimension_type steepest_edge() const;

  /*! \brief
    Returns <CODE>true</CODE> if and if only the algorithm successfully
    computed a feasible solution.
  */
  bool compute_simplex();

  /*! \brief
    Adds the slack variables to satisfy the standard form of a LP problem,
    inserts the "sign" to the cost functions, and makes the
    necessary swaps to express the problem with the 1st phase base.
  */
  void prepare_first_phase();

  /*! \brief
    Drop unnecessary slack variables from the tableau and get ready
    for the second phase of the simplex algorithm.
  */
  void erase_slacks();

  bool is_in_base(const dimension_type var_index,
		  dimension_type& row_index) const;

  Generator compute_generator() const;
};

#include "LP_Problem.inlines.hh"

#endif // !defined(PPL_LP_Problem_defs_hh)
