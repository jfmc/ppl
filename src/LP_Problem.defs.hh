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
#include "Linear_Expression.defs.hh"
#include "Constraint.types.hh"
#include "Constraint_System.types.hh"
#include "Generator.defs.hh"
#include <vector>
#include <deque>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::LP_Problem */
std::ostream&
operator<<(std::ostream& s, const LP_Problem& lp);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A Linear Programming problem.
/*! \ingroup PPL_CXX_interface
  An object of this class encodes a Linear Programming problem.
  The LP problem is specified by providing:
   - the dimension of the vector space;
   - the feasible region, by means of a finite set of linear equality
     and non-strict inequality constraints;
   - the objective function, described by a Linear_Expression;
   - the optimization mode (either maximization or minimization).

  The class provides support for the (incremental) solution of the
  LP problem based on the simplex method. The result of the resolution
  process is expressed in terms of an enumeration, encoding the
  feasibility and the unboundedness of the optimization problem.
  The class supports simple feasibility tests (i.e., no optimization),
  as well as the extraction of an optimal (resp., feasible) point,
  provided the LP_Problem is optimizable (resp., feasible).

  By exploiting the incremental nature of the solver, it is possible
  to reuse part of the computational work already done when solving
  variants of a given LP_Problem: currently, incremental resolution
  supports the addition of space dimensions, the addition of constraints,
  the change of objective function and the change of optimization mode.
*/
class Parma_Polyhedra_Library::LP_Problem {
public:
  //! Builds a trivial LP problem.
  /*!
    A trivial LP problem requires to maximize the objective function
    \f$0\f$ on a vector space under no constraints at all:
    the origin of the vector space is an optimal solution.

    \param dim
    The dimension of the vector space enclosing \p *this
    (optional argument with default value \f$0\f$).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.
  */
  explicit LP_Problem(dimension_type dim = 0);

  /*! \brief
    Builds an LP problem having space dimension \p dim
    from the sequence of constraints in the range
    \f$[\mathrm{first\_constraint}, \mathrm{last\_constraint})\f$,
    the objective function \p obj and optimization mode \p mode.

    \param dim
    The dimension of the vector space enclosing \p *this.

    \param first_constraint
    An input iterator to the start of the sequence of constraints.

    \param last_constraint
    A past-the-end input iterator to the sequence of constraints.

    \param obj
    The objective function (optional argument with default value \f$0\f$).

    \param mode
    The optimization mode (optional argument with default value
    <CODE>MAXIMIZATION</CODE>).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.

    \exception std::invalid_argument
    Thrown if a constraint in the sequence is a strict inequality
    or if the space dimension of a constraint (resp., of the
    objective function) is strictly greater than \p dim.
  */
  template <typename In>
  LP_Problem(dimension_type dim,
	     In first_constraint, In last_constraint,
	     const Linear_Expression& obj = Linear_Expression::zero(),
	     Optimization_Mode mode = MAXIMIZATION);

  /*! \brief
    Builds an LP problem having space dimension \p dim from the constraint
    system \p cs, the objective function \p obj and optimization mode \p mode.

    \param dim
    The dimension of the vector space enclosing \p *this.

    \param cs
    The constraint system defining the feasible region.

    \param obj
    The objective function (optional argument with default value \f$0\f$).

    \param mode
    The optimization mode (optional argument with default value
    <CODE>MAXIMIZATION</CODE>).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.

    \exception std::invalid_argument
    Thrown if the constraint system contains any strict inequality
    or if the space dimension of the constraint system (resp., the
    objective function) is strictly greater than \p dim.
  */
  LP_Problem(dimension_type dim,
	     const Constraint_System& cs,
	     const Linear_Expression& obj = Linear_Expression::zero(),
	     Optimization_Mode mode = MAXIMIZATION);

  //! Ordinary copy-constructor.
  LP_Problem(const LP_Problem& y);

  //! Destructor.
  ~LP_Problem();

  //! Assignment operator.
  LP_Problem& operator=(const LP_Problem& y);

  //! Returns the maximum space dimension an LP_Problem can handle.
  static dimension_type max_space_dimension();

  //! Returns the space dimension of the LP problem.
  dimension_type space_dimension() const;

private:
  //! A type alias for a sequence of constraints.
  typedef std::vector<Constraint> Constraint_Sequence;

public:
  /*! \brief
    A type alias for the read-only iterator on the constraints
    defining the feasible region.
  */
  typedef Constraint_Sequence::const_iterator const_iterator;

  /*! \brief
    Returns a read-only iterator to the first constraint defining
    the feasible region.
  */
  const_iterator constraints_begin() const;

  /*! \brief
    Returns a past-the-end read-only iterator to the sequence of
    constraints defining the feasible region.
  */
  const_iterator constraints_end() const;

  //! Returns the objective function.
  const Linear_Expression& objective_function() const;

  //! Returns the optimization mode.
  Optimization_Mode optimization_mode() const;

  //! Resets \p *this to be equal to the trivial LP problem.
  /*!
    The space dimension is reset to \f$0\f$.
  */
  void clear();

  /*! \brief
    Adds \p m new space dimensions and embeds the old LP problem
    in the new vector space.

    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new LP problem; they are initially unconstrained.
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds a copy of constraint \p c to the LP problem.

    \exception std::invalid_argument
    Thrown if the constraint \p c is a strict inequality or if its space
    dimension is strictly greater than the space dimension of \p *this.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of the constraints in \p cs to the LP problem.

    \exception std::invalid_argument
    Thrown if the constraint system \p cs contains any strict inequality
    or if its space dimension is strictly greater than the space dimension
    of \p *this.
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

  //! Optimizes the LP problem.
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

  PPL_OUTPUT_DECLARATIONS

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
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
  //! The dimension of the vector space.
  dimension_type external_space_dim;

  /*! \brief
    The space dimension of the current (partial) solution of the
    LP problem; it may be smaller than \p external_space_dim.
  */
  dimension_type internal_space_dim;

  //! The matrix encoding the current feasible region in tableau form.
  Matrix tableau;

  //! The working cost function.
  Row working_cost;

  //! A map between the variables of `input_cs' and `tableau'.
  /*!
    Contains all the pairs (i, j) such that mapping[i].first encodes the index
    of the column in the tableau where input_cs[i] is stored; mapping[i].second
    not a zero, encodes the split part of the tableau of input_cs[i].
    The "positive" one is represented by mapping[i].first and
    the "negative" one is represented by mapping[i].second.
  */
  std::vector<std::pair<dimension_type, dimension_type> > mapping;

  //! The current basic solution.
  std::vector<dimension_type> base;

  //! An enumerated type describing the internal status of the LP problem.
  enum Status {
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
      new space dimensions or new constraints; a feasible solution for
      the old feasible region is still available.
    */
    PARTIALLY_SATISFIABLE
  };

  //! The internal state of the LP problem.
  Status status;

  /*! \brief
    A Boolean encoding whether or not internal data structures have
    already been properly sized and populated: useful to allow for
    deeper checks in method OK().
  */
  bool initialized;

  //! The sequence of constraints describing the feasible region.
  Constraint_Sequence input_cs;

  //! The first index of `input_cs' containing a pending constraint.
  dimension_type first_pending_constraint;

  //! The objective function to be optimized.
  Linear_Expression input_obj_function;

  //! The optimization mode requested.
  Optimization_Mode opt_mode;

  //! The last successfully computed feasible or optimizing point.
  Generator last_generator;

  //! Processes the pending constraints of \p *this.
  /*!
    \return
    <CODE>true</CODE> if and only if the LP problem is satisfiable after
    processing the pending constraints, <CODE>false</CODE> otherwise.
  */
  bool process_pending_constraints();

  /*! \brief
    Optimizes the LP problem using the second phase of the
    primal simplex algorithm.
  */
  void second_phase();

  /*! \brief
    Assigns to \p this->tableau a simplex tableau representing the
    LP problem, inserting into \p this->mapping the information
    that is required to recover the original LP problem.

    \return
    <CODE>UNFEASIBLE_LP_PROBLEM</CODE> if the constraint system contains
    any trivially unfeasible constraint (tableau was not computed);
    <CODE>UNBOUNDED_LP_PROBLEM</CODE> if the problem is trivially unbounded
    (the computed tableau contains no constraints);
    <CODE>OPTIMIZED_LP_PROBLEM></CODE> if the problem is neither trivially
    unfeasible nor trivially unbounded (the tableau was computed successfully).
  */
  LP_Problem_Status compute_tableau(std::vector<dimension_type>&
				    worked_out_row);

  /*! \brief
    Parses the pending constraints to gather information on
    how to resize the tableau.

    \return
    <CODE>UNSATISFIABLE</CODE> if is detected a trivially false constraint,
    <CODE>SATISFIABLE</CODE> otherwise.

    \param new_num_rows
    This will store the number of rows that has to be added to the original
    tableau.

    \param num_slack_variables
    This will store the number of slack variables that has to be added to
    the original tableau.

    \param is_tableau_constraint
    Every element of this vector will be set to <CODE>true</CODE> if the
    associated pending constraint has to be inserted in the tableau,
    <CODE>false</CODE> otherwise.

    \param nonnegative_variable
    This will encode for each variable if this one was splitted or not.
    Every element of this vector will be set to <CODE>true</CODE> if the
    associated variable is splitted, <CODE>false</CODE> otherwise.

    \param unfeasible_tableau_rows
    This will contain all the row indexes of the tableau that are no more
    satisfied after adding more constraints to \p *this.

    \param satisfied_ineqs
    This will contain all the row indexes of the tableau that are already
    satisfied by `last_generator' and do not require artificial variables to
    have a starting feasible base.

  */
  bool parse_constraints(dimension_type& new_num_rows,
			 dimension_type& num_slack_variables,
			 std::deque<bool>& is_tableau_constraint,
			 std::deque<bool>& nonnegative_variable,
			 std::vector<dimension_type>& unfeasible_tableau_rows,
			 std::deque<bool>& satisfied_ineqs);
  /*! \brief
    Checks for optimality and, if it does not hold, computes the column
    index of the variable entering the base of the LP problem.
    Implemented with anti-cycling rule.

    \return
    The column index of the variable that enters the base. If no such
    variable exists, optimality was achieved and <CODE>0</CODE> is returned.
  */
  dimension_type textbook_entering_index() const;

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
    Performs the pivoting operation on the tableau.

    \param entering_var_index
    The index of the variable entering the base.

    \param exiting_base_index
    The index of the row exiting the base.
  */
  void pivot(const dimension_type entering_var_index,
	     const dimension_type exiting_base_index);

  /*! \brief
    Checks for optimality and, if it does not hold, computes the column
    index of the variable entering the base of the LP problem.

    \return
    The column index of the variable that enters the base. If no such
    variable exists, optimality was achieved and <CODE>0</CODE> is returned.

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
    can be computed dividing the value of the coefficient by the value of
    the variable in base. Obviously the result may not be an Integer, so
    we will proceed in another way: the following code will compute the
    lcm of all the variables in base to get the good ``weight'' of each
    Coefficient of the tableau.
  */
  dimension_type steepest_edge_entering_index() const;

  /*! \brief
    Returns <CODE>true</CODE> if and if only the algorithm successfully
    computed a feasible solution.
  */
  bool compute_simplex();

  /*! \brief
    Drop unnecessary artificial variables from the tableau and get ready
    for the second phase of the simplex algorithm.
  */
  void erase_artificials(const dimension_type begin_artificials,
			 const dimension_type end_artificials);

  bool is_in_base(const dimension_type var_index,
		  dimension_type& row_index) const;

  /*! \brief
    Computes a valid generator that satisfies all the constraints of the
    Linear Programming problem associated to \p *this.
  */
  void compute_generator() const;

  /*! \brief
    Merge split variables in the tableau if a nonnegativity constraint is
    detected.

    \param var_index
    The index of the variable that has to be unsplit.

    \param nonfeasible_cs
    This will contain all the row indexes that are no more satisfied by
    the computed generator after unsplitting a variable.
  */
  void merge_split_variables(dimension_type var_index,
			     std::vector<dimension_type>& nonfeasible_cs);

  /*! \brief
    Returns <CODE>true</CODE> if and only if constraint \p c is
    satisfied by \p last_generator.
  */
  bool is_satisfied(const Constraint& c) const;
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::LP_Problem */
void swap(Parma_Polyhedra_Library::LP_Problem& x,
	  Parma_Polyhedra_Library::LP_Problem& y);

} // namespace std

#include "LP_Problem.inlines.hh"
#include "LP_Problem.templates.hh"

#endif // !defined(PPL_LP_Problem_defs_hh)
