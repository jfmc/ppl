/* PIP_Problem class declaration.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_PIP_Problem_defs_hh
#define PPL_PIP_Problem_defs_hh 1

#include "PIP_Problem.types.hh"
#include "globals.types.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.types.hh"
#include "Constraint_System.types.hh"
#include "Generator.defs.hh"
#include "Variables_Set.defs.hh"
#include <vector>
#include <deque>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::PIP_Problem */
std::ostream&
operator<<(std::ostream& s, const PIP_Problem& p);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A Parametric Integer (linear) Programming problem.
/*! \ingroup PPL_CXX_interface
  An object of this class encodes a parametric integer (linear)
  programming problem.
  The PIP problem is specified by providing:
   - the dimension of the vector space;
   - the feasible region, by means of a finite set of linear equality
     and non-strict inequality constraints;
   - the subset of the unknown variables that range over the integers
     (the other variables implicitly ranging over the reals);
   - the objective function, described by a Linear_Expression;
   - the optimization mode (either maximization or minimization).

  The class provides support for the (incremental) solution of the
  PIP problem based on variations of the revised simplex method and
  on branch-and-bound techniques. The result of the resolution
  process is expressed in terms of an enumeration, encoding the
  feasibility and the unboundedness of the optimization problem.
  The class supports simple feasibility tests (i.e., no optimization),
  as well as the extraction of an optimal (resp., feasible) point,
  provided the PIP_Problem is optimizable (resp., feasible).

  By exploiting the incremental nature of the solver, it is possible
  to reuse part of the computational work already done when solving
  variants of a given PIP_Problem: currently, incremental resolution
  supports the addition of space dimensions, the addition of constraints,
  the change of objective function and the change of optimization mode.
*/
class Parma_Polyhedra_Library::PIP_Problem {
public:
  //! Builds a trivial PIP problem.
  /*!
    A trivial PIP problem requires to maximize the objective function
    \f$0\f$ on a vector space under no constraints at all:
    the origin of the vector space is an optimal solution.

    \param dim
    The dimension of the vector space enclosing \p *this
    (optional argument with default value \f$0\f$).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.
  */
  explicit PIP_Problem(dimension_type dim = 0);

  /*! \brief
    Builds an PIP problem having space dimension \p dim
    from the sequence of constraints in the range
    \f$[\mathrm{first}, \mathrm{last})\f$,
    the objective function \p obj and optimization mode \p mode;
    those dimensions whose indices occur in \p int_vars are
    constrained to take an integer value.

    \param dim
    The dimension of the vector space enclosing \p *this.

    \param first
    An input iterator to the start of the sequence of constraints.

    \param last
    A past-the-end input iterator to the sequence of constraints.

    \param int_vars
    The set of variables' indexes that are constrained to take integer values.

    \param obj
    The objective function (optional argument with default value \f$0\f$).

    \param mode
    The optimization mode (optional argument with default value
    <CODE>MAXIMIZATION</CODE>).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.

    \exception std::invalid_argument
    Thrown if a constraint in the sequence is a strict inequality,
    if the space dimension of a constraint (resp., of the
    objective function or of the integer variables) or the space dimension
    of the integer variable set is strictly greater than \p dim.
  */
  template <typename In>
  PIP_Problem(dimension_type dim,
	      In first, In last,
	      const Variables_Set& int_vars,
	      const Linear_Expression& obj = Linear_Expression::zero(),
	      Optimization_Mode mode = MAXIMIZATION);

  /*! \brief
    Builds an PIP problem having space dimension \p dim
    from the sequence of constraints in the range
    \f$[\mathrm{first}, \mathrm{last})\f$,
    the objective function \p obj and optimization mode \p mode.

    \param dim
    The dimension of the vector space enclosing \p *this.

    \param first
    An input iterator to the start of the sequence of constraints.

    \param last
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
    objective function or of the integer variables) is strictly
    greater than \p dim.
  */
  template <typename In>
  PIP_Problem(dimension_type dim,
              In first, In last,
              const Linear_Expression& obj = Linear_Expression::zero(),
              Optimization_Mode mode = MAXIMIZATION);

  /*! \brief
    Builds an PIP problem having space dimension \p dim from the constraint
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
  PIP_Problem(dimension_type dim,
	      const Constraint_System& cs,
	      const Linear_Expression& obj = Linear_Expression::zero(),
	      Optimization_Mode mode = MAXIMIZATION);

  //! Ordinary copy-constructor.
  PIP_Problem(const PIP_Problem& y);

  //! Destructor.
  ~PIP_Problem();

  //! Assignment operator.
  PIP_Problem& operator=(const PIP_Problem& y);

  //! Returns the maximum space dimension an PIP_Problem can handle.
  static dimension_type max_space_dimension();

  //! Returns the space dimension of the PIP problem.
  dimension_type space_dimension() const;

  /*! \brief
    Returns a set containing all the variables' indexes constrained
    to be integral.
  */
  const Variables_Set& integer_space_dimensions() const;

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

  //! Resets \p *this to be equal to the trivial PIP problem.
  /*!
    The space dimension is reset to \f$0\f$.
  */
  void clear();

  /*! \brief
    Adds \p m new space dimensions and embeds the old PIP problem
    in the new vector space.

    \param m
    The number of dimensions to add.

    \exception std::length_error
    Thrown if adding \p m new space dimensions would cause the
    vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new PIP problem; they are initially unconstrained.
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Sets the variables whose indexes are in set \p i_vars to be
    integer space dimensions.

    \exception std::invalid_argument
    Thrown if some index in \p i_vars does not correspond to
    a space dimension in \p *this.
  */
  void add_to_integer_space_dimensions(const Variables_Set& i_vars);

  /*! \brief
    Adds a copy of constraint \p c to the PIP problem.

    \exception std::invalid_argument
    Thrown if the constraint \p c is a strict inequality or if its space
    dimension is strictly greater than the space dimension of \p *this.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of the constraints in \p cs to the PIP problem.

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
    <CODE>true</CODE> if and only if the PIP problem is satisfiable.
  */
  bool is_satisfiable() const;

  //! Optimizes the PIP problem.
  /*!
    \return
    An PIP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible, unbounded or optimized problem).
  */
  PIP_Problem_Status solve() const;

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
    Thrown if the PIP problem is not satisfiable.
  */
  const Generator& feasible_point() const;

  //! Returns an optimal point for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the PIP problem is unbounded or not satisfiable.
  */
  const Generator& optimizing_point() const;

  /*! \brief
    Sets \p num and \p den so that \f$\frac{num}{den}\f$ is
    the solution of the optimization problem.

    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the PIP problem is unbounded or not satisfiable.
  */
  void optimal_value(Coefficient& num, Coefficient& den) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Swaps \p *this with \p y.
  void swap(PIP_Problem& y);

  //! Names of PIP problems' control parameters.
  enum Control_Parameter_Name {
    //! The pricing rule.
    PRICING
  };

  //! Possible values for PIP problem's control parameters.
  enum Control_Parameter_Value {
    //! Steepest edge pricing method, using floating points (default).
    PRICING_STEEPEST_EDGE_FLOAT,
    //! Steepest edge pricing method, using Coefficient.
    PRICING_STEEPEST_EDGE_EXACT,
    //! Textbook pricing method.
    PRICING_TEXTBOOK
  };

  //! Returns the value of the control parameter \p name.
  Control_Parameter_Value
  get_control_parameter(Control_Parameter_Name name) const;

  //! Sets control parameter \p value.
  void set_control_parameter(Control_Parameter_Value value);
};

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::PIP_Problem */
void swap(Parma_Polyhedra_Library::PIP_Problem& x,
	  Parma_Polyhedra_Library::PIP_Problem& y);

} // namespace std

#include "PIP_Problem.inlines.hh"
#include "PIP_Problem.templates.hh"

#endif // !defined(PPL_PIP_Problem_defs_hh)
