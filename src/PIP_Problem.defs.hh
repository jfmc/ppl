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
#include "Constraint.defs.hh"
#include "Constraint_System.types.hh"
#include "Generator.defs.hh"
#include "Variables_Set.defs.hh"
#include "PIP_Tree.defs.hh"
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
   - the subset of those dimensions of the vector space that are
     interpreted as integer parameters (the other space dimensions
     are interpreted as non-parameter integer variables);

   // FIXME: should the non-negativity constraints be assumed?
   // If that is the case, the problem will always be either
   // unfeasible or optimizable (since the origin of the vector
   // space is the minimum for the unconstrained non-negative problem).

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
  friend class PIP_Solution_Node;
public:
  //! Builds a trivial PIP problem.
  /*!
    A trivial PIP problem requires to minimize the objective function
    \f$0\f$ on a vector space under no constraints and with no parameters:
    the origin of the vector space is an optimal solution.

    \param dim
    The dimension of the vector space enclosing \p *this
    (optional argument with default value \f$0\f$).

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.
  */
  explicit PIP_Problem(dimension_type dim = 0);

  /*! \brief
    Builds a PIP problem having space dimension \p dim
    from the sequence of constraints in the range
    \f$[\mathrm{first}, \mathrm{last})\f$;
    those dimensions whose indices occur in \p p_vars are
    interpreted as parameters.

    \param dim
    The dimension of the vector space (variables and parameters) enclosing
    \p *this.

    \param first
    An input iterator to the start of the sequence of constraints.

    \param last
    A past-the-end input iterator to the sequence of constraints.

    \param p_vars
    The set of variables' indexes that are interpreted as parameters.

    \exception std::length_error
    Thrown if \p dim exceeds <CODE>max_space_dimension()</CODE>.

    \exception std::invalid_argument
    Thrown if a constraint in the sequence is a strict inequality or
    if the space dimension of a constraint (resp., the parameter
    variables) is strictly greater than \p dim.
  */
  template <typename In>
  PIP_Problem(dimension_type dim,
	      In first, In last,
	      const Variables_Set& p_vars);

  //! Ordinary copy-constructor.
  PIP_Problem(const PIP_Problem& y);

  //! Destructor.
  ~PIP_Problem();

  //! Assignment operator.
  PIP_Problem& operator=(const PIP_Problem& y);

  //! Returns the maximum space dimension a PIP_Problem can handle.
  static dimension_type max_space_dimension();

  //! Returns the space dimension of the PIP problem.
  dimension_type space_dimension() const;

  /*! \brief
    Returns a set containing all the variables' indexes representing
    the parameters of the PIP problem.
  */
  const Variables_Set& parameter_space_dimensions() const;

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

  //! Resets \p *this to be equal to the trivial PIP problem.
  /*!
    The space dimension is reset to \f$0\f$.
  */
  void clear();

  /*! \brief
    Adds <CODE>m_pip_vars + m_pip_params</CODE> new space dimensions
    and embeds the old PIP problem in the new vector space.

    \param m_pip_vars
    The number of space dimensions to add that are interpreted as
    PIP problem variables (i.e., non parameters). These are added
    \e before adding the \p m_pip_params parameters.

    \param m_pip_params
    The number of space dimensions to add that are interpreted as
    PIP problem parameters. These are added \e after having added the
    \p m_pip_vars problem variables.

    \exception std::length_error
    Thrown if adding <CODE>m_pip_vars + m_pip_params</CODE> new space
    dimensions would cause the vector space to exceed dimension
    <CODE>max_space_dimension()</CODE>.

    The new space dimensions will be those having the highest indexes
    in the new PIP problem; they are initially unconstrained.
  */
  void add_space_dimensions_and_embed(dimension_type m_pip_vars,
                                      dimension_type m_pip_params);

  /*! \brief
    Sets the space dimensions whose indexes which are in set \p p_vars to be
    parameter space dimensions.

    \exception std::invalid_argument
    Thrown if some index in \p p_vars does not correspond to
    a space dimension in \p *this.
  */
  void add_to_parameter_space_dimensions(const Variables_Set& p_vars);

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
    attempt (unfeasible or optimized problem).
  */
  PIP_Problem_Status solve() const;

  //! Returns a feasible solution for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if the PIP problem is not satisfiable.
  */
  PIP_Tree solution() const;

  //! Returns an optimizing solution for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the PIP problem is unbounded or not satisfiable.
  */
  PIP_Tree optimizing_solution() const;

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

private:
  //! The dimension of the vector space.
  dimension_type external_space_dim;

  /*! \brief
    The space dimension of the current (partial) solution of the
    PIP problem; it may be smaller than \p external_space_dim.
  */
  dimension_type internal_space_dim;

  //! An enumerated type describing the internal status of the PIP problem.
  enum Status {
    //! The PIP problem is unsatisfiable.
    UNSATISFIABLE,
    //! The PIP problem is satisfiable; a feasible solution has been computed.
    SATISFIABLE,
    //! The PIP problem is optimized; the solution tree has been computed.
    OPTIMIZED,
    /*! \brief
      The feasible region of the PIP problem has been changed by adding
      new variables, parameters or constraints; a feasible solution for
      the old feasible region is still available.
    */
    PARTIALLY_SATISFIABLE
  };

  //! The internal state of the MIP problem.
  Status status;

  //! The current solution decision tree
  PIP_Tree_Node* current_solution;

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

  /*! \brief
    A set containing all the indices of space dimensions that are
    interpreted as problem parameters.
  */
  Variables_Set parameters;

  /*! \brief
    The initial context

    Contains problem constraints on parameters only
  */
  Matrix initial_context;
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
