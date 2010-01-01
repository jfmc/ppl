/* PIP_Problem class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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
   - the subset of those dimensions of the vector space that are
     interpreted as integer parameters (the other space dimensions
     are interpreted as non-parameter integer variables);
   - the feasible region, by means of a finite set of linear equality
     and (strict or non-strict) inequality constraints involving both
     variables and parameters;
   - optionally, an initial context restricting the definition domain for
     the parameters, by means of a finite set of linear equality and
     (strict or non-strict) inequality constraints involving only
     parameters;
   - optionally, the dimension of a parameter to be considered arbitrarily
     big; such a parameter is called the &ldquo;big parameter&rdquo;.

  All variables and parameters are considered non-negative integers.

  The class provides support for the (incremental) solution of the
  PIP problem based on variations of the revised simplex method and
  on Gomory cut generation techniques.

  The solution for a PIP problem is the lexicographic minimum of the
  integer points of the feasible region, expressed in terms of the
  parameters. As the problem to be solved only involves non-negative
  variables and parameters, the problem will always be either unfeasible
  or optimizable.

  As the feasibility and the solution value of a PIP problem depend on the
  values of the parameters, the solution is a binary decision tree,
  dividing the context parameter set into subsets. The tree nodes are:
   - decision nodes, encoding one or more linear tests on the parameters;
     if all the tests are true, then the solution is the node's
     &ldquo;true&rdquo; child, otherwise the solution is the node's
     &ldquo;false&rdquo; child;
   - solution nodes, encoding the solution of the problem in the current
     context subset, where each variable is defined in terms of a linear
     expression of the parameters. Solution nodes also embed an optionally
     not empty set of parameter constraints, meaning if all constraint tests
     are true, the solution is described by the node, otherwise the problem
     is empty.

  Concerning the decision nodes, it may happen that a decision node has no
  &ldquo;false&rdquo; or &ldquo;true&rdquo; child. This means the
  corresponding test leads to an unfeasible solution. In addition, decision
  nodes with two or more linear tests cannot have a &ldquo;false&rdquo;
  child.

  Both tree node types may also contain the definition of extra parameters
  which are artificially introduced by the solver to keep the solution
  values integer. Such artificial parameters are defined by the integer
  division of a linear expression on the parameters by an integer
  coefficient.

  By exploiting the incremental nature of the solver, it is possible
  to reuse part of the computational work already done when solving
  variants of a given PIP_Problem: currently, incremental resolution
  supports the addition of space dimensions, the addition of constraints,
  the change of objective function and the change of optimization mode.

  \par Example problem
  An example PIP problem can be defined the following:
  \code
  3*j >= -2*i+8
  j <= 4*i - 4
  i <= n
  j <= m
  \endcode
  where \c i and \c j are variables, and \c n and \c m are parameters.
  The resulting solution tree is:
  \code
  if 7*n >= 10 then
    if 7*m >= 12 then
      {i = 2 ; j = 2}
    else
      New Parameter P = (m) div 2
      if 2*n + 3*m >= 8 then
        {i = -m - P + 4 ; j = m}
      else
        _|_
  else
    _|_
  \endcode
  The \f$\perp\f$ notation, also called &ldquo;bottom&rdquo;, denotes
  the lexicographic minimum of an empty set, here meaning the problem is
  infeasible.\n
  Also notice that a new parameter is defined after the first \c else. It
  is only valid inside the block where it is defined, and is used when
  formulating the value of the <tt>{i,j}</tt> vector.

  \par Context restriction
  The above solution is correct in an unrestricted original context,
  meaning all possible values are allowed for the parameters. If we
  restrict the context with the following parameter inequalities:
  \code
  m >= n
  n >= 5
  \endcode
  then the result will simply be:
  \code
  {i = 2 ; j = 2}
  \endcode

  \par Problem object creation
  The PIP_Problem object correspondind to the above example problem can be
  created like follows:
  \code
  Variable i(0);
  Variable j(1);
  Variable n(2);
  Variable m(3);
  Variables_Set params(n, m);
  Constraint_System cs;
  cs.insert(3*j >= -2*i+8);
  cs.insert(j <= 4*i - 4);
  cs.insert(j <= m);
  cs.insert(i <= n);
  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);
  \endcode
  If you want to restrict the original context, simply add the parameter
  constraints the same way as for normal constraints.
  \code
  cs.insert(m >= n);
  cs.insert(n >= 5);
  \endcode

  \par Problem solving
  Once a PIP_Problem object has been created, you can start the
  resolution of the problem by calling the solve() method:
  \code
  PIP_Problem_Status status;
  status = pip.solve();
  \endcode
  where the returned \c status indicates if the problem has been optimized
  or if it is unfeasible for any possible configuration of the parameter
  values.

  \par Solution tree spanning
  Retrieve the optimized solution decision tree:
  \code
  const PIP_Tree_Node* node = solution();
  \endcode
  If the pointer designates a \f$\perp\f$ solution (infeasible), its value
  is \c 0.\n
  To check whether a non-null tree node pointer designates a solution or a
  decision node, you can call the PIP_Tree_Node::as_decision() and
  PIP_Tree_Node::as_solution() virtual methods:
  \code
  const PIP_Solution_Node* sol = node->as_solution();
  if (sol != 0) {
    // The node is a solution node
    ...
  }
  else {
    // The node is a decision node
    const PIP_Decision_Node* dec = node->as_decision();
    ...
  }
  \endcode

  \par
  To access the two child nodes of a Decision Node, use the
  PIP_Tree_Node::child_node(bool) method, using \b true or \b false as an
  argument to access the child node you want.

  \par Artificial parameters
  The expression of an artificial parameter is stored in an
  PIP_Tree_Node::Artificial_Parameter object, which encodes the integer
  division of a Linear_Expression (only expressed in terms of the other
  parameters, including the previously-defined artificials) by a
  denominator Coefficient.
  To get the effective value of an artificial parameter, just divide the
  result of the Linear_Expression applied to the effective values of the
  other parameters by the value of the denominator.
  The dimensions of the artificial parameters in a node can be computed as
  <tt>dim</tt>, <tt>dim+1</tt>, ... where <tt>dim</tt>'s value is computed
  the following way:
   - for the root node \c dim is the maximum space dimension of the
     original problem, plus one.
   - for any other node of the tree, it is the sum of the \c dim value
     computed for the parent node, plus the total number of artificial
     parameters in the parent node.
  \par
  The definition of artificial parameters' dimension values always follow
  this rule. If you choose to add a new variable or parameter on a problem
  which already has a solution (incremental solving), all artificial
  parameters in the solution will have their dimension values incremented.

  \par Node constraints
  All node types can embed parameter-only constraints. Decision nodes
  contain at least one of them. You can access the node's constraints set
  using the PIP_Tree_Node::constraints() method.
  The signification of the node constraints is the following:
   - On a decision node, if all tests in the constraints are true, then the
     solution is the &ldquo;true&rdquo; child; otherwise it is the
     &ldquo;false&rdquo; child. If the number of constraints is greater than
     one, the &ldquo;false&rdquo; child is always infeasible.
   - On a solution node, if the constraint set is empty or all tests in the
     constraints are true, then the solution is described by the node;
     otherwise the solution is \f$\perp\f$ (infeasible problem).
  \par
  Node constraints always are defined in terms of the parameters, including
  those from the original problem and any of the artificial parameters
  defined in nodes which belong to the path from the root to the concerned
  node.

  \par Getting the optimal values for the variables
  Once having spanned the solution tree using the actual values of the
  parameters, and having ended at a solution node, you can fetch the
  parametric expression of any of the variables using the
  PIP_Solution_Node::parametric_values() method.
  \par
  Variable expressions always are defined in terms of the parameters,
  including those from the original problem and any of the artificial
  parameters defined in nodes which belong to the path from the root to the
  concerned node.

  FIXME: different uses of the big parameter.
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
    Thrown if the space dimension of a constraint in the sequence
    (resp., the parameter variables) is strictly greater than \p dim.
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
    Thrown if the space dimension of \p c is strictly greater than
    the space dimension of \p *this.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of the constraints in \p cs to the PIP problem.

    \exception std::invalid_argument
    Thrown if the space dimension of constraint system \p cs is strictly
    greater than the space dimension of \p *this.
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
    A PIP_Problem_Status flag indicating the outcome of the optimization
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
    Thrown if \p *this does not have an optimizing point, i.e.,
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

  //! Possible names for PIP_Problem control parameters.
  enum Control_Parameter_Name {
    //! Cutting strategy
    CUTTING_STRATEGY,

    //! Number of different enumeration values.
    CONTROL_PARAMETER_NAME_SIZE
  };

  //! Possible values for PIP_Problem control parameters.
  enum Control_Parameter_Value {
    //! Choose the first non-integer row
    CUTTING_STRATEGY_FIRST,
    //! Choose row which generates the deepest cut
    CUTTING_STRATEGY_DEEPEST,

    //! Number of different enumeration values.
    CONTROL_PARAMETER_VALUE_SIZE
  };

  //! Returns the value of control parameter \p name.
  Control_Parameter_Value
  get_control_parameter(Control_Parameter_Name name) const;

  //! Sets control parameter \p value.
  void set_control_parameter(Control_Parameter_Value value);

  //! Sets the dimension for the big parameter
  void set_big_parameter_dimension(dimension_type x);

  /*! \brief
    Returns the space dimension for the big parameter.

    If a big parameter was not set, returns \c not_a_dimension().
  */
  dimension_type get_big_parameter_dimension() const;

private:
  //! Initializes the control parameters with default values.
  void control_parameters_init();

  //! Copies the control parameters from problem object \p y.
  void control_parameters_copy(const PIP_Problem& y);

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

  //! The control parameters for the problem object.
  Control_Parameter_Value
  control_parameters[CONTROL_PARAMETER_NAME_SIZE];

  /*! \brief
    The dimension for the big parameter, or \c not_a_dimension()
    if not set.
  */
  dimension_type big_parameter_dimension;
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
