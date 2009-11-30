/* PIP_Tree class declaration.
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

#ifndef PPL_PIP_Tree_defs_hh
#define PPL_PIP_Tree_defs_hh 1

#include "PIP_Tree.types.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.types.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Constraint.defs.hh"
#include "PIP_Problem.types.hh"
#include "Matrix.defs.hh"
#include "Variables_Set.defs.hh"
#include "globals.defs.hh"

namespace Parma_Polyhedra_Library {

/*! \brief
  The base class for the nodes of the trees representing the solutions
  of PIP problems.
*/
class PIP_Tree_Node {
public:
  //! Returns \p this if \p *this is a solution node, 0 otherwise.
  virtual const PIP_Solution_Node* as_solution() const;

  //! Returns \p this if \p *this is a solution node, 0 otherwise.
  virtual PIP_Solution_Node* as_solution();

  //! Returns \p this if \p *this is a decision node, 0 otherwise.
  virtual const PIP_Decision_Node* as_decision() const;

  //! Returns \p this if \p *this is a decision node, 0 otherwise.
  virtual PIP_Decision_Node* as_decision();

  //! Destructor.
  virtual ~PIP_Tree_Node();

  //! Returns \c true if and only if \p *this is well formed.
  bool OK() const;

  /*! \brief
    Returns the system of parameter constraints controlling \p *this.

    The indices in the constraints are the same as the original variables and
    parameters. Coefficients in indices corresponding to variables always are
    zero.
  */
  const Constraint_System& constraints() const;

  /*! \brief
    A class to store the expession of artificial parameters in solution trees.

    These locally new parameters are of the form of the integer division of a
    linear expression of the other parameters (constant term included), by a
    coefficient. Coefficients at indexes corresponding to variables always are
    zero.
  */
  class Artificial_Parameter : public Linear_Expression {
  public:
    Artificial_Parameter();
    Artificial_Parameter(const Linear_Expression &e, const Coefficient &d);
    Artificial_Parameter(const Artificial_Parameter &x);

    const Coefficient& get_denominator() const;

    //! Returns \b true if \p x and \p y are equal.
    friend bool operator==(const Artificial_Parameter& x,
                           const Artificial_Parameter& y);

    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);

  private:
    Coefficient denominator;
  };

  //! A type alias for a sequence of Artificial_Parameter's.
  typedef std::vector<Artificial_Parameter> Artificial_Parameter_Sequence;

  //! Returns a const_iterator to the beginning of local artificial parameters.
  Artificial_Parameter_Sequence::const_iterator art_parameter_begin() const;

  //! Returns a const_iterator to the end of local artificial parameters.
  Artificial_Parameter_Sequence::const_iterator art_parameter_end() const;

  //! Returns the number of local artificial parameters.
  dimension_type art_parameter_count() const;

  void ascii_dump(std::ostream& s) const;
  bool ascii_load(std::istream& s);

  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const = 0;

protected:
  //! Default constructor.
  PIP_Tree_Node();

  //! Copy constructor.
  PIP_Tree_Node(const PIP_Tree_Node &x);

  //! A type alias for a sequence of constraints.
  typedef std::vector<Constraint> Constraint_Sequence;

  // Only PIP_Problem and PIP_Decision_Node are allowed to use the
  // constructor and methods.
  friend class PIP_Problem;
  friend class PIP_Decision_Node;
  friend class PIP_Solution_Node;

  //! A pointer to \p *this 's parent, or 0 if \p *this is the root node.
  const PIP_Decision_Node* parent_;

  //! The local system of parameter constraints.
  Constraint_System constraints_;

  //! The local sequence of expressions for local artificial parameters.
  Artificial_Parameter_Sequence artificial_parameters;

  //! Set this node's parent to \p *p.
  void set_parent(const PIP_Decision_Node* p);

  //! Returns a pointer to this node's parent.
  const PIP_Decision_Node* parent() const;

  /*! \brief
    Inserts in parameter set \p params the parameter indices corresponding
    to all artificials parameters involved in this node (from the solution
    tree root to the node included).

    This utility method can typically be used by user programs when spanning
    a solution tree. As new parameters may be defined in tree nodes by the
    solver, local solutions are likely to be expressed in terms of both the
    upper level parameters and the local ones.

    \param params
    the Variables_Set to be updated

    \param space_dimension
    the space dimension of the simplex tableau (including artificial
    parameters)

    \return
    the number of inserted artificial parameters.
  */
  dimension_type insert_artificials(Variables_Set &params,
                                    dimension_type space_dimension) const;

  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param problem
    the containing problem object

    \param external_space_dim
    size of constraints (variables and parameters added)

    \param first_pending_constraint
    first element in \p input_cs to be added to the tableau, which already
    contains the previous elements

    \param input_cs
    all the constraints of the problem

    \param parameters
    a \c std::set of indices of the parameters in the constraints
  */
  virtual void update_tableau(const PIP_Problem& problem,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence &input_cs,
                              const Variables_Set &parameters) = 0;

  /*! \brief
    Execute a parametric simplex on the tableau, under specified context.

    \param parent_ref
    a pointer to the parent reference to \p this

    \param problem
    the containing problem object

    \param context
    the context, being a set of constraints on the parameters

    \param params
    local parameter set, including parent artificial parameters

    \param space_dimension
    space dimension of parent, including artificial parameters

    \return
    An PIP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible or optimized problem).
  */
  virtual PIP_Problem_Status solve(PIP_Tree_Node*& parent_ref,
                                   const PIP_Problem& problem,
                                   const Matrix& context,
                                   const Variables_Set& params,
                                   dimension_type space_dimension) = 0;

  //! Inserts a new parametric constraint in internal Row format
  void add_constraint(const Row &x, const Variables_Set& parameters);
};

//! A tree node representing part of the space of solutions.
class PIP_Solution_Node : public PIP_Tree_Node {
public:
  //! Default constructor.
  PIP_Solution_Node();

  //! Destructor.
  ~PIP_Solution_Node();

  //! Returns \p this.
  virtual const PIP_Solution_Node* as_solution() const;

  //! Returns \p this.
  virtual PIP_Solution_Node* as_solution();

  /*! \brief
    Returns a parametric expression of the values of variable \p v.

    The returned linear expression involves original and artificial
    parameters.

    \param v
    the variable which is queried about

    \param parameters
    a \c std::set of indices of the parameters in the problem constraints

    \exception std::invalid_argument
    Thrown if \p v is dimension-incompatible with \p *this
    or if \p v is a parameter.
  */
  const Linear_Expression&
  parametric_values(const Variable &v,
                    const Variables_Set& parameters) const;

  void ascii_dump(std::ostream& s) const;
  bool ascii_load(std::istream& s);

  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const;

  bool OK() const;

private:
  //! The type for parametric simplex tableau.
  struct Tableau {
    //! The matrix of simplex coefficients.
    Matrix s;
    //! The matrix of parameter coefficients.
    Matrix t;
    //! A common denominator for all matrix elements
    Coefficient denominator;

    //! Default constructor.
    Tableau();
    //! Copy constructor.
    Tableau(const Tableau& x);
    //! Destructor.
    ~Tableau();

    //! Returns the allocated capacity of each Row of the \p s Matrix.
    dimension_type s_capacity() const;
    //! Returns the allocated capacity of each Row of the \p t Matrix.
    dimension_type t_capacity() const;
    //! Tests whether the matrix is integer, \e ie. the denominator is 1.
    bool is_integer() const;
    //! Multiplies all coefficients and denominator with ratio.
    void scale(const Coefficient &ratio);
    //! Normalizes the modulo of coefficients so that they are mutually prime.
    /*!
      Computes the Greatest Common Divisor (GCD) among the elements of
      the matrices and normalizes them and the denominator by the GCD itself.
    */
    void normalize();
    //! Returns the value of the denominator.
    const Coefficient &get_denominator() const;

    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);

    //! Returns \c true if and only if \p *this is well formed.
    bool OK() const;
  };

  //! The parametric simplex tableau.
  Tableau tableau;

  /*! \brief
    A boolean vector for identifying the basic variables.

    Variable identifiers are numbered from 0 to <tt>n+m-1</tt>, where \p n
    is the number of columns in the simplex tableau corresponding to variables,
    and \p m is the number of rows.

    Indices from 0 to <tt>n-1</tt> correspond to the original variables.

    Indices from \p n to <tt>n+m-1</tt> correspond to the slack variables
    associated to the internal constraints, which do not strictly correspond
    to original constraints, since these may have been transformed to fit the
    standard form of the dual simplex.

    The value for <tt>basis[i]</tt> is:
     - \b true if variable \p i is basic,
     - \b false if variable \p i is nonbasic.
  */
  std::vector<bool> basis;

  /*! \brief
    A mapping between the tableau rows/columns and the original variables.

    The value of <tt>mapping[i]</tt> depends of the value of <tt>basis[i]</tt>.

     - If <tt>basis[i]</tt> is \b true, <tt>mapping[i]</tt> encodes the column
       index of variable \p i in the \p s matrix of the tableau.
     - If <tt>basis[i]</tt> is \b false, <tt>mapping[i]</tt> encodes the row
       index of variable \p i in the tableau.
  */
  std::vector<dimension_type> mapping;

  /*! \brief A vector of the variable identifiers associated to each row of
     the simplex tableau. */
  std::vector<dimension_type> var_row;

  /*! \brief A vector of the variable identifiers associated to each column
     of the simplex tableau. */
  std::vector<dimension_type> var_column;

  /*! \brief The variable number of the special inequality used for modelling
    equality constraints.

    The subset of equality constraints in a specific problem can be expressed
    as: \f$f_i(x,p) = 0 ; 1 \leq i \leq n\f$. As the dual simplex standard form
    requires constraints to be inequalities, the following constraints can be
    modelized the following way:

     - \f$f_i(x,p) \geq 0 ; 1 \leq i \leq n\f$

     - \f$\sum\limits_{i=1}^n f_i(x,p) \leq 0\f$

    The \p special_equality_row value stores the variable number of the
    specific constraint which is used to modelize the latter sum of
    constraints. If no such constraint exists, the value is set to \p 0.
  */
  dimension_type special_equality_row;

  /*! \brief
    The column number in the parametric part of the simplex tableau
    which corresponds to the big parameter; \c not_a_dimension()
    if not set.
  */
  dimension_type big_dimension;

  //! The possible values for the sign of a parametric linear expression.
  enum Row_Sign {
    //! Not computed yet (default)
    UNKNOWN,
    //! All row coefficients are zero.
    ZERO,
    //! All nonzero row coefficients are positive.
    POSITIVE,
    //! All nonzero row coefficients are negative.
    NEGATIVE,
    //! The row contains positive and negative coefficients.
    MIXED
  };

  //! A cache for computed sign values of constraint parametric RHS.
  std::vector<Row_Sign> sign;

  //! Parametric values for the solution.
  std::vector<Linear_Expression> solution;

  //! An indicator for solution validity.
  bool solution_valid;

  //! Determines the sign of given Row.
  static Row_Sign row_sign(const Row &x, dimension_type big_dimension);

  /*! \brief
    Checks whether a constraint is compatible with a context, ie. does not
    make the context empty.

    The method consists in applying the revised dual simplex algorithm on a
    Matrix consisting in the original matrix with the constraint inserted. If
    the simplex terminates with a feasible (optimal) solution, then the
    restrained context is not empty. Otherwise, it is.

    The algorithm ensures the feasible solutions are integer, by applying a
    cut generation method when intermediate non-integer solutions are found.
  */
  static bool compatibility_check(const Matrix &ctx, const Row &cnst);

protected:
  //! Copy constructor.
  PIP_Solution_Node(const PIP_Solution_Node &x);

  /*! \brief
    Copy constructor, allowing not to copy the constraint system and
    artificial parameters
  */
  PIP_Solution_Node(const PIP_Solution_Node &x, bool empty_constraints);

  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param problem
    the containing problem object

    \param external_space_dim
    size of constraints (variables and parameters added)

    \param first_pending_constraint
    first element in \p input_cs to be added to the tableau, which already
    contains the previous elements

    \param input_cs
    all the constraints of the problem

    \param parameters
    a \c std::set of indices of the parameters in the constraints
  */
  virtual void update_tableau(const PIP_Problem& problem,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence& input_cs,
                              const Variables_Set& parameters);

  /*! \brief
    Update the solution values.

    \param parameters
    a \c std::set of indices of the parameters in the constraints
  */
  virtual void update_solution(const Variables_Set& parameters);

  /*! \brief
    Execute a parametric simplex on the tableau, under specified context.

    \param parent_ref
    a pointer to the parent reference to \p this

    \param problem
    the containing problem object

    \param context
    the context, being a set of constraints on the parameters

    \param params
    local parameter set, including parent artificial parameters

    \param space_dimension
    space dimension of parent, including artificial parameters

    \return
    An PIP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible or optimized problem).
  */
  virtual PIP_Problem_Status solve(PIP_Tree_Node*& parent_ref,
                                   const PIP_Problem& problem,
                                   const Matrix& context,
                                   const Variables_Set& params,
                                   dimension_type space_dimension);
  // FIXME: constructors to be decided.
};

//! A tree node representing a decision in the space of solutions.
class PIP_Decision_Node : public PIP_Tree_Node {
public:
  //! Destructor.
  ~PIP_Decision_Node();

  //! Returns \p this.
  const PIP_Decision_Node* as_decision() const;

  //! Returns \p this.
  PIP_Decision_Node* as_decision();

  //! Returns a const pointer to the \p b (true or false) branch of \p *this.
  const PIP_Tree_Node* child_node(bool b) const;

  //! Returns a pointer to the \p v (true or false) branch of \p *this.
  PIP_Tree_Node* child_node(bool v);

  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const;

  bool OK() const;

private:
  // only PIP_Solution_Node is allowed to use the constructor and methods.
  friend class PIP_Solution_Node;

  //! Pointer to the "true" child of \p *this.
  PIP_Tree_Node* true_child;

  //! Pointer to the "false" child of \p *this.
  PIP_Tree_Node* false_child;

  /*! \brief
    Constructs "if constraints then \p tcp else \p fcp". Initial constraint
    set is empty.

    Constructs a decision node,
    with "true" child \p tcp and "false" child \p fcp.

    \param fcp
    Pointer to "false" child

    \param tcp
    Pointer to "true" child

    \exception std::invalid_argument
    Thrown if \p cs contains strict inequalities.
  */
  PIP_Decision_Node(PIP_Tree_Node* fcp, PIP_Tree_Node* tcp);

protected:
  //! Copy constructor.
  PIP_Decision_Node(const PIP_Decision_Node &x);

  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param problem
    the containing problem object

    \param external_space_dim
    size of constraints (variables and parameters added)

    \param first_pending_constraint
    first element in \p input_cs to be added to the tableau, which already
    contains the previous elements

    \param input_cs
    all the constraints of the problem

    \param parameters
    a \c std::set of indices of the parameters in the constraints
  */
  virtual void update_tableau(const PIP_Problem& problem,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence &input_cs,
                              const Variables_Set &parameters);

  /*! \brief
    Execute a parametric simplex on the tableau, under specified context.

    \param parent_ref
    a pointer to the parent reference to \p this

    \param problem
    the containing problem object

    \param context
    the context, being a set of constraints on the parameters

    \param params
    local parameter set, including parent artificial parameters

    \param space_dimension
    space dimension of parent, including artificial parameters

    \return
    An PIP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible or optimized problem).
  */
  virtual PIP_Problem_Status solve(PIP_Tree_Node*& parent_ref,
                                   const PIP_Problem& problem,
                                   const Matrix& context,
                                   const Variables_Set& params,
                                   dimension_type space_dimension);
};

typedef const PIP_Tree_Node* PIP_Tree;

namespace IO_Operators {

std::ostream& operator<<(std::ostream& os,
                         const PIP_Tree_Node::Artificial_Parameter& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "PIP_Tree.inlines.hh"

#endif // !defined(PPL_PIP_Tree_defs_hh)
