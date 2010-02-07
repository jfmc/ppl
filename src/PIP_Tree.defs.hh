/* PIP_Tree class declaration.
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

#ifndef PPL_PIP_Tree_defs_hh
#define PPL_PIP_Tree_defs_hh 1

#include "PIP_Tree.types.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.types.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Constraint.defs.hh"
#include "Matrix.defs.hh"
#include "Variables_Set.defs.hh"
#include "globals.defs.hh"
#include "PIP_Problem.defs.hh"

namespace Parma_Polyhedra_Library {

/*! \brief
  The base class for the nodes of the trees representing the solutions
  of PIP problems.
*/
class PIP_Tree_Node {
protected:
  //! Default constructor.
  PIP_Tree_Node();

  //! Copy constructor.
  PIP_Tree_Node(const PIP_Tree_Node& y);

public:
  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const = 0;

  //! Destructor.
  virtual ~PIP_Tree_Node();

  //! Returns \c true if and only if \p *this is well formed.
  virtual bool OK() const;

  //! Returns \p this if \p *this is a solution node, 0 otherwise.
  virtual const PIP_Solution_Node* as_solution() const;

  //! Returns \p this if \p *this is a solution node, 0 otherwise.
  virtual PIP_Solution_Node* as_solution();

  //! Returns \p this if \p *this is a decision node, 0 otherwise.
  virtual const PIP_Decision_Node* as_decision() const;

  //! Returns \p this if \p *this is a decision node, 0 otherwise.
  virtual PIP_Decision_Node* as_decision();

  /*! \brief
    Returns the system of parameter constraints controlling \p *this.

    The indices in the constraints are the same as the original variables and
    parameters. Coefficients in indices corresponding to variables always are
    zero.
  */
  const Constraint_System& constraints() const;

  class Artificial_Parameter;

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

protected:
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

    \return
    The number of inserted artificial parameters.

    \param params
    The Variables_Set to be updated

    \param space_dimension
    The space dimension of the simplex tableau (including artificial
    parameters).
  */
  dimension_type insert_artificials(Variables_Set& params,
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
                              const Constraint_Sequence& input_cs,
                              const Variables_Set& parameters) = 0;

  /*! \brief
    Execute a parametric simplex on the tableau, under specified context.

    \return
    The root of the PIP tree solution, or 0 if unfeasible.

    \param problem
    the containing problem object

    \param context
    the context, being a set of constraints on the parameters

    \param params
    local parameter set, including parent artificial parameters

    \param space_dimension
    space dimension of parent, including artificial parameters
  */
  virtual PIP_Tree_Node* solve(const PIP_Problem& problem,
                               const Matrix& context,
                               const Variables_Set& params,
                               dimension_type space_dimension) = 0;

  //! Inserts a new parametric constraint in internal Row format
  void add_constraint(const Row& x, const Variables_Set& parameters);

}; // class PIP_Tree_Node


/*! \brief
  Artificial parameters in PIP solution trees.

  These parameters are built from a linear expression combining other
  parameters (constant term included) divided by a positive integer
  denominator. Coefficients at variables indices corresponding to
  PIP problem variables are always zero.
*/
class PIP_Tree_Node::Artificial_Parameter
  : public Linear_Expression {
public:
  //! Default constructor: builds a zero artificial parameter.
  Artificial_Parameter();

  //! Constructor.
  /*!
    Builds artificial parameter \f$\frac{\mathit{expr}}{\mathit{den}}\f$.

    \param expr
    The expression that, after normalization, will form the numerator of
    the artificial parameter.

    \param den
    The integer constant thatm after normalization, will form the
    denominator of the artificial parameter.

    \exception std::invalid_argument
    Thrown if \p den is zero.

    Normalization will ensure that the denominator is positive.
  */
  Artificial_Parameter(const Linear_Expression& expr,
                       Coefficient_traits::const_reference den);

  //! Copy constructor.
  Artificial_Parameter(const Artificial_Parameter& y);

  //! Returns the normalized (i.e., positive) denominator.
  Coefficient_traits::const_reference get_denominator() const;

  //! Returns \b true if \p x and \p y are equal.
  friend bool operator==(const Artificial_Parameter& x,
                         const Artificial_Parameter& y);

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  //! Returns \c true if and only if the parameter is well-formed.
  bool OK() const;

private:
  //! The normalized (i.e., positive) denominator.
  Coefficient denominator;
}; // class PIP_Tree_Node::Artificial_Parameter


//! A tree node representing part of the space of solutions.
class PIP_Solution_Node : public PIP_Tree_Node {
public:
  //! Default constructor.
  PIP_Solution_Node();

  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const;

  //! Destructor.
  virtual ~PIP_Solution_Node();

  //! Returns \c true if and only if \p *this is well formed.
  virtual bool OK() const;

  //! Returns \p this.
  virtual const PIP_Solution_Node* as_solution() const;

  //! Returns \p this.
  virtual PIP_Solution_Node* as_solution();

  /*! \brief
    Returns a parametric expression of the values of variable \p v.

    The returned linear expression involves original and artificial
    parameters.

    \param v
    The variable which is queried about.

    \param parameters
    A \c std::set of indices of the parameters in the problem constraints.

    \exception std::invalid_argument
    Thrown if \p v is dimension-incompatible with \p *this
    or if \p v is a parameter.
  */
  const Linear_Expression&
  parametric_values(Variable v,
                    const Variables_Set& parameters) const;

  void ascii_dump(std::ostream& s) const;
  bool ascii_load(std::istream& s);

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
    Tableau(const Tableau& y);
    //! Destructor.
    ~Tableau();

    //! Tests whether the matrix is integer, \e ie. the denominator is 1.
    bool is_integer() const;

    //! Multiplies all coefficients and denominator with ratio.
    void scale(Coefficient_traits::const_reference ratio);

    //! Normalizes the modulo of coefficients so that they are mutually prime.
    /*!
      Computes the Greatest Common Divisor (GCD) among the elements of
      the matrices and normalizes them and the denominator by the GCD itself.
    */
    void normalize();

    /*! \brief
      Compares two pivot row and column pairs before pivoting.

      The algorithm searches the first (ie, leftmost) column \f$k\f$ in
      parameter matrix for which the \f$c=s_{*j}\frac{t_{ik}}{s_{ij}}\f$
      and \f$c'=s_{*j'}\frac{t_{i'k}}{s_{i'j'}}\f$ columns are different,
      where \f$s_{*j}\f$ denotes the \f$j\f$<sup>th</sup> column from the
      \f$s\f$ matrix and \f$s_{*j'}\f$ is the \f$j'\f$<sup>th</sup> column
      of \f$s\f$.

      \f$c\f$ is the computed column that would be subtracted to column
      \f$k\f$ in parameter matrix if pivoting is done using the \f$(i,j)\f$
      row and column pair.
      \f$c'\f$ is the computed column that would be subtracted to column
      \f$k\f$ in parameter matrix if pivoting is done using the
      \f$(i',j')\f$ row and column pair.

      The test is true if the computed \f$-c\f$ column is lexicographically
      bigger than the \f$-c'\f$ column. Due to the column ordering in the
      parameter matrix of the tableau, leftmost search will enforce solution
      increase with respect to the following priority order:
       - the constant term
       - the coefficients for the original parameters
       - the coefficients for the oldest artificial parameters.

      \return
      \c true if pivot row and column pair \f$(i,j)\f$ is more
      suitable for pivoting than the \f$(i',j')\f$ pair

      \param mapping
      the PIP_Solution_Node::mapping vector for the tableau

      \param basis
      the PIP_Solution_Node::basis vector for the tableau

      \param i
      the row number for the first pivot row and column pair to be compared

      \param j
      the column number for the first pivot row and column pair to be
      compared

      \param i_
      the row number for the second pivot row and column pair to be compared

      \param j_
      the column number for the second pivot row and column pair to be
      compared
    */
    bool is_better_pivot(const std::vector<dimension_type>& mapping,
                         const std::vector<bool>& basis,
                         const dimension_type i,
                         const dimension_type j,
                         const dimension_type i_,
                         const dimension_type j_) const;

    //! Returns the value of the denominator.
    Coefficient_traits::const_reference get_denominator() const;

    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);

    //! Returns \c true if and only if \p *this is well formed.
    bool OK() const;
  }; // struct Tableau

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

  /*! \brief
    The variable identifiers associated to the rows of the simplex tableau.
  */
  std::vector<dimension_type> var_row;

  /*! \brief
    The variable identifiers associated to the columns of the simplex tableau.
  */
  std::vector<dimension_type> var_column;

  /*! \brief
    The variable number of the special inequality used for modelling
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
    The column index in the parametric part of the simplex tableau
    corresponding to the big parameter; \c not_a_dimension() if not set.
  */
  dimension_type big_dimension;

  //! The possible values for the sign of a parametric linear expression.
  enum Row_Sign {
    //! Not computed yet (default).
    UNKNOWN,
    //! All row coefficients are zero.
    ZERO,
    //! All nonzero row coefficients are positive.
    POSITIVE,
    //! All nonzero row coefficients are negative.
    NEGATIVE,
    //! The row contains both positive and negative coefficients.
    MIXED
  };

  //! A cache for computed sign values of constraint parametric RHS.
  std::vector<Row_Sign> sign;

  //! Parametric values for the solution.
  std::vector<Linear_Expression> solution;

  //! An indicator for solution validity.
  bool solution_valid;

  //! Returns the sign of row \p x.
  static Row_Sign row_sign(const Row& x, dimension_type big_dimension);

  /*! \brief
    Checks whether a constraint is compatible with a context,
    i.e., if it does not make the context unsatisfiable.

    The method consists in applying the revised dual simplex algorithm on a
    Matrix consisting in the original matrix with the constraint inserted. If
    the simplex terminates with a feasible (optimal) solution, then the
    restrained context is not empty. Otherwise, it is.

    The algorithm ensures the feasible solutions are integer, by applying a
    cut generation method when intermediate non-integer solutions are found.
  */
  static bool compatibility_check(const Matrix& ctx, const Row& cnst);

protected:
  //! Copy constructor.
  PIP_Solution_Node(const PIP_Solution_Node& y);

  //! A tag type to select the alternative copy constructor.
  struct No_Constraints {};

  //! Alternative copy constructor.
  /*!
    This constructor differs from the default copy constructor in that
    it will not copy the constraint system, nor the artificial parameters.
  */
  PIP_Solution_Node(const PIP_Solution_Node& y, No_Constraints);

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
    A \c std::set of indices of the parameters in the constraints
  */
  void update_solution(const Variables_Set& parameters);

  /*! \brief
    Execute a parametric simplex on the tableau, under specified context.

    \return
    The root of the PIP tree solution, or 0 if unfeasible.

    \param problem
    The containing problem object.

    \param context
    The context, being a set of constraints on the parameters.

    \param params
    The local set of parameters, including parent artificial parameters.

    \param space_dimension
    Space dimension of parent, including artificial parameters.
  */
  virtual PIP_Tree_Node* solve(const PIP_Problem& problem,
                               const Matrix& context,
                               const Variables_Set& params,
                               dimension_type space_dimension);

  /*! \brief
    Generate a Gomory cut using non-integer tableau row \p i.

    \param i
    row index in simplex tableau from which the cut is generated

    \param parameters
    a std::set of the current parameter dimensions (including artificials);
    to be updated if a new artificial parameter is to be created

    \param context
    a set of linear inequalities on the parameters, in Matrix form; to be
    updated if a new artificial parameter is to be created

    \param space_dimension
    the current space dimension, including variables and all parameters; to
    be updated if an extra parameter is to be created
  */
  void generate_cut(dimension_type i,
                    Variables_Set& parameters,
                    Matrix& context,
                    dimension_type& space_dimension);

}; // class PIP_Solution_Node


//! A tree node representing a decision in the space of solutions.
class PIP_Decision_Node : public PIP_Tree_Node {
public:
  //! Returns a pointer to a dynamically-allocated copy of \p *this.
  virtual PIP_Tree_Node* clone() const;

  //! Destructor.
  virtual ~PIP_Decision_Node();

  //! Returns \c true if and only if \p *this is well formed.
  virtual bool OK() const;

  //! Returns \p this.
  virtual const PIP_Decision_Node* as_decision() const;

  //! Returns \p this.
  virtual PIP_Decision_Node* as_decision();

  //! Returns a const pointer to the \p b (true or false) branch of \p *this.
  const PIP_Tree_Node* child_node(bool b) const;

  //! Returns a pointer to the \p v (true or false) branch of \p *this.
  PIP_Tree_Node* child_node(bool v);

  void ascii_dump(std::ostream& s) const;
  bool ascii_load(std::istream& s);

private:
  // PIP_Solution_Node is allowed to use the constructor and methods.
  friend class PIP_Solution_Node;

  // PIP_Problem ascii load method needs access to private constructors.
  friend bool PIP_Problem::ascii_load(std::istream& s);

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
  PIP_Decision_Node(const PIP_Decision_Node& y);

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
    Execute a parametric simplex on the tableau, under specified context.

    \return
    The root of the PIP tree solution, or 0 if unfeasible.

    \param problem
    the containing problem object

    \param context
    the context, being a set of constraints on the parameters

    \param params
    local parameter set, including parent artificial parameters

    \param space_dimension
    space dimension of parent, including artificial parameters

  */
  virtual PIP_Tree_Node* solve(const PIP_Problem& problem,
                               const Matrix& context,
                               const Variables_Set& params,
                               dimension_type space_dimension);
};

namespace IO_Operators {

std::ostream& operator<<(std::ostream& os,
                         const PIP_Tree_Node::Artificial_Parameter& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "PIP_Tree.inlines.hh"

#endif // !defined(PPL_PIP_Tree_defs_hh)
