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
#include "Constraint_System.types.hh"
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

protected:
  //! A type alias for a sequence of constraints.
  typedef std::vector<Constraint> Constraint_Sequence;

  // Only PIP_Problem and PIP_Decision_Node are allowed to use the
  // constructor and methods.
  friend class PIP_Problem;
  friend class PIP_Decision_Node;

  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param parent_ref
    a pointer to the parent reference to \p this

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
  virtual void update_tableau(PIP_Tree_Node **parent_ref,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence &input_cs,
                              const Variables_Set &parameters) = 0;
};

//! A tree node representing part of the space of solutions.
class PIP_Solution_Node : public PIP_Tree_Node {
public:
  //! Destructor.
  ~PIP_Solution_Node();

  //! Returns \p this.
  virtual const PIP_Solution_Node* as_solution() const;

  //! Returns \p this.
  virtual PIP_Solution_Node* as_solution();

  /*! \brief
    Returns a parametric expression of the values of variable \p v.

    The returned linear expression only involves parameters.

    \exception std::invalid_argument
    Thrown if \p v is dimension-incompatible with \p *this
    or if \p v is a parameter.
  */
  const Linear_Expression& parametric_values(Variable v);

  //! Returns the constraints (on variables and parameters) of \p *this.
  const Constraint_System& constraints();

  void ascii_dump(std::ostream& s) const;
  bool ascii_load(std::istream& s);

private:
  //! A rational matrix, with a common nenominator
  class Rational_Matrix : public Matrix {
  public:
    //! Builds an empty matrix.
    /*!
      Rows' size and capacity are initialized to \f$0\f$.
    */
    Rational_Matrix();

    //! Builds a zero matrix with specified dimensions and flags.
    /*!
      \param n_rows
      The number of rows of the matrix that will be created;

      \param n_columns
      The number of columns of the matrix that will be created.

      \param row_flags
      The flags used to build the rows of the matrix;
      by default, the rows will have all flags unset.
    */
    Rational_Matrix(dimension_type n_rows, dimension_type n_columns,
	            Row::Flags row_flags = Row::Flags());

    //! Copy constructor.
    Rational_Matrix(const Rational_Matrix& y);

    //! Normalizes the modulo of coefficients so that they are mutually prime.
    /*!
      Computes the Greatest Common Divisor (GCD) among the elements of
      the matrix and normalizes them and the denominator by the GCD itself.
    */
    void normalize();

    //! Tests whether the matrix is integer, \e ie. the denominator is 1.
    bool is_integer() const;

    //! Returns the value of the denominator.
    const Coefficient &get_denominator() const;

    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);

  protected:
    Coefficient denominator;
  };

  //! The type for parametric simplex tableau.
  struct Tableau {
    //! The matrix of simplex coefficients.
    Rational_Matrix s;
    //! The matrix of parameter coefficients.
    Rational_Matrix t;
  };

  //! The parametric simplex tableau.
  Tableau tableau;

  //! A set containing the internal indices of the basic variables
  Variables_Set basis;

protected:
  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param parent_ref
    a pointer to the parent reference to \p this

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
  virtual void update_tableau(PIP_Tree_Node **parent_ref,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence &input_cs,
                              const Variables_Set &parameters);

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

  //! Returns the system of constraints controlling \p *this.
  const Constraint_System& constraints();

private:
  //! Pointer to the "true" child of \p *this.
  PIP_Tree_Node* true_child;

  //! Pointer to the "false" child of \p *this.
  PIP_Tree_Node* false_child;

  /*! \brief
    Constructs if \p cs then \p tcp \p else \p fcp.

    Constructs a decision node controlled by \p cs (which is copied),
    with "true" child \p tcp and "false" child \p fcp.

    \param cs
    The system of constraints controlling the node.

    \exception std::invalid_argument
    Thrown if \p cs contains strict inequalities.
  */
  PIP_Decision_Node(const Constraint_System& cs,
                    PIP_Tree_Node* fcp, PIP_Tree_Node* tcp);

  /*! \brief
    Constructs if \p cs then \p tcp \p else \p fcp.

    Constructs a decision node controlled by \p cs (which is recycled),
    with "true" child \p tcp and "false" child \p fcp.

    \param cs
    The system of constraints controlling the node.  It is not
    declared <CODE>const</CODE> because its data-structures may be
    recycled to build the polyhedron.

    \param dummy
    A dummy tag to syntactically differentiate this one
    from the other constructors.

    \exception std::invalid_argument
    Thrown if \p cs contains strict inequalities.
  */
  PIP_Decision_Node(Constraint_System& cs, Recycle_Input dummy,
                    PIP_Tree_Node* fcp, PIP_Tree_Node* tcp);

protected:
  /*! \brief
    Populates the parametric simplex tableau using external data, if necessary

    \param parent_ref
    a pointer to the parent reference to \p this

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
  virtual void update_tableau(PIP_Tree_Node **parent_ref,
                              dimension_type external_space_dim,
                              dimension_type first_pending_constraint,
                              const Constraint_Sequence &input_cs,
                              const Variables_Set &parameters);
};

typedef const PIP_Tree_Node* PIP_Tree;

} // namespace Parma_Polyhedra_Library

#include "PIP_Tree.inlines.hh"

#endif // !defined(PPL_PIP_Tree_defs_hh)
