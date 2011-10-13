/* Constraint class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Constraint_defs_hh
#define PPL_Constraint_defs_hh 1

#include "Constraint.types.hh"

#include "Congruence.types.hh"
#include "Variables_Set.types.hh"
#include "Polyhedron.types.hh"

#include "Linear_Expression.defs.hh"
#include "Variable.defs.hh"
#include "Topology.hh"

#include <iosfwd>

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The basic comparison function.
/*! \relates Constraint
  \return
  The returned absolute value can be \f$0\f$, \f$1\f$ or \f$2\f$.

  \param x
  A row of coefficients;

  \param y
  Another row.

  Compares \p x and \p y, where \p x and \p y may be of different size,
  in which case the "missing" coefficients are assumed to be zero.
  The comparison is such that:
  -# equalities are smaller than inequalities;
  -# lines are smaller than points and rays;
  -# the ordering is lexicographic;
  -# the positions compared are, in decreasing order of significance,
     1, 2, ..., \p size(), 0;
  -# the result is negative, zero, or positive if x is smaller than,
     equal to, or greater than y, respectively;
  -# when \p x and \p y are different, the absolute value of the
     result is 1 if the difference is due to the coefficient in
     position 0; it is 2 otherwise.

  When \p x and \p y represent the hyper-planes associated
  to two equality or inequality constraints, the coefficient
  at 0 is the known term.
  In this case, the return value can be characterized as follows:
  - -2, if \p x is smaller than \p y and they are \e not parallel;
  - -1, if \p x is smaller than \p y and they \e are parallel;
  -  0, if \p x and y are equal;
  - +1, if \p y is smaller than \p x and they \e are parallel;
  - +2, if \p y is smaller than \p x and they are \e not parallel.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
int compare(const Constraint& x, const Constraint& y);

}

//! A linear equality or inequality.
/*! \ingroup PPL_CXX_interface
  An object of the class Constraint is either:
  - an equality: \f$\sum_{i=0}^{n-1} a_i x_i + b = 0\f$;
  - a non-strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b \geq 0\f$; or
  - a strict inequality: \f$\sum_{i=0}^{n-1} a_i x_i + b > 0\f$;

  where \f$n\f$ is the dimension of the space,
  \f$a_i\f$ is the integer coefficient of variable \f$x_i\f$
  and \f$b\f$ is the integer inhomogeneous term.

  \par How to build a constraint
  Constraints are typically built by applying a relation symbol
  to a pair of linear expressions.
  Available relation symbols are equality (<CODE>==</CODE>),
  non-strict inequalities (<CODE>\>=</CODE> and <CODE>\<=</CODE>) and
  strict inequalities (<CODE>\<</CODE> and <CODE>\></CODE>).
  The space dimension of a constraint is defined as the maximum
  space dimension of the arguments of its constructor.

  \par
  In the following examples it is assumed that variables
  <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE>
  are defined as follows:
  \code
  Variable x(0);
  Variable y(1);
  Variable z(2);
  \endcode

  \par Example 1
  The following code builds the equality constraint
  \f$3x + 5y - z = 0\f$, having space dimension \f$3\f$:
  \code
  Constraint eq_c(3*x + 5*y - z == 0);
  \endcode
  The following code builds the (non-strict) inequality constraint
  \f$4x \geq 2y - 13\f$, having space dimension \f$2\f$:
  \code
  Constraint ineq_c(4*x >= 2*y - 13);
  \endcode
  The corresponding strict inequality constraint
  \f$4x > 2y - 13\f$ is obtained as follows:
  \code
  Constraint strict_ineq_c(4*x > 2*y - 13);
  \endcode
  An unsatisfiable constraint on the zero-dimension space \f$\Rset^0\f$
  can be specified as follows:
  \code
  Constraint false_c = Constraint::zero_dim_false();
  \endcode
  Equivalent, but more involved ways are the following:
  \code
  Constraint false_c1(Linear_Expression::zero() == 1);
  Constraint false_c2(Linear_Expression::zero() >= 1);
  Constraint false_c3(Linear_Expression::zero() > 0);
  \endcode
  In contrast, the following code defines an unsatisfiable constraint
  having space dimension \f$3\f$:
  \code
  Constraint false_c(0*z == 1);
  \endcode

  \par How to inspect a constraint
  Several methods are provided to examine a constraint and extract
  all the encoded information: its space dimension, its type
  (equality, non-strict inequality, strict inequality) and
  the value of its integer coefficients.

  \par Example 2
  The following code shows how it is possible to access each single
  coefficient of a constraint. Given an inequality constraint
  (in this case \f$x - 5y + 3z \leq 4\f$), we construct a new constraint
  corresponding to its complement (thus, in this case we want to obtain
  the strict inequality constraint \f$x - 5y + 3z > 4\f$).
  \code
  Constraint c1(x - 5*y + 3*z <= 4);
  cout << "Constraint c1: " << c1 << endl;
  if (c1.is_equality())
    cout << "Constraint c1 is not an inequality." << endl;
  else {
    Linear_Expression e;
    for (dimension_type i = c1.space_dimension(); i-- > 0; )
      e += c1.coefficient(Variable(i)) * Variable(i);
    e += c1.inhomogeneous_term();
    Constraint c2 = c1.is_strict_inequality() ? (e <= 0) : (e < 0);
    cout << "Complement c2: " << c2 << endl;
  }
  \endcode
  The actual output is the following:
  \code
  Constraint c1: -A + 5*B - 3*C >= -4
  Complement c2: A - 5*B + 3*C > 4
  \endcode
  Note that, in general, the particular output obtained can be
  syntactically different from the (semantically equivalent)
  constraint considered.
*/
class Parma_Polyhedra_Library::Constraint {
public:

  //! The possible kinds of Constraint objects.
  enum Kind {
    LINE_OR_EQUALITY = 0,
    RAY_OR_POINT_OR_INEQUALITY = 1
  };

  //! The constraint type.
  enum Type {
    /*! The constraint is an equality. */
    EQUALITY,
    /*! The constraint is a non-strict inequality. */
    NONSTRICT_INEQUALITY,
    /*! The constraint is a strict inequality. */
    STRICT_INEQUALITY
  };

  // TODO: Update the documentation of this method.
  //! Constructs the \f$0<0\f$ constraint.
  explicit Constraint(dimension_type sz = 2);

  //! Constructs the \f$0<0\f$ constraint.
  Constraint(dimension_type sz, Kind kind, Topology topology);

  //! Constructs the \f$0<0\f$ constraint.
  Constraint(dimension_type sz, dimension_type capacity);

  // TODO: Update the documentation of this method.
  //! Constructs the \f$0<0\f$ constraint.
  Constraint(dimension_type sz, dimension_type capacity, Kind kind,
             Topology topology);

  //! Ordinary copy constructor.
  Constraint(const Constraint& c);

  //! Copy constructor with given size.
  Constraint(const Constraint& c, dimension_type sz);
  
  //! Copy constructor with given size and capacity.
  Constraint(const Constraint& c, dimension_type sz, dimension_type capacity);
  
  //! Copy-constructs from equality congruence \p cg.
  /*!
    \exception std::invalid_argument
    Thrown if \p cg is a proper congruence.
  */
  explicit Constraint(const Congruence& cg);

  //! Destructor.
  ~Constraint();

  //! \name Flags inspection methods
  //@{
  //! Returns the topological kind of \p *this.
  Topology topology() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if the topology
    of \p *this row is not necessarily closed.
  */
  bool is_not_necessarily_closed() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if the topology
    of \p *this row is necessarily closed.
  */
  bool is_necessarily_closed() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this row
    represents a line or an equality.
  */
  bool is_line_or_equality() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this row
    represents a ray, a point or an inequality.
  */
  bool is_ray_or_point_or_inequality() const;
  //@} // Flags inspection methods

  //! \name Flags coercion methods
  //@{

  //! Sets to \p x the topological kind of \p *this row.
  void set_topology(Topology x);

  // TODO: Consider removing this, or making it private.
  //! Marks the epsilon dimension as a standard dimension.
  /*!
    The row topology is changed to <CODE>NOT_NECESSARILY_CLOSED</CODE>, and
    the number of space dimensions is increased by 1.
  */
  void mark_as_necessarily_closed();

  // TODO: Consider removing this, or making it private.
  //! Marks the last dimension as the epsilon dimension.
  /*!
    The row topology is changed to <CODE>NECESSARILY_CLOSED</CODE>, and
    the number of space dimensions is decreased by 1.
  */
  void mark_as_not_necessarily_closed();

  //! Sets to \p NECESSARILY_CLOSED the topological kind of \p *this row.
  void set_necessarily_closed();

  //! Sets to \p NOT_NECESSARILY_CLOSED the topological kind of \p *this row.
  void set_not_necessarily_closed();

  //! Sets to \p LINE_OR_EQUALITY the kind of \p *this row.
  void set_is_line_or_equality();

  //! Sets to \p RAY_OR_POINT_OR_INEQUALITY the kind of \p *this row.
  void set_is_ray_or_point_or_inequality();
  //@} // Flags coercion methods

  //! Assignment operator.
  Constraint& operator=(const Constraint& c);

  //! Returns the maximum space dimension a Constraint can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Sets the dimension of the vector space enclosing \p *this to
  //! \p space_dim .
  void set_space_dimension(dimension_type space_dim);

  //! Swaps the coefficients of the variables \p v1 and \p v2 .
  void swap_space_dimensions(Variable v1, Variable v2);

  // TODO: Consider making this private.
  //! Removes all the specified dimensions from the constraint.
  /*!
    The space dimension of the variable with the highest space
    dimension in \p vars must be at most the space dimension
    of \p this.

    Always returns \p true. The return value is needed for compatibility with
    the Generator class.
  */
  bool remove_space_dimensions(const Variables_Set& vars);

  // TODO: Consider making this private.
  //! Permutes the space dimensions of the constraint.
  /*
    \param cycle
    A vector representing a cycle of the permutation according to which the
    space dimensions must be rearranged.

    The \p cycle vector represents a cycle of a permutation of space
    dimensions.
    For example, the permutation
    \f$ \{ x_1 \mapsto x_2, x_2 \mapsto x_3, x_3 \mapsto x_1 \}\f$ can be
    represented by the vector containing \f$ x_1, x_2, x_3 \f$.
  */
  void permute_space_dimensions(const std::vector<Variable>& cycle);

  //! Returns the constraint type of \p *this.
  Type type() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is an equality constraint.
  */
  bool is_equality() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is an inequality constraint (either strict or non-strict).
  */
  bool is_inequality() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is a non-strict inequality constraint.
  */
  bool is_nonstrict_inequality() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is a strict inequality constraint.
  */
  bool is_strict_inequality() const;

  //! Linearly combines \p *this with \p y so that the coefficient of \p v
  //! is 0.
  /*!
    \param y
    The Constraint that will be combined with \p *this object;

    \param v
    The variable whose coefficient has to become \f$0\f$.

    Computes a linear combination of \p *this and \p y having
    the coefficient of variable \p v equal to \f$0\f$. Then it assigns
    the resulting Constraint to \p *this and normalizes it.
  */
  void linear_combine(const Constraint& y, Variable v);

  //! Linearly combines \p *this with \p y so that the inhomogeneous term
  //! becomes 0.
  /*!
    \param y
    The expression that will be combined with \p *this object;

    Computes a linear combination of \p *this and \p y having
    the inhomogeneous term equal to \f$0\f$. Then it assigns
    the resulting constraint to \p *this.

    \p *this and \p y must have the same space dimension.
  */
  void linear_combine_inhomogeneous(const Constraint& y);
  
  //! Returns the coefficient of \p v in \p *this.
  /*!
    \exception std::invalid_argument thrown if the index of \p v
    is greater than or equal to the space dimension of \p *this.
  */
  Coefficient_traits::const_reference coefficient(Variable v) const;

  //! Returns the inhomogeneous term of \p *this.
  Coefficient_traits::const_reference inhomogeneous_term() const;

  /*! \brief
    Normalizes the sign of the coefficients so that the first non-zero
    (homogeneous) coefficient of a line-or-equality is positive.
  */
  void sign_normalize();

  /*! \brief
    Strong normalization: ensures that different Constraint objects
    represent different hyperplanes or hyperspaces.

    Applies both Constraint::normalize() and Constraint::sign_normalize().
  */
  void strong_normalize();

  /*! \brief
    Returns <CODE>true</CODE> if and only if the coefficients are
    strongly normalized.
  */
  bool check_strong_normalized() const;

  //! Initializes the class.
  static void initialize();

  //! Finalizes the class.
  static void finalize();

  //! The unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$.
  static const Constraint& zero_dim_false();

  /*! \brief
    The true (zero-dimension space) constraint \f$0 \leq 1\f$,
    also known as <EM>positivity constraint</EM>.
  */
  static const Constraint& zero_dim_positivity();

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is a tautology (i.e., an always true constraint).

    A tautology can have either one of the following forms:
    - an equality: \f$\sum_{i=0}^{n-1} 0 x_i + 0 = 0\f$; or
    - a non-strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b \geq 0\f$,
      where \f$b \geq 0\f$; or
    - a strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b > 0\f$,
      where \f$b > 0\f$.
  */
  bool is_tautological() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this is inconsistent (i.e., an always false constraint).

    An inconsistent constraint can have either one of the following forms:
    - an equality: \f$\sum_{i=0}^{n-1} 0 x_i + b = 0\f$,
      where \f$b \neq 0\f$; or
    - a non-strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b \geq 0\f$,
      where \f$b < 0\f$; or
    - a strict inequality: \f$\sum_{i=0}^{n-1} 0 x_i + b > 0\f$,
      where \f$b \leq 0\f$.
  */
  bool is_inconsistent() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y
    are equivalent constraints.

    Constraints having different space dimensions are not equivalent.
    Note that constraints having different types may nonetheless be
    equivalent, if they both are tautologies or inconsistent.
  */
  bool is_equivalent_to(const Constraint& y) const;

  //! Returns <CODE>true</CODE> if \p *this is identical to \p y.
  /*!
    This is faster than is_equivalent_to(), but it may return `false' even
    for equivalent constraints.
  */
  bool is_equal_to(const Constraint& y) const;

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //! Swaps \p *this with \p y.
  void swap(Constraint& y);

  /*! \brief
    Builds a constraint of type \p type and topology \p topology,
    stealing the coefficients from \p e.
  */
  Constraint(Linear_Expression& e, Type type, Topology topology);

  //! Constructs from a congruence, with specified size and capacity.
  Constraint(const Congruence& cg, dimension_type sz, dimension_type capacity);

  //! Returns the zero-dimension space constraint \f$\epsilon \geq 0\f$.
  static const Constraint& epsilon_geq_zero();

  /*! \brief
    The zero-dimension space constraint \f$\epsilon \leq 1\f$
    (used to implement NNC polyhedra).
  */
  static const Constraint& epsilon_leq_one();

  // TODO: Make this private.
  //! Sets the constraint type to <CODE>EQUALITY</CODE>.
  void set_is_equality();

  // TODO: Make this private.
  //! Sets the constraint to be an inequality.
  /*!
    Whether the constraint type will become <CODE>NONSTRICT_INEQUALITY</CODE>
    or <CODE>STRICT_INEQUALITY</CODE> depends on the topology and the value
    of the low-level coefficients of the constraint.
  */
  void set_is_inequality();

  // TODO: Remove this.
  Linear_Expression& expression();

  // TODO: Remove this.
  const Linear_Expression& expression() const;

private:
  Linear_Expression expr;

  Kind kind_;

  Topology topology_;

  /*! \brief
    Holds (between class initialization and finalization) a pointer to
    the unsatisfiable (zero-dimension space) constraint \f$0 = 1\f$.
  */
  static const Constraint* zero_dim_false_p;

  /*! \brief
    Holds (between class initialization and finalization) a pointer to
    the true (zero-dimension space) constraint \f$0 \leq 1\f$, also
    known as <EM>positivity constraint</EM>.
  */
  static const Constraint* zero_dim_positivity_p;

  /*! \brief
    Holds (between class initialization and finalization) a pointer to
    the zero-dimension space constraint \f$\epsilon \geq 0\f$.
  */
  static const Constraint* epsilon_geq_zero_p;

  /*! \brief
    Holds (between class initialization and finalization) a pointer to
    the zero-dimension space constraint \f$\epsilon \leq 1\f$
    (used to implement NNC polyhedra).
  */
  static const Constraint* epsilon_leq_one_p;

  /*! \brief
    Throws a <CODE>std::invalid_argument</CODE> exception containing
    error message \p message.
  */
  void
  throw_invalid_argument(const char* method, const char* message) const;

  /*! \brief
    Throws a <CODE>std::invalid_argument</CODE> exception
    containing the appropriate error message.
  */
  void
  throw_dimension_incompatible(const char* method,
			       const char* name_var,
			       Variable v) const;

  //! Returns the epsilon coefficient. The constraint must be NNC.
  Coefficient_traits::const_reference epsilon_coefficient() const;

  //! Sets the epsilon coefficient to \p n. The constraint must be NNC.
  void set_epsilon_coefficient(Coefficient_traits::const_reference n);

  /*! \brief
    Builds a new copy of the zero-dimension space constraint
    \f$\epsilon \geq 0\f$ (used to implement NNC polyhedra).
  */
  static Constraint construct_epsilon_geq_zero();

  friend int
  compare(const Constraint& x, const Constraint& y);

  friend class Constraint_System;
  friend class Polyhedron;
};

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Constraint */
std::ostream& operator<<(std::ostream& s, const Constraint& c);

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Constraint */
std::ostream& operator<<(std::ostream& s, const Constraint::Type& t);

} // namespace IO_Operators

//! Returns <CODE>true</CODE> if and only if \p x is equivalent to \p y.
/*! \relates Constraint */
bool
operator==(const Constraint& x, const Constraint& y);

//! Returns <CODE>true</CODE> if and only if \p x is not equivalent to \p y.
/*! \relates Constraint */
bool
operator!=(const Constraint& x, const Constraint& y);

//! Returns the constraint \p e1 = \p e2.
/*! \relates Constraint */
Constraint
operator==(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the constraint \p v1 = \p v2.
/*! \relates Constraint */
Constraint
operator==(Variable v1, Variable v2);

//! Returns the constraint \p e = \p n.
/*! \relates Constraint */
Constraint
operator==(const Linear_Expression& e, Coefficient_traits::const_reference n);

//! Returns the constraint \p n = \p e.
/*! \relates Constraint */
Constraint
operator==(Coefficient_traits::const_reference n, const Linear_Expression& e);

//! Returns the constraint \p e1 \<= \p e2.
/*! \relates Constraint */
Constraint
operator<=(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the constraint \p v1 \<= \p v2.
/*! \relates Constraint */
Constraint
operator<=(Variable v1, Variable v2);

//! Returns the constraint \p e \<= \p n.
/*! \relates Constraint */
Constraint
operator<=(const Linear_Expression& e, Coefficient_traits::const_reference n);

//! Returns the constraint \p n \<= \p e.
/*! \relates Constraint */
Constraint
operator<=(Coefficient_traits::const_reference n, const Linear_Expression& e);

//! Returns the constraint \p e1 \>= \p e2.
/*! \relates Constraint */
Constraint
operator>=(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the constraint \p v1 \>= \p v2.
/*! \relates Constraint */
Constraint
operator>=(Variable v1, Variable v2);

//! Returns the constraint \p e \>= \p n.
/*! \relates Constraint */
Constraint
operator>=(const Linear_Expression& e, Coefficient_traits::const_reference n);

//! Returns the constraint \p n \>= \p e.
/*! \relates Constraint */
Constraint
operator>=(Coefficient_traits::const_reference n, const Linear_Expression& e);

//! Returns the constraint \p e1 \< \p e2.
/*! \relates Constraint */
Constraint
operator<(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the constraint \p v1 \< \p v2.
/*! \relates Constraint */
Constraint
operator<(Variable v1, Variable v2);

//! Returns the constraint \p e \< \p n.
/*! \relates Constraint */
Constraint
operator<(const Linear_Expression& e, Coefficient_traits::const_reference n);

//! Returns the constraint \p n \< \p e.
/*! \relates Constraint */
Constraint
operator<(Coefficient_traits::const_reference n, const Linear_Expression& e);

//! Returns the constraint \p e1 \> \p e2.
/*! \relates Constraint */
Constraint
operator>(const Linear_Expression& e1, const Linear_Expression& e2);

//! Returns the constraint \p v1 \> \p v2.
/*! \relates Constraint */
Constraint
operator>(Variable v1, Variable v2);

//! Returns the constraint \p e \> \p n.
/*! \relates Constraint */
Constraint
operator>(const Linear_Expression& e, Coefficient_traits::const_reference n);

//! Returns the constraint \p n \> \p e.
/*! \relates Constraint */
Constraint
operator>(Coefficient_traits::const_reference n, const Linear_Expression& e);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Constraint */
template <>
void swap(Parma_Polyhedra_Library::Constraint& x,
          Parma_Polyhedra_Library::Constraint& y);

} // namespace std


#include "Constraint.inlines.hh"

#endif // !defined(PPL_Constraint_defs_hh)
