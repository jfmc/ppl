/* Polynomial_Space class declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Polynomial_Space_defs_hh
#define PPL_Polynomial_Space_defs_hh 1

#include "Polynomial_Space.types.hh"
#include "globals.types.hh"
#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Constraint_System.types.hh"
#include "Generator_System.types.hh"
#include "Polyhedron.types.hh"
#include "Poly_Con_Relation.types.hh"
#include "Poly_Gen_Relation.types.hh"
#include "Polynomial.types.hh"
#include "Polynomial_Constraint.types.hh"
#include "Polynomial_Constraint_System.types.hh"
#include "Polynomial_Constraint_System.defs.hh"
#include "Term.defs.hh"
#include <map>
#include <cstddef>
#include <iosfwd>
#include <list>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial_Space
  Writes a textual representation of \p ps on \p s:
  <CODE>false</CODE> is written if \p ps is the bottom polynomial space;
  <CODE>true</CODE> is written if \p ps is the top polynomial space;
  a system of polynomial constraints defining \p ps is written otherwise,
  all constraints separated by ", ".
*/
template <degree_type db>
std::ostream&
operator<<(std::ostream& s, const Polynomial_Space<db>& ps);

} // namespace IO_Operators

/*! \brief
  Returns <CODE>true</CODE> if and only if \p x and \p y are
  the same polynomial space.

  \relates Polynomial_Space
  Note that \p x and \p y may be dimension-incompatible spaces:
  in this case, the value <CODE>false</CODE> is returned.
*/
template <degree_type db>
bool operator==(const Polynomial_Space<db>& x, const Polynomial_Space<db>& y);

/*! \brief
  Returns <CODE>true</CODE> if and only if \p x and \p y are not the
  same space.

 \relates Polynomial_Space
  Note that \p x and \p y may be dimension-incompatible spaces:
  in this case, the value <CODE>true</CODE> is returned.
*/
template <degree_type db>
bool operator!=(const Polynomial_Space<db>& x, const Polynomial_Space<db>& y);

/*! \brief
  Assigns to term \p t the following term in the graded lexicographical
  ordering with \p num_dimensions variables.
*/
void next_graded_lex_term(dimension_type num_dimensions, Term& t);

/*! \brief
  Returns the first term in the graded lexicographical ordering of
  degree \f$ db + 1 \f$, marking the end of the terms of degree \p db.
*/
Term end_graded_lex_term(degree_type db);

/*! \brief
  Returns the polynomial obtained by moving forward all variables in \p
  p a number of \p offset dimensions.
*/
Polynomial move_to_higher_space_dimensions(const Polynomial& p,
					   dimension_type offset);

/*! \brief
  Returns the polynomial obtained by substituting the occurrences of
  variable \p x in the polynomial \p p by the polynomial \p q.
*/
Polynomial substitute(const Polynomial& p, Variable x, const Polynomial& q);

} // namespace Parma_Polyhedra_Library

//! A polynomial space.
/*! \ingroup PPL_CXX_interface
  Let \f$\Rset_d[x_1, \dots, x_n]\f$ be the set of all polynomial
  equality constraints in the variables \f$x_1, \dots, x_n\f$ of
  degree at most \f$d\f$.
  Consider the <EM>entailment relation</EM>
  \f$
    \reld{\entails}
         {\wpf\bigl(\Rset_d[x_1, \dots, x_n]\bigr)}
         {\Rset_d[x_1, \dots, x_n]}
  \f$
  defined by the following axiom schemata:

  1) \f$ \emptyset \entails 0 = 0 \f$;
  2) \f$ \{ p_1 = 0, p_2 = 0 \} \entails p_1 + p_2 = 0 \f$;
  3) \f$ \{ p = 0 \} \entails \lambda p = 0 \f$,
     if \f$\lambda \in \Rset\f$;
  4) \f$ \{ p_1 = 0, p_2 = 0 \} \entails p_1 p_2 = 0 \f$,
     if \f$ \deg(p_1p_2) \leq d \f$.

  The <EM>inference map</EM>
  \f$
    \fund{\rho_{\entails}}
         {\wp\bigl(\Rset_d[x_1, \dots, x_n]\bigr)}
         {\wp\bigl(\Rset_d[x_1, \dots, x_n]\bigr)}
  \f$
  is given, for each \f$P \sseq \Rset_d[x_1, \dots, x_n]\f$, by
  \f[
    \rho_{\entails}(P)
      \defeq
        \{\,
          p \in \Rset_d[x_1, \dots, x_n]
        \mid
          \exists P' \sseqf P \st P' \entails p
        \,\}.
  \f]

  A polynomial space of degree \f$d\f$ in the variables
  \f$x_1, \dots, x_n\f$ is a set \f$P \sseq  \Rset_d[x_1, \dots, x_n]\f$
  that is closed under `\f$\entails\f$', i.e., such that
  \f$\rho_{\entails}(P) = P\f$.  An equality constraint \f$p = 0\f$ can be
  identified with the polynomial \f$p\f$.  Using this identification,
  the sets of polynomials that are closed under 1), 2) and 3) have the
  algebraic structure of a vector space.  So, a polynomial space of
  degree \f$d\f$ can also be regarded as a vector space of polynomials
  that is closed under bounded-degree products, i.e., closed under 4).

  \if Include_Implementation_Details

  A polynomial space is implemented as a sequence of
  \ref Terms_Monomials_and_Polynomials "polynomials in primitive form"
  which are a basis of the polynomial space regarded as a vector space.
  A polynomial space has a canonical form: by considering the sequence of
  polynomials as the sequence of rows of a matrix whose columns are
  indexed by decreasing terms (according to the total ordering defined
  on terms), the canonical form is the reduced row-echelon form of the matrix.
  For instance, the representation given by the sequence of polynomials
  \f$y^2 + x - y\f$, \f$2xy - x\f$, \f$3x^2 + 2x + 2y\f$
  is in canonical form, since the polynomials are in primitive form and
  the resulting matrix is in reduced row-echelon form:
  \f[
    \begin{array}{cccccc|l}
      y^2 &   xy &   x^2 &  y  &   x &   1 &                \\
      \hline
       1  &      &       &  -1 &   1 &     & y^2 + x - y    \\
          &   2  &       &     &  -1 &     & 2xy - x        \\
          &      &   3   &  2  &  2  &     & 3x^2 + 2x + 2y \\
    \end{array}
  \f]

  Note that different polynomial spaces may actually represent the
  same set of points, i.e., have the same set of zeroes. For instance,
  \f$\rho_{\entails}\bigl(\{x^2, x , 1\}\bigr)\f$ and
  \f$\rho_{\entails}\bigl(\{x^2 + 1\}\bigr)\f$ are different polynomial
  spaces of degree \f$2\f$, but they both represent the empty subset
  of \f$\Rset\f$.

  Polynomial spaces of degree \f$d\f$ form a lattice, which is ordered
  by superset inclusion. The top polynomial space is the null
  vector space \f$\{0\} = \rho_{\entails}(\emptyset)\f$. On the other hand,
  the bottom is the set of all polynomials of degree at most \f$d\f$:
  \f$
    \Rset_d[x_1, \dots, x_n]
      = \rho_{\entails}\bigl(\Rset_d[x_1, \dots, x_n]\bigr)
  \f$.
  The join is defined as set intersection, whereas the meet between
  the polynomial spaces \f$P_1\f$ and \f$P_2\f$ is defined as
  \f$\rho_{\entails}(P_1 \union P_2)\f$.
  More generally, the domain of polynomial spaces of degree \f$d\f$
  is a <EM>determinate constraint system</EM> as defined in
  \ref Bag98 "[Bag98]".

  \endif
*/
template <Parma_Polyhedra_Library::degree_type db>
class Parma_Polyhedra_Library::Polynomial_Space {
public:
  //! Returns the maximum space dimension that a polynomial space can handle.
  static dimension_type max_space_dimension();

  //! \name Constructors, Assignment, Swap and Destructor
  //@{

  //! The degenerate space type.
  enum Degenerate_Space {
    //! Top polynomial space.
    TOP,
    //! Bottom polynomial space.
    BOTTOM
  };

  //! Builds either the top or bottom polynomial space.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the polynomial
    space;

    \param kind
    Specifies whether the top or the bottom polynomial space has to be built.
  */
  explicit Polynomial_Space(dimension_type num_dimensions = 0,
			    Degenerate_Space kind = TOP);

  //! Ordinary copy-constructor.
  Polynomial_Space(const Polynomial_Space& y);

  //! Builds a conservative, upward approximation of \p y.
  template <Parma_Polyhedra_Library::degree_type other_db>
  explicit Polynomial_Space(const Polynomial_Space<other_db>& y);

  /*! \brief
    Builds a polynomial space of dimension \p num_dimensions from
    the system of constraints \p cs.

    \param num_dimensions
    The number of dimensions of the vector space enclosing the polynomial
    space;

    \param cs
    A system of linear constraints.

    \exception std::invalid_argument
    Thrown if \p num_dimensions is smaller than the syntactic space
    dimension of \p cs.

    \note
    Those constraints in \p cs which are not equalities are ignored.
  */
  Polynomial_Space(dimension_type num_dimensions,
		   const Constraint_System& cs);

  /*! \brief
    Builds a polynomial space of dimension \p num_dimensions from
    the system of polynomial constraints \p cs.

    \param num_dimensions
    The number of dimensions of the vector space enclosing the polynomial
    space;

    \param pcs
    A system of polynomial constraints.

    \exception std::invalid_argument
    Thrown if \p num_dimensions is smaller than the syntactic space
    dimension of \p pcs.

    Those constraints in \p pcs which are not equalities are ignored.
  */
  Polynomial_Space(dimension_type num_dimensions,
		   const Polynomial_Constraint_System& pcs);

  /*! \brief
    The assignment operator
    (\p *this and \p y can be dimension-incompatible).
  */
  Polynomial_Space& operator=(const Polynomial_Space& y);

  /*! \brief
    Swaps \p *this with \p y
    (\p *this and \p y can be dimension-incompatible).
  */
  void swap(Polynomial_Space& y);

  //@} Constructors, Assignment, Swap and Destructor

  //! \name Member Functions that Do Not Modify the Polynomial_Space
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Returns a system of polynomial constraints defining \p *this.
  Polynomial_Constraint_System polynomial_constraints() const;

  //! Returns <CODE>true</CODE> if and only if \p *this entails \p y.
  /*!
    \note
    The returned result may be <CODE>false</CODE>, even though the
    zeroes of *this may be included in the set of zeroes of \p y.
    For instance, when the degree boudn is 2, the polynomial
    space \f$ [x^2] \f$ does not entail \f$ [x] \f$, even though they
    have the same set of zeroes. On the other hand, the soundness of
    the entailment relation guarantees that, when <CODE>true</CODE> is
    returned, the zeroes of *this are included in the set of zeroes of
    \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool entails(const Polynomial_Space& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the
    bottom polynomial space.
  */
  bool is_bottom() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the
    top polynomial space.
  */
  bool is_top() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this satisfies
    all its invariants.
  */
  bool OK() const;

  //@} Member Functions that Do Not Modify the Polynomial_Space

  //! \name Space-Dimension Preserving Member Functions that May Modify the Polynomial_Space
  //@{

  /*! \brief
    Adds a copy of constraint \p c to the system of polynomial constraints
    defining \p *this.

    \param c
    The constraint to be added.

    Note: the constraint is ignored if it is not an equality.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    Adds a copy of\p c to the system of polynomial constraints
    defining \p *this.

    \param c
    The polynomial constraint to be added.

    Note: the constraint is ignored if it is not an equality or if its
    degree exceeds the degree bound \p db.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible.
  */
  void add_polynomial_constraint(const Polynomial_Constraint& c);

  /*! \brief
    Adds the constraints in \p cs to the system of polynomial constraints
    defining \p *this.

    \param  cs
    The constraints that will be added.

    Note: those constraints which are not equalities are ignored.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible.
  */
  void add_constraints(const Constraint_System& cs);

  /*! \brief
    Adds the polynomial constraints in \p pcs to the system of polynomial
    constraints defining \p *this.

    \param  pcs
    The polynomial constraints that will be added.

    Note: those constraints which are not equalities or whose degree
    exceeds the degree bound \p db are ignored.

    \exception std::invalid_argument
    Thrown if \p *this and \p pcs are dimension-incompatible.
  */
  void add_polynomial_constraints(const Polynomial_Constraint_System& pcs);

  //! Assigns to \p *this the meet of \p *this and \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void meet_assign(const Polynomial_Space& y);

  /*! \brief
    Assigns to \p *this a polynomial space containing
    the join of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void join_assign(const Polynomial_Space& y);

  //! Same as join_assign.
  void upper_bound_assign(const Polynomial_Space& y);

  /*! \brief
    Assigns to \p *this the polynomial space difference
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  template <Parma_Polyhedra_Library::degree_type other_db>
  void polynomial_space_difference_assign(const Polynomial_Space<other_db>& y);

  //! Same as polynomial_space_difference_assign.
  template <Parma_Polyhedra_Library::degree_type other_db>
  void difference_assign(const Polynomial_Space<other_db>& y);

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Affine_Functions "affine image"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is assigned;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    Coefficient_traits::const_reference denominator
		    = Coefficient_one());

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Polynomial_Functions "polynomial image"
    of \p *this under the function mapping variable \p var into the
    polynomial expression specified by \p expr and \p denominator.

    \param var
    The variable to which the polynomial expression is assigned;

    \param expr
    The numerator of the polynomial expression;

    \param denominator
    The denominator of the polynomial expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
   void polynomial_image(Variable var,
			const Polynomial& expr,
			Coefficient_traits::const_reference denominator
			= Coefficient_one());


  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Affine_Functions "affine preimage"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is substituted;

    \param expr
    The numerator of the affine expression;

    \param denominator
    The denominator of the affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
  void affine_preimage(Variable var,
		       const Linear_Expression& expr,
		       Coefficient_traits::const_reference denominator
		       = Coefficient_one());

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Polynomial_Functions "polynomial preimage"
    of \p *this under the function mapping variable \p var into the
    polynomial expression specified by \p expr and \p denominator.

    \param var
    The variable to which the polynomial expression is substituted;

    \param expr
    The numerator of the polynomial expression;

    \param denominator
    The denominator of the polynomial expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension of \p *this.
  */
  void polynomial_preimage(Variable var,
			   const Polynomial& expr,
			   Coefficient_traits::const_reference denominator
			   = Coefficient_one());

  //! Same as join_assign.
  void widening_assign(const Polynomial_Space& y, unsigned* tp = 0);

  //@} Space-Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  /*! \brief
    Adds \p m new dimensions and embeds the old polynomial space into
    the new space.

    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the
    new polynomial space, which is obtained by letting the variables
    running through the new dimensions be unconstrained.  For
    instance, for \f$d = 2\f$ when starting from the polynomial space
    \f$\{\, c x + d x^2 \mid c \in \mathbb{R} \,\}\f$
    and adding a second dimension,
    the result will be the polynomial space
    \f[
      \{\, c x + d x^2 + e x y \mid c \in \mathbb{R} \,\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds \p m new dimensions to the polynomial space and does not embed it in
    the new vector space.

    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the
    new polynomial space, which is obtained by constraining the
    variables running through the new dimensions to 0.  For instance,
    for \f$d = 2\f$ when starting from the polynomial space
    \f$\{\, c x + d x^2 \mid c \in \mathbb{R} \,\}\f$
    and adding a second dimension, the result will be the polynomial space
    \f[
      \{\, c x + d x^2 + e y + f x y + g y^2 \mid c \in \mathbb{R} \,\}.
    \f]

  */
  void add_space_dimensions_and_project(dimension_type m);

  /*! \brief
    First increases the space dimension of \p *this by adding \p
    y.space_dimension() new dimensions; then adds to the system of
    constraints of \p *this a renamed-apart version of the constraints
    of \p y.
  */
  void concatenate_assign(const Polynomial_Space& y);

  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed
    The set of Variable objects corresponding to the dimensions to be removed.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with one of the Variable
    objects contained in \p to_be_removed.
  */
  void remove_space_dimensions(const Variables_Set& to_be_removed);

  /*! \brief
    Removes the higher dimensions so that the resulting space
    will have dimension \p new_dimension.

    \exception std::invalid_argument
    Thrown if \p new_dimensions is greater than the space dimension
    of \p *this.
  */
  void remove_higher_space_dimensions(dimension_type new_dimension);

  // FIXME: how to implement this?
  /*! \brief
    Remaps the dimensions of the vector space according to
    a \ref Mapping_the_Dimensions_of_the_Vector_Space "partial function".

    \param pfunc
    The partial function specifying the destiny of each dimension.

    The template class PartialFunction must provide the following
    methods.
    \code
      bool has_empty_codomain() const
    \endcode
    returns <CODE>true</CODE> if and only if the represented partial
    function has an empty co-domain (i.e., it is always undefined).
    The <CODE>has_empty_codomain()</CODE> method will always be called
    before the methods below.  However, if
    <CODE>has_empty_codomain()</CODE> returns <CODE>true</CODE>, none
    of the functions below will be called.
    \code
      dimension_type max_in_codomain() const
    \endcode
    returns the maximum value that belongs to the co-domain
    of the partial function.
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$k\f$ be the value
    of \p i.  If \f$f\f$ is defined in \f$k\f$, then \f$f(k)\f$ is
    assigned to \p j and <CODE>true</CODE> is returned.
    If \f$f\f$ is undefined in \f$k\f$, then <CODE>false</CODE> is
    returned.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in the
    \ref Mapping_the_Dimensions_of_the_Vector_Space
    "specification of the mapping operator".
  */
  template <typename PartialFunction>
  void map_space_dimensions(const PartialFunction& pfunc);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

  PPL_OUTPUT_DECLARATIONS;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by ascii_dump)
    and sets \p *this accordingly.  Returns <CODE>true</CODE> if successful,
    <CODE>false</CODE> otherwise.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  friend bool
  Parma_Polyhedra_Library::operator==<db>(const Polynomial_Space<db>& x,
					  const Polynomial_Space<db>& y);

private:

  // The container type used to represent the sequence of polynomials.
  typedef std::list<Polynomial> Polynomial_Sequence;

  // Iterators on the container.
  typedef Polynomial_Sequence::iterator Iterator;
  typedef Polynomial_Sequence::const_iterator C_Iterator;
  typedef Polynomial_Sequence::reverse_iterator R_Iterator;
  typedef Polynomial_Sequence::const_reverse_iterator CR_Iterator;

  //! The sequence of polynomials.
  Polynomial_Sequence polynomials;

  template <Parma_Polyhedra_Library::degree_type other_db>
  friend class Parma_Polyhedra_Library::Polynomial_Space;

  /*! \brief
    Flag indicating if the sequence is in canonical form (i.e.,
    primitive reduced row-echelon form).
  */
  bool in_canonical_form;

  //! The dimension of the vector space enclosing the polynomial space.
  dimension_type dim;

  /*! \brief
    Finds the canonical (i.e., primitive reduced row-echelon) form of \p
    *this.
  */
  void canonicalize() const;

  //! Existentially quantifies on all terms having degree greater than \p deg.
  void existentially_quantify(degree_type deg);

  /*! \brief
    Existentially quantifies on all terms where variable \p v
    has exponent greater than zero.
  */
  void existentially_quantify_all(const Variable v);

  //! Existentially quantifies on term \p t.
  void existentially_quantify(const Term& t);

  //! Computes the closure with respect to bounded degree products.
  void close_by_products(void);

  /*! \brief
    Computes a list of the degrees of the polynomials in \p *this in
    the variable \p var, and sets \p max_deg to the maximum of all of
    them.  The behaviour is undefined is \p degrees is not empty.
  */
  void compute_degrees(const Variable var,
		       exponent_type& max_deg,
		       std::list<exponent_type>& degrees) const;

  /*! \brief
    Assigns to \p *this the result of substituting the occurrences of
    variable \p var by \p new_num/\p new_denom in each polynomial
    \f$ p * (new_denom)^degree(p,var)\f$, where \p p is a polynomial
    in \p *this, and then taking the primitive form of the substituted
    polynomial.  The degrees degree(\p p, \p var) of the polynomials
    \p p belonging to \p *this in \p var are stored in \p degrees, and
    the maximum of these degrees is \p max_deg.

    The behaviour is undefined if \p degrees is not empty.
  */
  void substitute_assign(const Variable var,
			 const Polynomial& new_num,
			 Coefficient_traits::const_reference new_den,
			 const exponent_type& max_deg,
			 std::list<exponent_type>& degrees);
  /*! \brief
    Assigns to \p *this the result of substituting the occurrences of
    variable \p var by \p new_num/\p new_denom in each polynomial
    \f$ p * (new_denom)^degree(p,var)\f$, where \p p is a polynomial
    in \p *this, and then taking the primitive form of the substituted
    polynomial.  The degrees degree(\p p, \p var) of the polynomials
    \p p belonging to \p *this in \p var are stored in \p degrees, and
    the maximum of these degrees is \p max_deg.

    The behaviour is undefined if \p degrees is not empty or if the
    substitution is not invertible.
  */
  void substitute_invertible_assign(const Variable var,
				    const Polynomial& new_num,
				    Coefficient_traits::const_reference new_den,
				    const exponent_type& max_deg,
				    const std::list<exponent_type>& degrees);

  /*! \brief
    Assigns to \p *this a polynomial space containing the join of \p
    *this and \p y using an adaptation of Karr's algorithm, which
    canonicalizes both \p *this and \p.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void join_assign_karr(const Polynomial_Space& y);

  /*! \brief
    Assigns to \p *this a polynomial space containing the join of \p
    *this and \p y using an algorithm that does not require that both
    \p *this and \p are in canonical form.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void join_assign_canonical_not_required(const Polynomial_Space& y);

private:
  //! TO BE WRITTEN.
  typedef std::map<Term, dimension_type, Term::Compare> Term_To_Dimension_Map;
  typedef std::vector<Term> Dimension_To_Term_Map;

#define PPL_IN_PC_CLASS
  //#include "PC_Status.idefs.hh"
#undef PPL_IN_PC_CLASS

  //! The status flags to keep track of the internal state.
  //  Status status;

#if !defined(__GNUC__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ > 3)
  friend std::ostream&
  Parma_Polyhedra_Library::
  IO_Operators::operator<<<>(std::ostream& s,
			     const Polynomial_Space<db>& c);
#else
  // This is too lax than wanted.
  template <typename U>
  friend std::ostream&
  Parma_Polyhedra_Library::
  IO_Operators::operator<<(std::ostream& s,
			   const Polynomial_Space<U>& c);
#endif

  //! \name Exception Throwers
  //@{
  template <Parma_Polyhedra_Library::degree_type other_db>
  void throw_dimension_incompatible(const char* method,
				    const Polynomial_Space<other_db>& x) const;

  void throw_dimension_incompatible(const char* method,
				    dimension_type required_dim) const;

  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Constraint& c) const;

  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Polynomial_Constraint& c) const;

  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Constraint_System& cs) const;

  void throw_dimension_incompatible(const char* method,
				    const char* c_name,
				    const Polynomial_Constraint_System& cs) const;

  void throw_dimension_incompatible(const char* method,
				    const Generator& g) const;

  void throw_dimension_incompatible(const char* method,
				    const char* name_row,
				    const Linear_Expression& y) const;

  void throw_dimension_incompatible(const char* method,
				    const char* name_row,
				    const Polynomial& p) const;


  static void throw_space_dimension_overflow(const char* method,
					     const char* reason);

  static void throw_constraint_incompatible(const char* method);

  static void throw_generic(const char* method, const char* reason);
  //@} // Exception Throwers
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polynomial_Space */
template <Parma_Polyhedra_Library::degree_type db>
void swap(Parma_Polyhedra_Library::Polynomial_Space<db>& x,
	  Parma_Polyhedra_Library::Polynomial_Space<db>& y);

} // namespace std

//#include "PC_Status.inlines.hh"
#include "Polynomial_Space.inlines.hh"
#include "Polynomial_Space.templates.hh"

#endif // !defined(PPL_Polynomial_Space_defs_hh)


