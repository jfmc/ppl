/* Polynomial_Cone class declaration.
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

#ifndef PPL_Polynomial_Cone_defs_hh
#define PPL_Polynomial_Cone_defs_hh 1

#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Linear_Expression.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"
#include "Constraint_System.types.hh"
#include "Generator_System.types.hh"
#include "Polyhedron.types.hh"
#include "Poly_Con_Relation.types.hh"
#include "Poly_Gen_Relation.types.hh"
#include "Polynomial_Cone.types.hh"
#include "Polynomial.types.hh"
#include "Polynomial_Constraint.types.hh"
#include "Polynomial_Constraint_System.types.hh"
#include "Term.defs.hh"
#include <map>
#include <cstddef>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Polynomial_Cone
  Writes a textual representation of \p pc on \p s:
  <CODE>false</CODE> is written if \p pc is an empty polynomial cone;
  <CODE>true</CODE> is written if \p pc is the universe polynomial cone;
  a system of polynomial constraints defining \p pc is written otherwise,
  all constraints separated by ", ".
*/
template <typename PH, unsigned db>
std::ostream&
operator<<(std::ostream& s, const Polynomial_Cone<PH, db>& pc);

} // namespace IO_Operators

//! Returns <CODE>true</CODE> if and only if \p x and \p y are the same cone.
/*!
  \relates Polynomial_Cone
  Note that \p x and \p y may be dimension-incompatible cones:
  in this case, the value <CODE>false</CODE> is returned.
*/
template <typename PH, unsigned db>
bool operator==(const Polynomial_Cone<PH, db>& x, const Polynomial_Cone<PH, db>& y);

/*! \brief
  Returns <CODE>true</CODE> if and only if \p x and \p y are not the
  same cone.
*/
/*!
 \relates Polynomial_Cone
  Note that \p x and \p y may be dimension-incompatible cones:
  in this case, the value <CODE>true</CODE> is returned.
*/
template <typename PH, unsigned db>
bool operator!=(const Polynomial_Cone<PH, db>& x, const Polynomial_Cone<PH, db>& y);

} // namespace Parma_Polyhedra_Library

//! A (not-necessarily product-closed) polynomial cone.
/*! \ingroup PPL_CXX_interface
  FIXME: to be written.
*/
template <typename PH, unsigned db>
class Parma_Polyhedra_Library::Polynomial_Cone {
public:
  //! Returns the maximum space dimension that a polynomial cone can handle.
  static dimension_type max_space_dimension();

  //! \name Constructors, Assignment, Swap and Destructor
  //@{

  /*! \brief
    Builds a universe or empty polynomial cone of the specified space
    dimension.

    \param num_dimensions
    The number of dimensions of the vector space enclosing the BDS;

    \param kind
    Specifies whether the universe or the empty BDS has to be built.
  */
  explicit Polynomial_Cone(dimension_type num_dimensions = 0,
			   Degenerate_Element kind = UNIVERSE);

  //! Ordinary copy-constructor.
  Polynomial_Cone(const Polynomial_Cone& y);

  //! Builds a conservative, upward approximation of \p y.
  template <typename Other_PH, unsigned other_db>
  explicit Polynomial_Cone(const Polynomial_Cone<Other_PH, other_db>& y);

  //! Builds a polynomial cone from the system of constraints \p cs.
  /*!
    The polynomial cone inherits the space dimension of \p cs.

    \param cs
    A system of linear constraints.

    \exception std::invalid_argument
    Thrown if the system of constraints \p cs contains strict inequalities.
  */
  Polynomial_Cone(const Constraint_System& cs);

  //! Builds a polynomial cone from the system of constraints \p cs.
  /*!
    The polynomial cone inherits the space dimension of \p cs.

    \param pcs
    A system of polynomial constraints.

    \exception std::invalid_argument
    Thrown if the system of constraints \p cs contains strict inequalities.
  */
  Polynomial_Cone(const Polynomial_Constraint_System& pcs);

  //! Builds a polynomial cone from the system of generators \p gs.
  /*!
    Builds the smallest polynomial cone containing the polyhedron
    defined by \p gs.  The polynomial cone inherits the space
    dimension of \p gs.
  */
  Polynomial_Cone(const Generator_System& gs);

  //! Builds a polynomial cone from the polyhedron \p ph.
  /*!
    Builds a polynomial cone containing \p ph using algorithms whose complexity
    does not exceed the one specified by \p complexity.  If
    \p complexity is \p ANY_COMPLEXITY, then the polynomial cone built is the
    smallest one containing \p ph.
  */
  Polynomial_Cone(const Polyhedron& ph,
		  Complexity_Class complexity = ANY_COMPLEXITY);

  /*! \brief
    The assignment operator
    (\p *this and \p y can be dimension-incompatible).
  */
  Polynomial_Cone& operator=(const Polynomial_Cone& y);

  /*! \brief
    Swaps \p *this with \p y
    (\p *this and \p y can be dimension-incompatible).
  */
  void swap(Polynomial_Cone& y);

  //! Destructor.
  ~Polynomial_Cone();

  //@} Constructors, Assignment, Swap and Destructor

  //! \name Member Functions that Do Not Modify the Polynomial_Cone
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  /*! \brief
    Returns \f$0\f$, if \p *this is empty; otherwise, returns the
    \ref Affine_Independence_and_Affine_Dimension "affine dimension"
    of \p *this.
  */
  dimension_type affine_dimension() const;

  //! Returns a system of constraints defining \p *this.
  Polynomial_Constraint_System polynomial_constraints() const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool contains(const Polynomial_Cone& y) const;

  //! Returns <CODE>true</CODE> if and only if \p *this strictly contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool strictly_contains(const Polynomial_Cone& y) const;

  //! Returns the relations holding between \p *this and the constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible
    or if \p c is a strict inequality or if \p c is not a bounded
    difference constraint.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! Returns the relations holding between \p *this and the generator \p g.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! Returns <CODE>true</CODE> if and only if \p *this is an empty polynomial cone.
  bool is_empty() const;

  //! Returns <CODE>true</CODE> if and only if \p *this is a universe polynomial cone.
  bool is_universe() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this satisfies
    all its invariants.
  */
  bool OK() const;

  //@} Member Functions that Do Not Modify the Polynomial_Cone

  //! \name Space-Dimension Preserving Member Functions that May Modify the Polynomial_Cone
  //@{

  /*! \brief
    Adds a copy of constraint \p c to the system of polynomial constraints
    defining \p *this.

    \param c
    The constraint to be added.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible,
    or if \p c is a strict inequality.
  */
  void add_constraint(const Constraint& c);

  /*! \brief
    If the degree of \p c does not exceed \p db, adds a copy of \p c
    to the system of polynomial constraints defining \p *this.

    \param c
    The polynomial constraint to be added. If its degree exceeds the degree
    bound \p db, insertion will not take place.

    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible,
    or if \p c is a strict inequality.
  */
  void add_polynomial_constraint(const Polynomial_Constraint& c);

  /*! \brief
    Adds the constraints in \p cs to the system of polynomial constraints
    defining \p *this.

    \param  cs
    The constraints that will be added.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are dimension-incompatible,
    or if \p cs contains a strict inequality.
  */
  void add_constraints(const Constraint_System& cs);

  /*! \brief
    Adds the polynomial constraints in \p pcs to the system of polynomial
    constraints defining \p *this.

    \param  pcs
    The polynomial constraints that will be added.  Constraints exceeding
    the degree bound \p db will simply be ignored.

    \exception std::invalid_argument
    Thrown if \p *this and \p pcs are dimension-incompatible,
    or if \p pcs contains a strict inequality.
  */
  void add_polynomial_constraints(const Polynomial_Constraint_System& pcs);

  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void intersection_assign(const Polynomial_Cone& y);

  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const Polynomial_Cone& y);

  /*! \brief
    Assigns to \p *this the smallest polynomial cone containing the convex union
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void polynomial_cone_hull_assign(const Polynomial_Cone& y);

  /*! \brief
    Assigns to \p *this the smallest polynomial cone containing the convex union
    of \p *this and \p y.

    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool polynomial_cone_hull_assign_and_minimize(const Polynomial_Cone& y);

  //! Same as polynomial_cone_hull_assign.
  void upper_bound_assign(const Polynomial_Cone& y);

  /*! \brief
    If the bds-hull of \p *this and \p y is exact, it is assigned
    to \p *this and <CODE>true</CODE> is returned,
    otherwise <CODE>false</CODE> is returned.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  bool polynomial_cone_hull_assign_if_exact(const Polynomial_Cone& y);

  //! Same as polynomial_cone_hull_assign_if_exact.
  bool upper_bound_assign_if_exact(const Polynomial_Cone& y);

  /*! \brief
    Assigns to \p *this
    the \ref Convex_Polyhedral_Difference "poly-difference"
    of \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void polynomial_cone_difference_assign(const Polynomial_Cone& y);

  //! Same as polynomial_code_difference_assign.
  void difference_assign(const Polynomial_Cone& y);

  /*! \brief
    Assigns to \p *this the
    \ref Single_Update_Affine_Functions "affine image"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is assigned.

    \param expr
    The numerator of the affine expression.

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
    \ref Single_Update_Affine_Functions "affine preimage"
    of \p *this under the function mapping variable \p var into the
    affine expression specified by \p expr and \p denominator.

    \param var
    The variable to which the affine expression is substituted.

    \param expr
    The numerator of the affine expression.

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
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine transfer function.

    \param relsym
    The relation symbol.

    \param expr
    The numerator of the right hand side affine expression.

    \param denominator
    The denominator of the right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension
    of \p *this or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_image(Variable var,
				Relation_Symbol relsym,
				const Linear_Expression& expr,
				Coefficient_traits::const_reference denominator
				= Coefficient_one());

  /*! \brief
    Assigns to \p *this the image of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
    \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

    \param lhs
    The left hand side affine expression.

    \param relsym
    The relation symbol.

    \param rhs
    The right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with \p lhs or \p rhs
    or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_image(const Linear_Expression& lhs,
				Relation_Symbol relsym,
				const Linear_Expression& rhs);

  /*! \brief
    Assigns to \p *this the preimage of \p *this with respect to the
    \ref Generalized_Affine_Relations "affine relation"
    \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
    where \f$\mathord{\relsym}\f$ is the relation symbol encoded
    by \p relsym.

    \param var
    The left hand side variable of the generalized affine transfer function.

    \param relsym
    The relation symbol.

    \param expr
    The numerator of the right hand side affine expression.

    \param denominator
    The denominator of the right hand side affine expression.

    \exception std::invalid_argument
    Thrown if \p denominator is zero or if \p expr and \p *this
    are dimension-incompatible or if \p var is not a dimension
    of \p *this or if \p relsym is a strict relation symbol.
  */
  void generalized_affine_preimage(Variable var,
				   Relation_Symbol relsym,
				   const Linear_Expression& expr,
				   Coefficient_traits::const_reference
				   denominator = Coefficient_one());

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref Time_Elapse_Operator "time-elapse" between \p *this and \p y.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void time_elapse_assign(const Polynomial_Cone& y);

  /*! \brief
    Assigns to \p *this the result of computing the
    \ref polynomial_cone_widening "polynomial-cone-widening" of \p *this and \p y.

    \param y
    A polynomial cone that <EM>must</EM> be contained in \p *this.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are dimension-incompatible.
  */
  void widening_assign(const Polynomial_Cone& y, unsigned* tp = 0);

  /*! \brief
    Improves the result of the \ref polynomial_cone_widening "polynomial-cone-widening"
    computation by also enforcing those constraints in \p cs that are
    satisfied by all the points of \p *this.

    \param y
    A polynomial cone that <EM>must</EM> be contained in \p *this.

    \param pcs
    The system of constraints used to improve the widened polynomial cone.

    \param tp
    An optional pointer to an unsigned variable storing the number of
    available tokens (to be used when applying the
    \ref Widening_with_Tokens "widening with tokens" delay technique).

    \exception std::invalid_argument
    Thrown if \p *this, \p y and \p cs are dimension-incompatible or
    if \p cs contains a strict inequality.
  */
  void limited_extrapolation_assign(const Polynomial_Cone& y,
				    const Polynomial_Constraint_System& pcs,
				    unsigned* tp = 0);

  //@} Space-Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! Adds \p m new dimensions and embeds the old polynomial cone into the new space.
  /*!
    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the new
    polynomial cone, which is defined by a system of bounded differences in which the
    variables running through the new dimensions are unconstrained.
    For instance, when starting from the polynomial cone \f$\cB \sseq \Rset^2\f$
    and adding a third dimension, the result will be the polynomial cone
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  /*! \brief
    Adds \p m new dimensions to the polynomial cone and does not embed it in
    the new vector space.

    \param m
    The number of dimensions to add.

    The new dimensions will be those having the highest indexes in the
    new polynomial cone, which is defined by a system of bounded differences in
    which the variables running through the new dimensions are all
    constrained to be equal to 0.
    For instance, when starting from the polynomial cone \f$\cB \sseq \Rset^2\f$
    and adding a third dimension, the result will be the polynomial cone
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_project(dimension_type m);

  /*! \brief
    Seeing a polynomial cone as a set of tuples (its points),
    assigns to \p *this all the tuples that can be obtained by concatenating,
    in the order given, a tuple of \p *this with a tuple of \p y.

    Let \f$B \sseq \Rset^n\f$ and \f$D \sseq \Rset^m\f$ be the polynomial cones
    corresponding, on entry, to \p *this and \p y, respectively.
    Upon successful completion, \p *this will represent the polynomial cone
    \f$R \sseq \Rset^{n+m}\f$ such that
    \f[
      R \defeq
          \Bigl\{\,
            (x_1, \ldots, x_n, y_1, \ldots, y_m)^\transpose
          \Bigm|
            (x_1, \ldots, x_n)^\transpose \in B,
            (y_1, \ldots, y_m)^\transpose \in D
          \,\Bigl\}.
    \f]
    Another way of seeing it is as follows: first increases the space
    dimension of \p *this by adding \p y.space_dimension() new
    dimensions; then adds to the system of constraints of \p *this a
    renamed-apart version of the constraints of \p y.
  */
  void concatenate_assign(const Polynomial_Cone& y);

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

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Writes to \p s an ASCII representation of the internal
    encoding of \p *this.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Loads from \p s an ASCII representation (as produced by ascii_dump)
    and sets \p *this accordingly.  Returns <CODE>true</CODE> if successful,
    <CODE>false</CODE> otherwise.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  friend bool Parma_Polyhedra_Library::operator==<PH, db>(const Polynomial_Cone<PH, db>& x,
						     const Polynomial_Cone<PH, db>& y);

private:
  template <typename Other_PH, unsigned other_db>
  friend class Parma_Polyhedra_Library::Polynomial_Cone;

  //! The convex polyhedron used to represent the polynomial cone.
  PH ph;

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
  Parma_Polyhedra_Library::IO_Operators::operator<<<>(std::ostream& s,
						      const Polynomial_Cone<PH, db>& c);
#else
  // This is too lax than wanted.
  template <typename U>
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const Polynomial_Cone<U>& c);
#endif

  //! \name Exception Throwers
  //@{
  void throw_dimension_incompatible(const char* method,
				    const Polynomial_Cone& x) const;

  void throw_dimension_incompatible(const char* method,
				    dimension_type required_dim) const;

  void throw_dimension_incompatible(const char* method,
				    const Constraint& c) const;

  void throw_dimension_incompatible(const char* method,
				    const Generator& g) const;

  void throw_dimension_incompatible(const char* method,
				    const char* name_row,
				    const Linear_Expression& y) const;

  static void throw_constraint_incompatible(const char* method);

  static void throw_generic(const char* method, const char* reason);
  //@} // Exception Throwers
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polynomial_Cone */
template <typename PH, unsigned db>
void swap(Parma_Polyhedra_Library::Polynomial_Cone<PH, db>& x,
	  Parma_Polyhedra_Library::Polynomial_Cone<PH, db>& y);

} // namespace std

//#include "PC_Status.inlines.hh"
#include "Polynomial_Cone.inlines.hh"

#endif // !defined(PPL_Polynomial_Cone_defs_hh)
