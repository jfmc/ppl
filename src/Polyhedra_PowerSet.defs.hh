/* Polyhedra_PowerSet class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Polyhedra_PowerSet_defs_hh
#define PPL_Polyhedra_PowerSet_defs_hh

#include "Polyhedra_PowerSet.types.hh"
#include "BHRZ03_Certificate.types.hh"
#include "ConSys.types.hh"
#include "Constraint.types.hh"
#include "Polyhedron.defs.hh"
#include "Variable.defs.hh"
#include "Determinate.defs.hh"
#include "PowerSet.defs.hh"
#include "globals.hh"
#include <iosfwd>
#include <list>
#include <set>
#include <map>

//! The powerset construction instantiated on PPL polyhedra.
template <typename PH>
class Parma_Polyhedra_Library::Polyhedra_PowerSet
  : public Parma_Polyhedra_Library::PowerSet
<Parma_Polyhedra_Library::Determinate<PH> > {
public:
  //! Returns the maximum space dimension a Polyhedra_Powerset<PH> can handle.
  static dimension_type max_space_dimension();

  //! \name Constructors
  //@{

  //! Builds a universe (top) or empty (bottom) Polyhedra_PowerSet.
  /*!
    \param num_dimensions
    The number of dimensions of the vector space enclosing the powerset;

    \param kind
    Specifies whether the universe or the empty powerset has to be built.
  */
  explicit
  Polyhedra_PowerSet(dimension_type num_dimensions = 0,
		     Polyhedron::Degenerate_Kind kind = Polyhedron::UNIVERSE);

  //! Ordinary copy-constructor.
  Polyhedra_PowerSet(const Polyhedra_PowerSet& y);

  //! Creates a Polyhedra_PowerSet with the same information contents as \p cs.
  Polyhedra_PowerSet(const ConSys& cs);

  //@} // Constructors and Destructor

  //! \name Member Functions that Do Not Modify the Powerset of Polyhedra
  //@{

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this semantically 
  //! (i.e., geometrically) contains \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.

    \warning
    This may be <EM>really</EM> expensive!
  */
  bool semantically_contains(const Polyhedra_PowerSet& y) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is semantically 
  //! (i.e., geometrically) equal to \p y.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.

    \warning
    This may be <EM>really</EM> expensive!
  */
  bool semantically_equals(const Polyhedra_PowerSet& y) const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //@} // Member Functions that Do Not Modify the Powerset

  //! \name Space-Dimension Preserving Member Functions that May Modify the Powerset of Polyhedra
  //@{

  //! Intersects \p *this with constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are topology-incompatible
    or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Intersects \p *this with the constraint \p c, minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \exception std::invalid_argument
    Thrown if \p *this and \p c are topology-incompatible or
    dimension-incompatible.
  */
  bool add_constraint_and_minimize(const Constraint& c);

  //! Intersects \p *this with the constraints in \p cs.
  /*!
    \param cs
    The constraints to intersect with.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  void add_constraints(const ConSys& cs);

  //! \brief
  //! Intersects \p *this with the constraints in \p cs,
  //! minimizing the result.
  /*!
    \return
    <CODE>false</CODE> if and only if the result is empty.

    \param cs
    The constraints to intersect with.

    \exception std::invalid_argument
    Thrown if \p *this and \p cs are topology-incompatible or
    dimension-incompatible.
  */
  bool add_constraints_and_minimize(const ConSys& cs);

  //! \brief
  //! Assign to \p *this the result of (recursively) merging together
  //! the pairs of polyhedra whose poly-hull is the same as their
  //! set-theoretical union.
  /*!
    On exit, for all the pairs \f$\cP\f$, \f$\cQ\f$ of different polyhedra
    in \p *this, we have \f$\cP \uplus \cQ \neq \cP \union \cQ\f$.
  */
  void pairwise_reduce();

  //! \brief
  //! Assigns to \p *this the result of applying the BGP99 extrapolation
  //! operator to \p *this and \p y, using the widening function \p wf
  //! and the cardinality threshold \p max_disjuncts.
  /*!
    \param y
    A finite powerset of polyhedra.
    It <EM>must</EM> definitely entail \p *this;

    \param wf
    The widening function to be used on polyhedra objects. It is obtained
    from the corresponding widening method by using the helper function
    Parma_Polyhedra_Library::widen_fun. Legal values are, e.g.,
    <CODE>widen_fun(&Polyhedron::H79_widening_assign)</CODE> and
    <CODE>widen_fun(&Polyhedron::limited_H79_extrapolation_assign, cs)</CODE>;

    \param max_disjuncts
    The maximum number of disjuncts occurring in the powerset \p *this
    <EM>before</EM> starting the computation. If this number is exceeded,
    some of the disjuncts in \p *this are collapsed (i.e., joined together).

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.

    For a description of the extrapolation operator,
    see \ref BGP99 "[BGP99]" and \ref BHZ03b "[BHZ03b]".
  */
  template <typename Widening>
  void BGP99_extrapolation_assign(const Polyhedra_PowerSet& y,
				  Widening wf,
				  unsigned max_disjuncts);

  //! \brief
  //! Assigns to \p *this the result of computing the BHZ03-widening
  //! between \p *this and \p y, using the widening function \p wf
  //! certified by the convergence certificate \p Cert.
  /*!
    \param y
    The finite powerset of polyhedra computed in the previous iteration step.
    It <EM>must</EM> definitely entail \p *this;

    \param wf
    The widening function to be used on polyhedra objects.
    It is obtained from the corresponding widening method by using
    the helper function widen_fun. Legal values are, e.g.,
    <CODE>widen_fun(&Polyhedron::H79_widening_assign)</CODE> and
    <CODE>widen_fun(&Polyhedron::limited_H79_extrapolation_assign, cs)</CODE>.

    \exception std::invalid_argument
    Thrown if \p *this and \p y are topology-incompatible or
    dimension-incompatible.

    The BHZ03 widening framework is instantiated using two extrapolation
    heuristics: first, the least upper bound is tried; second, the BGP99
    extrapolation operator is tried, possibly applying pairwise reduction.
    If both heuristics fail to converge according to the certificate \p Cert,
    then an attempt is made to apply the base-level widening \p wf to
    the poly-hulls of \p *this and \p y, possibly improving the result
    using Polyhedron::poly_difference_assign. For more details and
    a justification of the overall approach, see \ref BHZ03b "[BHZ03b]"
    and \ref BHZ04 "[BHZ04]".

    \warning
    In order to obtain a proper widening operator, the template parameter
    \p Cert should be a finite convergence certificate for the base-level
    widening function \p wf; otherwise, an extrapolation operator is
    obtained.
    For a description of the methods that should be provided
    by \p Cert, see BHRZ03_Certificate or H79_Certificate.
  */
  template <typename Cert, typename Widening>
  void BHZ03_widening_assign(const Polyhedra_PowerSet& y, Widening wf);

  //! \brief
  //! An instance of the BHZ03 framework using the widening function \p wf
  //! certified by BHRZ03_Certificate.
  template <typename Widening>
  void BHZ03_widening_assign(const Polyhedra_PowerSet& y, Widening wf);

  //@} // Space-Dimension Preserving Member Functions that May Modify [...]

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! \brief
  //! The assignment operator
  //! (\p *this and \p y can be dimension-incompatible).
  Polyhedra_PowerSet& operator=(const Polyhedra_PowerSet& y);

  //! Swaps \p *this with \p y.
  void swap(Polyhedra_PowerSet& y);

  //! \brief
  //! Adds \p m new dimensions and embeds the old polyhedron
  //! in the new space.
  void add_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new dimensions to the polyhedron
  //! and does not embed it in the new space.
  void add_dimensions_and_project(dimension_type m);

  //! Assigns to \p *this the concatenation of \p *this and \p y.
  /*!
    Seeing a powerset as a set of tuples, this method assigns to
    \p *this all the tuples that can be obtained by concatenating,
    in the order given, a tuple of \p *this with a tuple of \p y.

    Intuitively, the result is obtained by computing the pair-wise
    \ref concatenate "concatenation" of each polyhedron in \p *this
    with each polyhedron in \p y.
  */
  void concatenate_assign(const Polyhedra_PowerSet& y);

  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed
    The set of Variable objects corresponding to the dimensions to be removed.

    \exception std::invalid_argument
    Thrown if \p *this is dimension-incompatible with one of the
    Variable objects contained in \p to_be_removed.
  */
  void remove_dimensions(const Variables_Set& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument
    Thrown if \p new_dimensions is greater than the space dimension
    of \p *this.
  */
  void remove_higher_dimensions(dimension_type new_dimension);

  //! \brief
  //! Remaps the dimensions of the vector space according to
  //! a partial function.
  /*!
    See also Polyhedron::map_dimensions.
  */
  template <typename PartialFunction>
  void map_dimensions(const PartialFunction& pfunc);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

private:
  typedef Determinate<PH> CS;
  typedef PowerSet<CS> Base;

public:
  typedef typename Base::Sequence Sequence;

  typedef typename Sequence::size_type size_type;

  typedef typename Base::iterator iterator;
  typedef typename Base::const_iterator const_iterator;
  typedef typename Base::reverse_iterator reverse_iterator;
  typedef typename Base::const_reverse_iterator const_reverse_iterator;
  typedef typename Base::value_type value_type;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

private:
  //! The number of dimensions of the enclosing vector space.
  dimension_type space_dim;

  //! \brief
  //! Assigns to \p *this the result of applying the BGP99 heuristics
  //! to \p *this and \p y, using the widening function \p wf.
  template <typename Widening>
  void BGP99_heuristics_assign(const Polyhedra_PowerSet& y, Widening wf);

  //! Records in \p cert_ms the certificates for this set of polyhedra.
  template <typename Cert>
  void collect_certificates(std::map<Cert, size_type,
		                     typename Cert::Compare>& cert_ms) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if the current set of polyhedra
  //! is stabilizing with respect to the multiset of certificates \p y_cert_ms.
  template <typename Cert>
  bool is_cert_multiset_stabilizing(const std::map<Cert, size_type,
                                                   typename Cert::Compare>&
				    y_cert_ms) const;
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Polyhedra_PowerSet */
template <typename PH>
void swap(Parma_Polyhedra_Library::Polyhedra_PowerSet<PH>& x,
	  Parma_Polyhedra_Library::Polyhedra_PowerSet<PH>& y);

} // namespace std

#include "Polyhedra_PowerSet.inlines.hh"

#endif // !defined(PPL_Polyhedra_PowerSet_defs_hh)
