/* BHRZ03_Certificate class declaration.
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

#ifndef PPL_BHRZ03_Certificate_defs_hh
#define PPL_BHRZ03_Certificate_defs_hh 1

#include "BHRZ03_Certificate.types.hh"
#include "Polyhedron.types.hh"
#include "globals.hh"
#include <cassert>
#include <vector>

class Parma_Polyhedra_Library::BHRZ03_Certificate {
public:
  //! Default constructor.
  BHRZ03_Certificate();
  
  //! Constructor: computes the certificate for \p ph.
  BHRZ03_Certificate(const Polyhedron& ph);

  //! The comparison function for certificates.
  /*!
    \return     The returned value can be \f$-1\f$, \f$0\f$ or \f$1\f$.

    Compares \p *this with \p y, using a total ordering which is a
    refinement of the lgo relation defined in BHRZ03.
    The result is negative, zero, or positive if \p *this is smaller
    than, equal to, or greater than y, respectively.
  */
  int compare(const BHRZ03_Certificate& y) const;

  //! Compares \p *this with the certificate for polyhedron \p ph.
  int compare(const Polyhedron& ph) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if the certificate for
  //! polyhedron \p ph is stricly smaller than \p *this.
  bool is_stabilizing(const Polyhedron& ph) const;
  
  //! A total ordering on BHRZ03 certificates.
  /*!
    This binary predicate defines a total ordering on BHRZ03 certificates
    which is a refinement of the BHRZ03 limited growth order relation.
    The total order is used when storing information about sets of polyhedra.
  */
  struct Compare {
    //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
    bool operator()(const BHRZ03_Certificate& x,
		    const BHRZ03_Certificate& y) const;
  };
  
  //! Check if gathered information is meaningful.
  bool OK() const;
  
private:
  //! Dimension of the polyhedron.
  dimension_type poly_dim;
  //! Dimension of the lineality space of the polyhedron.
  dimension_type lin_space_dim;
  //! Cardinality of a non-redundant constraint system for the polyhedron.
  dimension_type num_constraints;
  //! \brief
  //! Number of non-redundant points in a generator system
  //! for the polyhedron.
  dimension_type num_points;
  //! \brief
  //! A vector containing, for each index `0 <= i < space_dim',
  //! the number of non-redundant rays in a generator system of the
  //! polyhedron having exactly `i' null coordinates.
  std::vector<dimension_type> num_rays_null_coord;
};

#include "BHRZ03_Certificate.inlines.hh"

#endif // !defined(PPL_BHRZ03_Certificate_defs_hh)
