/* H79_Certificate class declaration.
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

#ifndef PPL_H79_Certificate_defs_hh
#define PPL_H79_Certificate_defs_hh 1

#include "H79_Certificate.types.hh"
#include "Polyhedron.types.hh"
#include "globals.hh"
#include <cassert>
#include <vector>

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A convergence certificate for the H79 widening operator.
/*!
  Convergence certificates are used to instantiate the BHZ03 framework
  so as to define widening operators for the finite powerset domain.
  \note
  The convergence of the H79 widening can also be certified by
  the BHRZ03_Certificate.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::H79_Certificate {
public:
  //! Default constructor.
  H79_Certificate();
  
  //! Constructor: computes the certificate for \p ph.
  H79_Certificate(const Polyhedron& ph);

  //! The comparison function for certificates.
  /*!
    \return
    \f$-1\f$, \f$0\f$ or \f$1\f$ depending on whether \p *this
    is smaller than, equal to, or greater than \p y, respectively.

    Compares \p *this with \p y, using a total ordering which is a
    refinement of the lgo relation for the H79 widening.
  */
  int compare(const H79_Certificate& y) const;

  //! Compares \p *this with the certificate for polyhedron \p ph.
  int compare(const Polyhedron& ph) const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if the certificate for
  //! polyhedron \p ph is stricly smaller than \p *this.
  bool is_stabilizing(const Polyhedron& ph) const;
  
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! A total ordering on H79 certificates.
  /*!
    This binary predicate defines a total ordering on H79 certificates
    which is used when storing information about sets of polyhedra.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  struct Compare {
    //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
    bool operator()(const H79_Certificate& x,
		    const H79_Certificate& y) const;
  };
  
  //! Check if gathered information is meaningful.
  bool OK() const;
  
private:
  //! Dimension of the polyhedron.
  dimension_type poly_dim;
  //! Cardinality of a non-redundant constraint system for the polyhedron.
  dimension_type num_constraints;
};

#include "H79_Certificate.inlines.hh"

#endif // !defined(PPL_H79_Certificate_defs_hh)
