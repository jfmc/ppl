/* Polyhedron class declaration.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Polyhedron_defs_hh
#define _Polyhedron_defs_hh 1

#include "Polyhedron.types.hh"
#include "NNC_Polyhedron.types.hh"
#include "PolyBase.defs.hh"

//! A necessarily closed convex polyhedron.
/*!
    An object of the class Polyhedron represents a
    <EM>necessarily closed</EM> convex polyhedron
    in the vector space \f$\Rset^n\f$.

    When building a necessarily closed polyhedron starting from
    a system of constraints, an exception is thrown if the system
    contains a <EM>strict inequality</EM> constraint.
    Similarly, an exception is thrown when building a necessarily closed
    polyhedron starting from a system of generators containing a
    <EM>closure point</EM>.

    \note
    Such an exception will be obtained even if the system of
    constraints (resp., generators) actually defines
    a topologically closed subset of the vector space, i.e.,
    even if all the strict inequalities (resp., closure points)
    in the system happen to be redundant with respect to the
    system obtained by removing all the strict inequality constraints
    (resp., all the closure points).
    In contrast, when building a necessarily closed polyhedron
    starting from an object of the class NNC_Polyhedron,
    the precise topological closure test will be performed.
*/

class Parma_Polyhedra_Library::Polyhedron : public PolyBase {
public:

  //! Builds either the universe or the empty polyhedron of dimension
  //! \p num_dimensions. Both parameters are optional:
  //! by default, a 0-dimension space universe polyhedron is built.
  explicit Polyhedron(size_t num_dimensions = 0,
		      Degenerate_Kind kind = UNIVERSE);

  //! Builds a polyhedron from a system of constraints.
  //! The polyhedron inherits the space dimension of the constraint system.
  //! \param cs       The system of constraints defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if the system of constraints
  //!                                  contains strict inequalities.
  Polyhedron(ConSys& cs);

  //! Builds a polyhedron from a system of generators.
  //! The polyhedron inherits the space dimension of the generator system.
  //! \param gs       The system of generators defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if the system of generators
  //!                                  is not empty but has no points,
  //!                                  or if it contains closure points.
  Polyhedron(GenSys& gs);

  //! Ordinary copy-constructor.
  Polyhedron(const Polyhedron& y);

  //! Builds a necessarily closed polyhedron from the NNC_Polyhedron \p y.
  //! \exception std::invalid_argument thrown if the polyhedron \p y
  //!                                  is not topologically closed.
  explicit Polyhedron(const NNC_Polyhedron& y);

  //! The assignment operator.
  //! (Note that \p *this and \p y can be dimension-incompatible.)
  Polyhedron& operator=(const Polyhedron& y);

  // Destructor
  ~Polyhedron();
};

#include "Polyhedron.inlines.hh"

#endif
