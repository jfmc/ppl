/* NNC_Polyhedron class declaration.
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

#ifndef _NNC_Polyhedron_defs_hh
#define _NNC_Polyhedron_defs_hh 1

#include "C_Polyhedron.types.hh"
#include "NNC_Polyhedron.types.hh"
#include "Polyhedron.defs.hh"

//! A not necessarily closed convex polyhedron.
/*!
    An object of the class NNC_Polyhedron represents a
    <EM>not necessarily closed</EM> (NNC) convex polyhedron
    in the vector space \f$\Rset^n\f$.

    \note
    Since NNC polyhedra are a generalization of closed polyhedra,
    any object of the class C_Polyhedron can be (explicitly) converted
    into an object of the class NNC_Polyhedron.
    The reason for defining two different classes is that objects of
    the class C_Polyhedron are characterized by a more efficient
    implementation, requiring less time and memory resources.
*/
class Parma_Polyhedra_Library::NNC_Polyhedron : public Polyhedron {
public:
  //! Builds either the universe or the empty NNC polyhedron of dimension
  //! \p num_dimensions. Both parameters are optional:
  //! by default, a 0-dimension space universe polyhedron is built.
  explicit NNC_Polyhedron(size_t num_dimensions = 0,
			  Degenerate_Kind kind = UNIVERSE);

  //! Builds a NNC polyhedron from a system of constraints.
  //! The polyhedron inherits the space dimension of the constraint system.
  //! \param cs       The system of constraints defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  NNC_Polyhedron(ConSys& cs);

  //! Builds a NNC polyhedron from a system of generators.
  //! The polyhedron inherits the space dimension of the generator system.
  //! \param gs       The system of generators defining the polyhedron.
  //!                 It is not declared <CODE>const</CODE>
  //!                 because it can be modified.
  //! \exception std::invalid_argument thrown if the system of generators
  //!                                  is not empty but has no points.
  NNC_Polyhedron(GenSys& gs);

  // FIXME: adapt the documentation of the corresponding constructor
  // of Polyhedron, once it has been peer reviewed.
  // Notice that a topological clash is impossible here.
  template <class Box>
  NNC_Polyhedron(const Box& box, From_Bounding_Box);

  //! Ordinary copy-constructor.
  NNC_Polyhedron(const NNC_Polyhedron& y);

  //! Builds a NNC polyhedron from the C_Polyhedron \p y.
  explicit NNC_Polyhedron(const C_Polyhedron& y);

  //! The assignment operator.
  //! (Note that \p *this and \p y can be dimension-incompatible.)
  NNC_Polyhedron& operator=(const NNC_Polyhedron& y);

  //! Destructor
  ~NNC_Polyhedron();

};

#include "NNC_Polyhedron.inlines.hh"

#endif // _NNC_Polyhedron_defs_hh
