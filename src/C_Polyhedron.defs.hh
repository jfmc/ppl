/* C_Polyhedron class declaration.
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

#ifndef PPL_C_Polyhedron_defs_hh
#define PPL_C_Polyhedron_defs_hh 1

#include "C_Polyhedron.types.hh"
#include "NNC_Polyhedron.types.hh"
#include "Polyhedron.defs.hh"

//! A closed convex polyhedron.
/*!
    An object of the class C_Polyhedron represents a
    <EM>topologically closed</EM> convex polyhedron
    in the vector space \f$\Rset^n\f$.

    When building a closed polyhedron starting from
    a system of constraints, an exception is thrown if the system
    contains a <EM>strict inequality</EM> constraint.
    Similarly, an exception is thrown when building a closed polyhedron
    starting from a system of generators containing a <EM>closure point</EM>.

    \note
    Such an exception will be obtained even if the system of
    constraints (resp., generators) actually defines
    a topologically closed subset of the vector space, i.e.,
    even if all the strict inequalities (resp., closure points)
    in the system happen to be redundant with respect to the
    system obtained by removing all the strict inequality constraints
    (resp., all the closure points).
    In contrast, when building a closed polyhedron starting from
    an object of the class NNC_Polyhedron,
    the precise topological closure test will be performed.
*/

class Parma_Polyhedra_Library::C_Polyhedron : public Polyhedron {
public:
  //! Builds either the universe or the empty C polyhedron.
  /*!
    \param num_dimensions   The number of dimensions of the vector
                            space enclosing the C polyhedron.
    \param kind             Specifies whether a universe or an empty
                            C polyhedron should be built. 

    Both parameters are optional:
    by default, a 0-dimension space universe C polyhedron is built.
  */
  explicit C_Polyhedron(dimension_type num_dimensions = 0,
			Degenerate_Kind kind = UNIVERSE);

  //! Builds a C polyhedron from a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.
    \param cs       The system of constraints defining the polyhedron.
    \exception std::invalid_argument thrown if the system of constraints
                                     contains strict inequalities.
  */
  C_Polyhedron(const ConSys& cs);

  //! Builds a C polyhedron recycling a system of constraints.
  /*!
    The polyhedron inherits the space dimension of the constraint system.
    \param cs       The system of constraints defining the polyhedron.
                    It is not declared <CODE>const</CODE> because its
                    data-structures will be recycled to build the polyhedron.
    \exception std::invalid_argument thrown if the system of constraints
                                     contains strict inequalities.
  */
  C_Polyhedron(ConSys& cs);

  //! Builds a C polyhedron from a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.
    \param gs       The system of generators defining the polyhedron.
    \exception std::invalid_argument thrown if the system of generators
                                     is not empty but has no points,
                                     or if it contains closure points.
  */
  C_Polyhedron(const GenSys& gs);

  //! Builds a C polyhedron recycling a system of generators.
  /*!
    The polyhedron inherits the space dimension of the generator system.
    \param gs       The system of generators defining the polyhedron.
                    It is not declared <CODE>const</CODE> because its
                    data-structures will be recycled to build the polyhedron.
    \exception std::invalid_argument thrown if the system of generators
                                     is not empty but has no points,
                                     or if it contains closure points.
  */
  C_Polyhedron(GenSys& gs);

  //! Builds a C polyhedron from the NNC polyhedron \p y.
  /*!
    \exception std::invalid_argument thrown if the polyhedron \p y
                                     is not topologically closed.
  */
  explicit C_Polyhedron(const NNC_Polyhedron& y);

  //! Builds a C polyhedron out of a generic, interval-based bounding box.
  /*!
    For a description of the methods that should be provided by
    the template class Box, see the documentation of the protected method:
      template <typename Box>
      Polyhedron::Polyhedron(Topology topol, const Box& box);
    \param box    The bounding box representing the polyhedron to be built.
    \param dummy  A dummy tag to syntactically differentiate this one
                  from the other constructors.
    \exception std::invalid_argument thrown if \p box has intervals that
                                     are not topologically closed (i.e.,
                                     having some finite but open bounds).
  */
  template <typename Box>
  C_Polyhedron(const Box& box, From_Bounding_Box dummy);

  //! Ordinary copy-constructor.
  C_Polyhedron(const C_Polyhedron& y);

  //! \brief
  //! The assignment operator.
  //! (\p *this and \p y can be dimension-incompatible.)
  C_Polyhedron& operator=(const C_Polyhedron& y);

  //! Destructor.
  ~C_Polyhedron();
};

#include "C_Polyhedron.inlines.hh"

#endif // !defined(PPL_C_Polyhedron_defs_hh)
