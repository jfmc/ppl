/* Relation enumerations declaration.
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

#ifndef _relations_hh
#define _relations_hh 1

#include <iostream>

namespace Parma_Polyhedra_Library {

  //! Possible relations between a polyhedron and a constraint.
  enum Relation_Poly_Con {
    //! The polyhedron and the set of points satisfying
    //! the constraint are disjoint.
    IS_DISJOINT,
    //! The polyhedron intersects the set of points satisfying
    //! the constraint, but it is not included in it.
    STRICTLY_INTERSECTS,
    //! The polyhedron is included in the set of points satisfying
    //! the constraint.
    IS_INCLUDED,
    //! The polyhedron is included in the set of points saturating
    //! the constraint.
    SATURATES
  };

  //! Output operator for Relation_Poly_Con.
  std::ostream& operator <<(std::ostream& s, Relation_Poly_Con r);

  //! Possible relations between a polyhedron and a generator.
  enum Relation_Poly_Gen {
    //! Adding the generator would not change the polyhedron.
    SUBSUMES,
    //! Adding the generator would change the polyhedron.
    DOES_NOT_SUBSUME
  };

  //! Output operator for Relation_Poly_Gen.
  std::ostream& operator <<(std::ostream& s, Relation_Poly_Gen r);

} // namespace Parma_Polyhedra_Library

#endif
