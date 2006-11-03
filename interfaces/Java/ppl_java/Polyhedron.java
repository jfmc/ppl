/* Polyhedron Java class declaration and implementation.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

package ppl_java;


//! A closed convex polyhedron.
/*! \ingroup PPL_Java_interface
  An object of the class Polyhedron represents a convex polyhedron
  in the vector space \f$\Rset^n\f$.
*/
public class Polyhedron extends PPL_Object {

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is
      an empty polyhedron.
    */
    protected native boolean is_empty();
    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a universe polyhedron.
    */
    protected native boolean is_universe();

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a topologically closed subset of the vector space.
    */
    protected native boolean is_topologically_closed();

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a bounded polyhedron.
    */
    protected native boolean is_bounded();

    //! Returns <CODE>true</CODE> if and only if \p this and \p y are disjoint.
    /*!
      \exception RuntimeErrorException
      Thrown if \p x and \p y are topology-incompatible or
      dimension-incompatible.
    */
    protected native boolean is_disjoint_from(Polyhedron y);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p expr is
      bounded from above in \p this.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.
    */

    protected native boolean bounds_from_above(Linear_Expression expr);
    /*! \brief
      Returns <CODE>true</CODE> if and only if \p expr is
      bounded from below in \p this.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.
    */

    protected native boolean bounds_from_below(Linear_Expression expr);
    //! Returns <CODE>true</CODE> if and only if \p this contains \p y.
    /*!
      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */

    protected native boolean contains(Polyhedron p);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      strictly contains \p y.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    protected native boolean strictly_contains(Polyhedron p);

}
