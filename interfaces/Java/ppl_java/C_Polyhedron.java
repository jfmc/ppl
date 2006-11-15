/* C_Polyhedron Java class declaration and implementation.
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
public class C_Polyhedron extends Polyhedron {

    //! Builds either the universe or the empty C polyhedron.
    /*!
      \param num_dimensions
      The number of dimensions of the vector space enclosing the C polyhedron;

      \param kind
      Specifies whether a universe or an empty C polyhedron should be built.

      \exception RunTimeException
      Thrown if \p num_dimensions exceeds the maximum allowed space dimension.
    */
    public C_Polyhedron(long num_dimensions,
			Degenerate_Element kind) {
	build_cpp_object(num_dimensions, kind);
    }

    //! Builds a C polyhedron from a system of constraints.
    /*!
      The polyhedron inherits the space dimension of the constraint system.

      \param cs
      The system of constraints defining the polyhedron.

      \exception RuntimeErrorException
      Thrown if the system of constraints contains strict inequalities.
    */
    public C_Polyhedron(Constraint_System cs) {
	build_cpp_object(cs);
    }

    //! Builds a C polyhedron from a system of generators.
    /*!
      The polyhedron inherits the space dimension of the generator system.

      \param gs
      The system of generators defining the polyhedron.

      \exception RuntimeErrorException
      Thrown if the system of generators is not empty but has no points,
      or if it contains closure points.
    */
   public C_Polyhedron(Generator_System gs) {
	build_cpp_object(gs);
    }

    //! Builds a C polyhedron from a system of grid generators.
    /*!
      The polyhedron inherits the space dimension of the generator system.

      \param ggs
      The system of grid generators defining the polyhedron.

      FIXME: is the following correct?
      \exception RuntimeErrorException
      Thrown if the system of generators is not empty but has no points,
      or if it contains closure points.
    */
    public C_Polyhedron(Grid_Generator_System ggs) {
	build_cpp_object(ggs);
    }

    //! Builds a C polyhedron from a system of congruences.
    /*!
      The polyhedron inherits the space dimension of the congruence system.

      \param cgs
      The system of congruences defining the polyhedron.  It is not
      declared <CODE>const</CODE> because its data-structures may be
      recycled to build the polyhedron.
    */
    public C_Polyhedron(Congruence_System cgs) {
	build_cpp_object(cgs);
    }

    //! Ordinary copy-constructor.
    public C_Polyhedron(C_Polyhedron y) {
        build_cpp_object(y);
    }
    /*! \brief
      If the poly-hull of \p this and \p y is exact it is assigned
      to \p this and <CODE>true</CODE> is returned,
      otherwise <CODE>false</CODE> is returned.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are dimension-incompatible.
    */
    public native boolean poly_hull_assign_if_exact(C_Polyhedron y);

    //! Same as poly_hull_assign_if_exact(y).
    public native boolean upper_bound_assign_if_exact(C_Polyhedron y);

    //! Destroys the underlined C++ object.
    protected native void finalize();

    //! Builds the underlined C++ object.
    private native void build_cpp_object(Constraint_System cs);

    //! Builds the underlined C++ object.
    private native void build_cpp_object(long num_dimensions,
                                         Degenerate_Element kind);

    //! Builds the underlined C++ object.
    private native void build_cpp_object(Generator_System gs);

    //! Builds the underlined C++ object.
    private native void build_cpp_object(Grid_Generator_System ggs);

    //! Builds the underlined C++ object.
    private native void build_cpp_object(Congruence_System cgs);
    
       //! Builds the underlined C++ object.
    private native void build_cpp_object(C_Polyhedron y);
}
