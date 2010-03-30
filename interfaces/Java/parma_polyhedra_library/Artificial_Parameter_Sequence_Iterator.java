/* PPL Java interface: Artificial_Parameter_Sequence_Iterator definition.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

package parma_polyhedra_library;

//! Iterator class for Artificial_Parameter_Sequence.
/*! \ingroup PPL_Java_interface
  Java class interfacing C++
  Parma_Polyhedra_Library::Artificial_Parameter_Sequence::iterator
*/
public class Artificial_Parameter_Sequence_Iterator extends PPL_Object {

  private Artificial_Parameter_Sequence_Iterator() {};

  private native void build_ppl_object(Artificial_Parameter_Sequence obj);

  public Artificial_Parameter_Sequence_Iterator
    (Artificial_Parameter_Sequence_Iterator y) {
      build_cpp_object(y);
  }

  private native void
      build_cpp_object(Artificial_Parameter_Sequence_Iterator y);

  public native boolean equals(Artificial_Parameter_Sequence_Iterator itr);

  public native void next();

  public native void prev();

  public native Artificial_Parameter get_artificial_parameter();

  public native void free();

  protected native void finalize();

}

