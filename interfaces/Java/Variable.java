/* Variable Java class declaration and implementation.
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

import javax.management.RuntimeErrorException;

public class Variable {
    //! The index of the Cartesian axis.
    private int varid;

    //! Builds the variable corresponding to the Cartesian axis of index \p i.
    /*!
      \exception RuntimeErrorException
      Thrown if <CODE>i</CODE> is has negative value.
    */
    public Variable(int i) {
	if (i < 0)
	    throw new RuntimeErrorException(new Error("ppl_java.Variable::"
						 + "Variable:\n"
						 + "an index variable can not"
						 + " be negative."));
	varid = i;
    }

    //! Returns the index of the Cartesian axis associated to the variable.
    public int id() {
	return varid;
    }
}
