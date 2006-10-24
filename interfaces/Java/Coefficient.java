/* Coefficient Java class declaration and implementation.
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

import java.math.BigInteger;

//! An alias for easily naming the type of PPL coefficients.
/*! \ingroup PPL_JAVA_interface
  Objects of type Coefficient are used to implement the integral valued
  coefficients occurring in linear expressions, constraints, generators,
  intervals, bounding boxes and so on.
*/
public class Coefficient {
    //! Holds the value of \p this.
    private BigInteger integer;

    //! Builds a coefficient built from an integer.
    public Coefficient(int value) {
	integer = new BigInteger(Integer.toString(value));
    }

    //! Builds a coefficient built from a long.
    public Coefficient(long value) {
	integer = new BigInteger(Long.toString(value));
    }

    //! Builds a coefficient built from a BigInteger.
    public Coefficient(BigInteger b_int) {
	integer = b_int;
    }

    //! Builds a coefficient built from a string that encodes its value.
    /*!
      \exception java.lang.NumberFormatException
      Thrown if val is not a valid representation of a BigInteger.
    */
    public Coefficient(String val) {
	integer = new BigInteger(val);
    }

    //! Builds a coefficient built from another coefficient.
    public Coefficient(Coefficient c) {
	integer = new BigInteger(c.integer.toString());
    }

    //| Returns the value held by \p this.
    public BigInteger getBigInteger() {
	return integer;
    }
}
