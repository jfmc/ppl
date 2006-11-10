/* Congruence_Linear_Expression Java class declaration and implementation.
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

//! A Congruence built from a constraint.
/*! \ingroup PPL_Java_interface
 */

public class Congruence_Constraint extends Congruence {
    //! The object representing the constraint.
    private Constraint constr;

	//! Returns a congruence from \p c, with \p m as the modulus.
	public Congruence_Constraint(Constraint c, Coefficient mod) {
	this.modulus = new Coefficient(mod);
	constr = c;
    }
}
