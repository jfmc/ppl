/* Test C_Polyhedron Java class of the Parma Polyhedra Library Java interface.
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

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Vector;
import ppl_java.*;

public class C_Polyhedron_test1 {
  static {
	System.loadLibrary("ppl_java");
    }

    public static void main(String[] args) {
	// Variable declarations.
	Variable A = new Variable(0);
	Variable B = new Variable(1);
	Variable C = new Variable(2);

	// Coefficient declaration.
	Coefficient coeff1 = new Coefficient("5");

	// Linear_Expression declarations.
	Linear_Expression le1 = new Linear_Expression_Variable(A);
	Linear_Expression le2 = new Linear_Expression_Variable(C);
	Linear_Expression le3 = new Linear_Expression_Coefficient(coeff1);
	Linear_Expression le4 = le1.times(new Coefficient(156));
	le2 = le2.sum(le4);

	// Constraint declarations.
	Constraint c1 = new Constraint(le2, le3, Relation_Symbol.EQUAL);
	Constraint c2 = new Constraint(le2, le3, Relation_Symbol.EQUAL);
	Constraint c3 = new Constraint(le2, le4,
				       Relation_Symbol.GREATER_THAN_OR_EQUAL);
	Constraint c4 = new Constraint(le2, le3,
				       Relation_Symbol.GREATER_THAN_OR_EQUAL);

	// Constraint_System declarations.
	Constraint_System cs1 = new Constraint_System();
	cs1.add(c1);
	cs1.add(c2);
	Constraint_System cs2 = new Constraint_System();
	cs2.add(c3);
	cs2.add(c4);

	le2 = le2.times(new Coefficient("10"));

	// C_Polyhedron declarations
	C_Polyhedron c_poly1 = new C_Polyhedron(cs1);
	C_Polyhedron c_poly2 = new C_Polyhedron(cs2);

	// Generator declarations.
	Generator g1 = Generator.point(le2, new Coefficient(1));
	Generator g2 = Generator.point(le2, new Coefficient(2));
        Generator g3 = Generator.point(le2, new Coefficient(3));
	Generator g4 = Generator.point(le1, new Coefficient(4));

	// Generator_System declaration.
	Generator_System gs = new Generator_System();
	gs.add(g1);
	gs.add(g2);
	gs.add(g3);
	gs.add(g4);
	C_Polyhedron poly3 = new C_Polyhedron(gs);

	// Here some tests.
	if(c_poly1.is_empty())
	    System.out.println("c_poly1 is empty..");
	else
	    System.out.println("c_poly1 is not empty.");
   	if(c_poly1.is_universe())
	    System.out.println("c_poly1 is universe.");
	else
	    System.out.println("c_poly1 is not universe.");
	if(c_poly1.is_topologically_closed())
	    System.out.println("c_poly1 is topologically closed.");
	else
	    System.out.println("c_poly1 is not topologically closed.");
	if(c_poly1.is_bounded())
	    System.out.println("c_poly1 is bounded.");
	else
	    System.out.println("c_poly1 is not bounded.");
	if(c_poly1.is_disjoint_from(c_poly2))
	    System.out.println("c_poly1 is disjoint.");
	else
	    System.out.println("c_poly1 is not disjoint.");
	if(c_poly1.bounds_from_above(le2))
	    System.out.println("c_poly1 bounds from above le2.");
	else
	    System.out.println("c_poly1 not bounds from above le2.");
        if(c_poly1.bounds_from_below(le2))
	    System.out.println("c_poly1 bounds from below le2.");
	else
	    System.out.println("c_poly1 not bounds from below.");
	if(c_poly1.contains(c_poly2))
	    System.out.println("c_poly1 contains c_poly2.");
	else
	    System.out.println("c_poly1 not contains c_poly2.");
	if(c_poly1.strictly_contains(c_poly2))
	    System.out.println("poly strictly contains c_poly2.");
	else
	    System.out.println("poly not strictly contains c_poly2.");
    }
}
