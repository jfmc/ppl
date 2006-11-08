/* Test C_Polyhedron Java test class of the Parma Polyhedra Library Java
   interface.
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
	if (c_poly1.is_empty())
	    System.out.println("c_poly1 is empty..");
	else
	    System.out.println("c_poly1 is not empty.");
   	if (c_poly1.is_universe())
	    System.out.println("c_poly1 is universe.");
	else
	    System.out.println("c_poly1 is not universe.");
	if (c_poly1.is_topologically_closed())
	    System.out.println("c_poly1 is topologically closed.");
	else
	    System.out.println("c_poly1 is not topologically closed.");
	if (c_poly1.is_bounded())
	    System.out.println("c_poly1 is bounded.");
	else
	    System.out.println("c_poly1 is not bounded.");
	if (c_poly1.is_disjoint_from(c_poly2))
	    System.out.println("c_poly1 is disjoint.");
	else
	    System.out.println("c_poly1 is not disjoint.");
	if (c_poly1.bounds_from_above(le2))
	    System.out.println("c_poly1 bounds from above le2.");
	else
	    System.out.println("c_poly1 not bounds from above le2.");
        if (c_poly1.bounds_from_below(le2))
	    System.out.println("c_poly1 bounds from below le2.");
	else
	    System.out.println("c_poly1 not bounds from below.");
	if (c_poly1.contains(c_poly2))
	    System.out.println("c_poly1 contains c_poly2.");
	else
	    System.out.println("c_poly1 not contains c_poly2.");
	if (c_poly1.strictly_contains(c_poly2))
	    System.out.println("c_poly1 strictly contains c_poly2.");
	else
	    System.out.println("c_poly1 not strictly contains c_poly2.");
        if (c_poly1.contains_integer_point())
	    System.out.println("c_poly1 contains an integer point.");
	else
	    System.out.println("c_poly1 does not cointain an integer point.");
	if (c_poly1.is_discrete())
	    System.out.println("c_poly1 is discrete.");
	else
	    System.out.println("c_poly1 is not discrete.");
	Linear_Expression le5 = new Linear_Expression_Variable(A);
	le5.sum(new Linear_Expression_Variable(B));
	le5.times(new Coefficient(100));
	le5.sum(new Linear_Expression_Coefficient(new Coefficient(145)));
	Constraint c5 = new Constraint(le1, le5,
				       Relation_Symbol.GREATER_THAN_OR_EQUAL);
        c_poly1.add_constraint(c5);

        c_poly1.affine_image(A,le1, new Coefficient(5));
        c_poly1.affine_preimage(A,le1, new Coefficient(5));
        c_poly1.generalized_affine_image(le1, Relation_Symbol.EQUAL, le2);
        c_poly1.generalized_affine_image(A, Relation_Symbol.EQUAL, le2,
					 new Coefficient(6));
        c_poly1.bounded_affine_image(B, le2, le3, new Coefficient(10));
        c_poly1.bounded_affine_preimage(B, le2, le4, new Coefficient(30));
        c_poly1.time_elapse_assign(c_poly2);
        c_poly1.difference_assign(c_poly2);
        c_poly1.topological_closure_assign();
        c_poly1.poly_hull_assign(c_poly2);
	if (c_poly1.poly_hull_assign_and_minimize(c_poly2))
            System.out.println("c_poly1 is minimized.");
	else
	    System.out.println("c_poly1 is not minimized.");
        c_poly1.intersection_assign(c_poly2);
    }
}
