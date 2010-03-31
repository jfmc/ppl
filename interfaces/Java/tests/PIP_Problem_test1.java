/* Test PIP_Problem Java test class of the Parma Polyhedra Library Java
   interface.
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

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Vector;
import parma_polyhedra_library.*;


public class PIP_Problem_test1 {
static {
    try {
        System.loadLibrary("ppl_java");
    }
    catch (UnsatisfiedLinkError  e) {
        System.out.println("Unable to load the library");
        System.out.println(e.getMessage());
        System.exit(-1);
    }
}

    // This code tests the PIP_Problem methods.
    public static boolean test01() {
	Variable A = new Variable(0);
	Variable B = new Variable(1);
	Variable C = new Variable(2);
	Variable D = new Variable(3);
        Variables_Set var_set_D = new Variables_Set();
        var_set_D.add(D);
	Linear_Expression_Variable le_b = new Linear_Expression_Variable(B);
	Linear_Expression_Variable le_c = new Linear_Expression_Variable(C);
	Linear_Expression_Variable le_a = new Linear_Expression_Variable(A);
	Linear_Expression_Variable le_d = new Linear_Expression_Variable(D);
        Coefficient coeff_1 = new Coefficient(1);
        Coefficient coeff_3 = new Coefficient(3);
        Coefficient coeff_5 = new Coefficient(5);
	Linear_Expression le_1 = new Linear_Expression_Coefficient(coeff_1);
	Linear_Expression le_3 = new Linear_Expression_Coefficient(coeff_3);
	Linear_Expression le_5 = new Linear_Expression_Coefficient(coeff_5);

	// Constraint declarations.
	Constraint c_a_geq_1
          = new Constraint(le_a, Relation_Symbol.GREATER_OR_EQUAL, le_1);
	Constraint c_a_leq_5
          = new Constraint(le_a, Relation_Symbol.LESS_OR_EQUAL, le_5);
	Constraint c_b_geq_3
          = new Constraint(le_b, Relation_Symbol.GREATER_OR_EQUAL, le_3);
	Constraint c_d_leq_1
          = new Constraint(le_d, Relation_Symbol.LESS_OR_EQUAL, le_1);
	Constraint constraint1 = c_a_geq_1;
	Constraint constraint2 = c_b_geq_3;
	Constraint_System constraints1 = new Constraint_System();
        constraints1.add(constraint1);
        C_Polyhedron ph1 = new C_Polyhedron(3, Degenerate_Element.UNIVERSE);
        ph1.add_constraints(constraints1);
        C_Polyhedron ph2 = new C_Polyhedron(4, Degenerate_Element.UNIVERSE);
        ph2.add_constraints(constraints1);
        ph2.add_constraint(constraint2);

	PIP_Problem pip1 = new PIP_Problem(3);
        pip1.add_constraints(constraints1);
        Constraint_System pip1_constraints = pip1.constraints();
//        Constraint pip1_c1 = pip1.constraint_at_index(0);
        long pip1_dim = pip1.space_dimension();

        PIP_Problem pip2 = new PIP_Problem(pip1_dim);
        pip2.add_constraints(pip1_constraints);

        boolean ok = (pip2.space_dimension() == 3);
        C_Polyhedron pip2_ph = new C_Polyhedron(3,
                                                Degenerate_Element.UNIVERSE);
        pip2_ph.add_constraints(pip1_constraints);
        ok = ok && new Boolean(pip2_ph.equals(ph1));
        if (!ok)
          return false;

        PIP_Problem pip3 = new PIP_Problem(3);
        pip3.add_constraints(constraints1);
        pip3.add_space_dimensions_and_embed(0, 1);
        pip3.add_constraint(constraint2);
        ok = ok && (pip3.space_dimension() == 4);
        Constraint_System pip3_constraints = pip3.constraints();
        C_Polyhedron pip3_ph = new C_Polyhedron(4,
                                                Degenerate_Element.UNIVERSE);
        pip3_ph.add_constraints(pip3_constraints);
        ok = ok && pip3_ph.equals(ph2);

	Constraint constraint4 = c_d_leq_1;
	Constraint_System constraints4 = new Constraint_System();
        constraints4.add(constraint1);
        constraints4.add(constraint4);
        PIP_Problem pip4 = new PIP_Problem(4, constraints4, var_set_D);
        ok = ok
             && (pip4.space_dimension() == 4)
             && (pip4.number_of_parameter_space_dimensions() == 1);
        Constraint_System pip4_constraints = pip4.constraints();
        C_Polyhedron pip4_ph = new C_Polyhedron(4,
                                                Degenerate_Element.UNIVERSE);
        C_Polyhedron ph4 = new C_Polyhedron(4, Degenerate_Element.UNIVERSE);
        ph4.add_constraint(constraint1);
        ph4.add_constraint(constraint4);
        pip4_ph.add_constraints(pip4_constraints);
        ok = ok && pip4_ph.equals(ph4);

	return ok;
    }

    // This code tests more PIP_Problem methods.
    public static boolean test02() {
	Variable A = new Variable(0);
	Variable B = new Variable(1);
	Linear_Expression_Variable le_a = new Linear_Expression_Variable(A);
        Coefficient coeff_0 = new Coefficient(0);
        Coefficient coeff_1 = new Coefficient(1);
        Coefficient coeff_5 = new Coefficient(5);
        Coefficient coeff_8 = new Coefficient(8);
	Linear_Expression le_1 = new Linear_Expression_Coefficient(coeff_1);
	Linear_Expression le_5 = new Linear_Expression_Coefficient(coeff_5);
	Linear_Expression le_8 = new Linear_Expression_Coefficient(coeff_8);

	// Constraint declarations.
	Constraint c_a_geq_1
          = new Constraint(le_a, Relation_Symbol.GREATER_OR_EQUAL, le_1);
	Constraint c_a_leq_5
          = new Constraint(le_a, Relation_Symbol.LESS_OR_EQUAL, le_5);
	Constraint c_a_eq_8
          = new Constraint(le_a, Relation_Symbol.EQUAL, le_8);
	Constraint constraint1 = c_a_geq_1;
	Constraint_System constraints1 = new Constraint_System();
        constraints1.add(constraint1);

        Variables_Set var_set_B = new Variables_Set();
        var_set_B.add(B);

	PIP_Problem pip1 = new PIP_Problem(2);
        pip1.add_constraints(constraints1);
        Constraint_System pip1_constraints = pip1.constraints();
        long pip1_dim = pip1.space_dimension();
        pip1.add_to_parameter_space_dimensions(var_set_B);
        Variables_Set var_set1 = pip1.parameter_space_dimensions();
        boolean ok = (var_set1.contains(B));
        if (!ok)
          return false;
        ok = pip1.is_satisfiable();
        if (!ok)
          return false;

        PIP_Problem_Status pip1_status;
        pip1_status = pip1.solve();
	ok = ok && (pip1_status == PIP_Problem_Status.OPTIMIZED_PIP_PROBLEM);

        PIP_Problem_Status pip2_status;
        pip1.add_constraint(c_a_leq_5);
        pip2_status = pip1.solve();
	ok = ok && (pip2_status == PIP_Problem_Status.OPTIMIZED_PIP_PROBLEM);
        if (!ok)
          return false;

        PPL_Test.println_if_noisy("Testing toString() and wrap_string(): ");
        PPL_Test.println_if_noisy(IO.wrap_string(pip1.toString(), 4, 64, 60));
        PPL_Test.println_if_noisy();

        PPL_Test.print_if_noisy("Testing max_space_dimension(): ");
        long max_space_dim = pip1.max_space_dimension();
        PPL_Test.println_if_noisy(max_space_dim);

        PIP_Problem_Control_Parameter_Value cp_value1
          = pip1.get_pip_problem_control_parameter
              (PIP_Problem_Control_Parameter_Name.CUTTING_STRATEGY);
        pip1.set_pip_problem_control_parameter(
          PIP_Problem_Control_Parameter_Value.CUTTING_STRATEGY_FIRST);
        PIP_Problem_Control_Parameter_Value cp_value2
          = pip1.get_pip_problem_control_parameter
              (PIP_Problem_Control_Parameter_Name.CUTTING_STRATEGY);
        ok = ok
          && (cp_value2
                == PIP_Problem_Control_Parameter_Value.CUTTING_STRATEGY_FIRST);
        if (!ok)
          return false;

        pip1.set_pip_problem_control_parameter(
          PIP_Problem_Control_Parameter_Value.PIVOT_ROW_STRATEGY_MAX_COLUMN);
        PIP_Problem_Control_Parameter_Value cp_value3
          = pip1.get_pip_problem_control_parameter
              (PIP_Problem_Control_Parameter_Name.PIVOT_ROW_STRATEGY);
        ok = ok
          && (cp_value3
                == PIP_Problem_Control_Parameter_Value
                    .PIVOT_ROW_STRATEGY_MAX_COLUMN);

        pip1.set_pip_problem_control_parameter(
          PIP_Problem_Control_Parameter_Value.CUTTING_STRATEGY_ALL);
        PIP_Problem_Control_Parameter_Value cp_value4
          = pip1.get_pip_problem_control_parameter
              (PIP_Problem_Control_Parameter_Name.CUTTING_STRATEGY);
        ok = ok
          && (cp_value4
                == PIP_Problem_Control_Parameter_Value.CUTTING_STRATEGY_ALL);

	return ok && pip1.OK();
    }


    // This code tests PIP_Tree and its methods.
    public static boolean test03() {
	Variable A = new Variable(0);
	Variable B = new Variable(1);
	Variable C = new Variable(2);
	Variable D = new Variable(3);
        Variables_Set var_set_D = new Variables_Set();
        var_set_D.add(D);
	Linear_Expression_Variable le_b = new Linear_Expression_Variable(B);
	Linear_Expression_Variable le_c = new Linear_Expression_Variable(C);
	Linear_Expression_Variable le_a = new Linear_Expression_Variable(A);
	Linear_Expression_Variable le_d = new Linear_Expression_Variable(D);
        Coefficient coeff_0 = new Coefficient(0);
        Coefficient coeff_1 = new Coefficient(1);
        Coefficient coeff_3 = new Coefficient(3);
        Coefficient coeff_5 = new Coefficient(5);
        Coefficient coeff_8 = new Coefficient(8);
	Linear_Expression le_1 = new Linear_Expression_Coefficient(coeff_1);
	Linear_Expression le_3 = new Linear_Expression_Coefficient(coeff_3);
	Linear_Expression le_5 = new Linear_Expression_Coefficient(coeff_5);
	Linear_Expression le_8 = new Linear_Expression_Coefficient(coeff_8);

	// Constraint declarations.
	Constraint c_a_geq_1
          = new Constraint(le_a, Relation_Symbol.GREATER_OR_EQUAL, le_1);
	Constraint c_a_leq_5
          = new Constraint(le_a, Relation_Symbol.LESS_OR_EQUAL, le_5);
	Constraint c_a_eq_8
          = new Constraint(le_a, Relation_Symbol.EQUAL, le_8);
	Constraint c_d_leq_1
          = new Constraint(le_d, Relation_Symbol.LESS_OR_EQUAL, le_1);
	Constraint constraint1 = c_a_geq_1;
	Constraint_System constraints1 = new Constraint_System();
        constraints1.add(constraint1);

        Variables_Set var_set_B = new Variables_Set();
        var_set_B.add(B);

	PIP_Problem pip1 = new PIP_Problem(2);
        pip1.add_constraints(constraints1);
        Constraint_System pip1_constraints = pip1.constraints();
        long pip1_dim = pip1.space_dimension();
        pip1.add_to_parameter_space_dimensions(var_set_B);
        Variables_Set var_set1 = pip1.parameter_space_dimensions();
        boolean ok = pip1.is_satisfiable();
        if (!ok)
          return false;
        PIP_Tree_Node ptree = pip1.solution();
        long num_of_arts = ptree.number_of_artificials();
//        PIP_Solution_Node psol = ptree.as_solution();
        Artificial_Parameter_Sequence_Iterator art_it_begin = ptree.begin();
        Artificial_Parameter_Sequence_Iterator art_it_end = ptree.end();
        ok = (ptree.OK() && (num_of_arts == 0) && (art_it_begin != art_it_end));
        if (!ok)
          return false;
//        pip1.free();

	Constraint constraint2 = c_d_leq_1;
	Constraint_System constraints2 = new Constraint_System();
        constraints2.add(constraint1);
        constraints2.add(constraint2);
        PIP_Problem pip2 = new PIP_Problem(4, constraints2, var_set_D);
        Constraint_System pip2_constraints = pip2.constraints();
        PIP_Tree_Node ptree2 = pip2.solution();
        long num_of_arts2 = ptree2.number_of_artificials();
//        PIP_Solution_Node psol2 = ptree2.as_solution();
        Artificial_Parameter_Sequence_Iterator art_it_begin2 = ptree2.begin();
        Artificial_Parameter_Sequence_Iterator art_it_end2 = ptree2.end();
//        art_it_begin.next();
        ok = (ptree2.OK() && (num_of_arts2 == 0)
             && (art_it_begin2 != art_it_end2));
//        ptree2.free();
        if (!ok)
          return false;
        return true;
   }
    public static void main(String[] args) {
        Parma_Polyhedra_Library.initialize_library();
	boolean test_result_ok =
	    Test_Executor.executeTests(PIP_Problem_test1.class);
        Parma_Polyhedra_Library.finalize_library();
	if (!test_result_ok)
	    System.exit(1);
	System.exit(0);
    }
}
