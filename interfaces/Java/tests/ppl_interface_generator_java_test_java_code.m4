dnl  -*- java -*-
m4_divert(-1)

This m4 file contains the code for generating ppl_java_generated_tests.java

Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ .

FIXME: Find a way to avoid having these dummy macros.
No code is needed for these procedure schemas in the Java interface
as the tokens argument for widening and extrapolation is optional.

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_widening_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_code', `')
m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code', `')

FIXME: This is the pattern used for finalize() which is protected.
m4_define(`ppl_delete_@CLASS@_code', `')

Define here as empty any known schematic method macros for which
the definition is not yet implemented.
m4_define(`ppl_@CLASS@_delete_iterator_code', `')

m4_define(`m4_run_class_code',
`dnl
    test1.run_@CLASS@_test();
')

m4_define(`m4_run_class_test_code',
`dnl
    public boolean run_@CLASS@_test() {
    try {

')

    m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
{
    System.out.print("Testing @TOPOLOGY@@CLASS@_from_space_dimension: ");
    @TOPOLOGY@@CLASS@ new_0_universe
        = new @TOPOLOGY@@CLASS@(0, Degenerate_Element.UNIVERSE);
    @TOPOLOGY@@CLASS@ new_6_universe
        = new @TOPOLOGY@@CLASS@(6, Degenerate_Element.UNIVERSE);
    @TOPOLOGY@@CLASS@ new_0_empty
        = new @TOPOLOGY@@CLASS@(0, Degenerate_Element.EMPTY);
    @TOPOLOGY@@CLASS@ new_6_empty
        = new @TOPOLOGY@@CLASS@(6, Degenerate_Element.EMPTY);
    if (new_0_universe.OK() && new_6_universe.OK()
       && new_0_empty.OK() && new_6_empty.OK())
       System.out.println("Success");
    else
      System.out.println("Failure");
}

')

    m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
{
    System.out.print("Testing @TOPOLOGY@@CLASS@ from @FRIEND@: ");
    @FRIEND@ friend_gd = new @FRIEND@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ new_gd = new @TOPOLOGY@@CLASS@(friend_gd);
    if (new_gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity_code',
    `dnl
{
    if (("@FRIEND@" != "@TOPOLOGY@@CLASS@")) {
        System.out.print("Testing @TOPOLOGY@@CLASS@ from @FRIEND@ with complexity: ");
        @FRIEND@ friend_gd
            = new @FRIEND@(@CONSTRAINER@s1);
        @TOPOLOGY@@CLASS@ new_gd_pc
            = new @TOPOLOGY@@CLASS@(friend_gd, Complexity_Class.POLYNOMIAL_COMPLEXITY);
        @TOPOLOGY@@CLASS@ new_gd_sc
            = new @TOPOLOGY@@CLASS@(friend_gd, Complexity_Class.SIMPLEX_COMPLEXITY);
        @TOPOLOGY@@CLASS@ new_gd_ac
            = new @TOPOLOGY@@CLASS@(friend_gd, Complexity_Class.ANY_COMPLEXITY);
        if (new_gd_ac.OK())
            System.out.println("Success");
        else
            System.out.println("Failure");
    }
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
    `dnl
{
    System.out.print("Testing @TOPOLOGY@@CLASS@ from @BUILD_REPRESENT@s: ");
    @TOPOLOGY@@CLASS@ new_gd1 = new @TOPOLOGY@@CLASS@(@BUILD_REPRESENT@s1);
    if (new_gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_swap_code',
    `dnl
{
    System.out.print("Testing swap: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s2);
    gd1.swap(gd2);
    if (gd1.OK() && gd2.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
    gd1.free();
    gd1.free();
}

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
    `dnl
{
    System.out.print("Testing bounds_from_@ABOVEBELOW@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean bounds_from_@ABOVEBELOW@
        = gd.bounds_from_@ABOVEBELOW@(le_A);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
    gd.free();
}

')

m4_define(`ppl_@CLASS@_hashcode_code',
    `dnl
{
    System.out.print("Testing hashcode: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    System.out.print("The hashcode is: " + gd.hashCode());
    if (gd.OK())
        System.out.println(", Success");
    else
        System.out.println(", Failure");
    gd.free();
}

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
    `dnl
{
    System.out.print("Testing @HAS_PROPERTY@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    if (gd.@HAS_PROPERTY@())
        System.out.println("@HAS_PROPERTY@ is true for gd.");
    else
        System.out.println("@HAS_PROPERTY@ is false for gd.");
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
    `dnl
{
    System.out.print("Testing @DIMENSION@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    System.out.print("@DIMENSION@ of gd = ");
    System.out.println(gd.@DIMENSION@());
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
    `dnl
{
    System.out.print("Testing @BINOP@: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s2);
    gd1.@BINOP@(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_code',
    `dnl
{
    System.out.print("Testing simplify_using_context_assign: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean gd_simplify_using_context_assign
        = gd.simplify_using_context_assign(gd);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
    `dnl
{
    System.out.print("Testing get_@GET_REPRESENT@s: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @UGET_REPRESENT@_System gd_@GET_REPRESENT@ = gd.@GET_REPRESENT@s();
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
    `dnl
{
    System.out.print("Testing get_minimized_@GET_REPRESENT@s: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @UGET_REPRESENT@_System gr = gd.minimized_@GET_REPRESENT@s();
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
    `dnl
{
    System.out.print("Testing @COMPARISON@: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s2);
    boolean gd1_@COMPARISON@ = gd2.@COMPARISON@(gd1);
    if (gd1.OK() && gd2.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
    `dnl
{
    System.out.print("Testing @EXTRAPOLATION@_narrowing_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s2);
    gd1.@EXTRAPOLATION@_narrowing_assign(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
    `dnl
{
    System.out.print("Testing relation_with_@RELATION_REPRESENT@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    Poly_@UALT_RELATION_REPRESENT@_Relation
        poly_relation = gd.relation_with(@RELATION_REPRESENT@1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
    `dnl
{
    System.out.print("Testing add_@ADD_REPRESENT@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.add_@ADD_REPRESENT@(@ADD_REPRESENT@1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
    `dnl
{
    System.out.print("Testing refine_with_@REFINE_REPRESENT@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.refine_with_@REFINE_REPRESENT@(@REFINE_REPRESENT@1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

 m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
    `dnl
{
    System.out.print("Testing add_@ADD_REPRESENT@s: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.add_@ADD_REPRESENT@s(@ADD_REPRESENT@s1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

 m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
    `dnl
{
    System.out.print("Testing refine_with_@REFINE_REPRESENT@s: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.refine_with_@REFINE_REPRESENT@s(@REFINE_REPRESENT@s1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

 m4_define(`ppl_@CLASS@_@UB_EXACT@_code',
    `dnl
{
    System.out.print("Testing @UB_EXACT@: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean is_exact = gd1.@UB_EXACT@(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
    `dnl
{
    System.out.print("Testing @AFFIMAGE@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.@AFFIMAGE@(var_C, le_A, coeff_5);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
    `dnl
{
    System.out.print("Testing generalized_@AFFIMAGE@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.generalized_@AFFIMAGE@(var_C, Relation_Symbol.EQUAL, le_A, coeff_5);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
    `dnl
{
    System.out.print("Testing generalized_@AFFIMAGE@_lhs_rhs: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.generalized_@AFFIMAGE@(le_A, Relation_Symbol.EQUAL, le_A);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
    `dnl
{
    System.out.print("Testing generalized_@AFFIMAGE@_with_congruence: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.generalized_@AFFIMAGE@_with_congruence(var_C, Relation_Symbol.EQUAL,
                                              le_A, coeff_5, coeff_5);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
    `dnl
{
    System.out.print("Testing generalized_@AFFIMAGE@_lhs_rhs_with_congruence: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.generalized_@AFFIMAGE@_lhs_rhs_with_congruence(le_A,
                                                      Relation_Symbol.EQUAL,
                                                      le_A, coeff_5);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
    `dnl
{
    System.out.print("Testing equals: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean equals = gd.equals(gd);
    if (!gd.equals(new Object()))
        System.out.println("A generic object is not equal to gd");
    if (equals && gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_OK_code',
    `dnl
{
    System.out.print("Testing OK: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean ok = gd.OK();
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
    `dnl
{
    System.out.print("Testing bounded_@AFFIMAGE@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.bounded_@AFFIMAGE@(var_C, le_A, le_A, coeff_5);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
    `dnl
{
    System.out.print("Testing @SIMPLIFY@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.@SIMPLIFY@();
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
    `dnl
{
    System.out.print("Testing unconstrain_space_dimension: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.unconstrain_space_dimension(var_C);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
    `dnl
{
    System.out.print("Testing unconstrain_space_dimensions: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.unconstrain_space_dimensions(var_set_A);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_constrains_code',
    `dnl
{
    System.out.print("Testing constrains: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean constrains = gd.constrains(var_C);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
    `dnl
{
    System.out.print("Testing @MAXMIN@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean @MAXMIN@
        = gd.@MAXMIN@(le_A, coeff_0, coeff_5, bool_by_ref1);
    if (gd.OK())
        System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
    `dnl
{
    System.out.print("Testing @MAXMIN@_with_point: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    boolean @MAXMIN@_with_point
        = gd.@MAXMIN@(le_A, coeff_0, coeff_5, bool_by_ref2, generator1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

');

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
    `dnl
{
    System.out.print("Testing add_space_dimensions_@EMBEDPROJECT@: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.add_space_dimensions_@EMBEDPROJECT@(2);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
    `dnl
{
    System.out.print("Testing remove_higher_space_dimensions: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.remove_higher_space_dimensions(2);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
    `dnl
{
    System.out.print("Testing remove_space_dimensions: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.remove_space_dimensions(var_set_A);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
    `dnl
{
    System.out.print("Testing expand_space_dimension: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.expand_space_dimension(var_C, 1);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
    `dnl
{
    System.out.print("Testing fold_space_dimensions: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.fold_space_dimensions(var_set_A, var_C);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
    `dnl
{
    System.out.print("Testing map_space_dimensions: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd.map_space_dimensions(partial_function);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
    `dnl
{
    System.out.print("Testing @WIDEN@_widening_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.@WIDEN@_widening_assign(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
    `dnl
{
    System.out.print("Testing : ppl_@CLASS@_@WIDEN@_widening_assign");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.@WIDEN@_widening_assign(gd2, int_by_ref1);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_widening_assign_code',
    `dnl
{
    System.out.print("Testing widening_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.widening_assign(gd2,
                        int_by_ref1);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_code',
    `dnl
{
    System.out.print("Testing @EXTRAPOLATION@_extrapolation_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.@EXTRAPOLATION@_extrapolation_assign(gd2, int_by_ref1);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
    `dnl
{
    System.out.print("Testing @EXTRAPOLATION@_narrowing_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.@EXTRAPOLATION@_narrowing_assign(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
    `dnl
{
    System.out.print("Testing @LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(gd2, @CONSTRAINER@s1,
                                                          zero_by_ref1);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_code',
    `dnl
{
    System.out.print("Testing BGP99_@DISJUNCT_WIDEN@_extrapolation_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.BGP99_@DISJUNCT_WIDEN@_extrapolation_assign(gd2, 2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_code',
    `dnl
{
    System.out.print("Testing BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign: ");
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    gd1.BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign(gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_string_code',
    `dnl
{
    System.out.print("Testing toString(): ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    System.out.println(gd.toString());
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}
');

m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
    `dnl
{
    System.out.print("Testing @MEMBYTES@(): ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    System.out.println("@UMEMBYTES@ of gd: ");
    System.out.println(gd.@MEMBYTES@());
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

');

m4_define(`ppl_@CLASS@_ascii_dump_code',
    `dnl
{
    System.out.print("Testing ascii_dump(): ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    System.out.println(gd.ascii_dump());
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

');

m4_define(`ppl_@CLASS@_@PARTITION@_code',
    `dnl
{
    System.out.print("Testing @PARTITION@: ");
    @CLASSTOPOLOGY@@DISJUNCT@ gd1
        = new @CLASSTOPOLOGY@@DISJUNCT@(constraints1);
    @CLASSTOPOLOGY@@DISJUNCT@ gd2
        = new @CLASSTOPOLOGY@@DISJUNCT@(constraints1);
    Pair p
        = @CLASS@.@PARTITION@(gd1, gd2);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
    System.out.print("Printing Pair from @PARTITION@: ");
    System.out.print(p.getFirst());
    System.out.print(", ");
    System.out.println(p.getSecond());
}

');

m4_define(`ppl_@CLASS@_approximate_partition_code',
    `dnl
{
    System.out.print("Testing @CLASS@_approximate_partition: ");
    @CLASSTOPOLOGY@@DISJUNCT@ gd1
        = new @CLASSTOPOLOGY@@DISJUNCT@(constraints1);
    @CLASSTOPOLOGY@@DISJUNCT@ gd2
        = new @CLASSTOPOLOGY@@DISJUNCT@(constraints1);
    Pair p
        = @CLASS@.approximate_partition(gd1, gd2, bool_by_ref1);
    if (gd1.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
    System.out.print("Printing Pair from approximate_partition: ");
    System.out.print(p.getFirst());
    System.out.print(", ");
    System.out.print(p.getSecond());
    System.out.print(", ");
    System.out.println(bool_by_ref1);
    System.out.println();
}

');

m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
    `dnl
{
    System.out.print("Testing @BEGINEND@_iterator: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@_Iterator it_gd = gd.@BEGINEND@_iterator();
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code',
    `dnl
{
    System.out.print("Testing @INCDEC@_iterator: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    if ("@INCDEC@" == "increment") {
      @TOPOLOGY@@CLASS@_Iterator it_gd = gd.begin_iterator();
      it_gd.@ALT_INCDEC@();
    }
    else {
      @TOPOLOGY@@CLASS@_Iterator it_gd = gd.end_iterator();
      it_gd.@ALT_INCDEC@();
    }
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_get_disjunct_code',
    `dnl
{
    System.out.print("Testing get_disjunct: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@_Iterator it_gd = gd.begin_iterator();
    @TOPOLOGY@@DISJUNCT@ gd_disjunct = it_gd.get_disjunct();
    if (gd.OK() && gd_disjunct.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
    `dnl
{
    System.out.print("Testing drop_disjunct: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@_Iterator it_gd = gd.begin_iterator();
    gd.drop_disjunct(it_gd);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_@CLASS@_add_disjunct_code',
    `dnl
{
    System.out.print("Testing add_disjunct: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @CLASSTOPOLOGY@@DISJUNCT@ gd_disjunct
        = new @CLASSTOPOLOGY@@DISJUNCT@(@CONSTRAINER@s1);
    gd.add_disjunct(gd_disjunct);
    if (gd.OK())
        System.out.println("Success");
    else
        System.out.println("Failure");
}

');

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
    `dnl
{
    System.out.print("Testing delete_iterator: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@_Iterator it_gd1 = gd.begin_iterator();
    @TOPOLOGY@@CLASS@_Iterator it_gd2 = gd.begin_iterator();
    boolean equals = it_gd1.equals(it_gd2);
    if (gd.OK() && equals)
        System.out.println("Success");
    else
        System.out.println("Failure");
}

')

m4_define(`ppl_free_@CLASS@_code',
    `dnl
{
    System.out.print("Testing free: ");
    @TOPOLOGY@@CLASS@ gd = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
    @TOPOLOGY@@CLASS@ gd1 = new @TOPOLOGY@@CLASS@(gd);
    gd1.free();
    @TOPOLOGY@@CLASS@ gd2 = new @TOPOLOGY@@CLASS@(gd);
    gd2 = null;
    System.out.println("Success");
}

')

m4_divert`'dnl
