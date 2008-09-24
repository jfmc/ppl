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

Define here as empty any known schematic method macros for which
the definition is not yet implemented.
m4_define(`ppl_@CLASS@_swap_code', `')
m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code', `')
m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code', `')
m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code', `')
m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code', `')
m4_define(`ppl_@CLASS@_remove_space_dimensions_code', `')
m4_define(`ppl_@CLASS@_expand_space_dimension_code', `')
m4_define(`ppl_@CLASS@_fold_space_dimensions_code', `')
m4_define(`ppl_@CLASS@_map_space_dimensions_code', `')
m4_define(`ppl_@CLASS@_widening_assign_with_tokens_code', `')
m4_define(`ppl_free_@CLASS@_code', `')
m4_define(`ppl_delete_@CLASS@_code', `')
m4_define(`ppl_@CLASS@_delete_iterator_code', `')
m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code', `')
m4_define(`ppl_@CLASS@_drop_disjunct_code', `')
m4_define(`ppl_@CLASS@_add_disjunct_code', `')
m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code', `')
m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code', `')
m4_define(`ppl_@CLASS@_iterator_equals_iterator_code', `')

m4_define(`m4_run_class_code',
`dnl
    test1.run_@CLASS@_test();
')

m4_define(`m4_run_class_test_code',
`dnl
    public boolean run_@CLASS@_test() {
    try {

')

m4_define(`m4_new_class_element_code',
`dnl
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@1
    = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@2
    = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@3
    = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@4
    = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@5
    = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
@LTOPOLOGY@@LCLASS@5.free();
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@6
    = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
@LTOPOLOGY@@LCLASS@6 = null;
System.gc();

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
{
System.out.print("Testing ppl_new_from_space_dimension: ");
@TOPOLOGY@@CLASS@ new_from_space_dimension1
    = new @TOPOLOGY@@CLASS@(0, Degenerate_Element.UNIVERSE);
@TOPOLOGY@@CLASS@ new_from_space_dimension2
    = new @TOPOLOGY@@CLASS@(6, Degenerate_Element.UNIVERSE);
@TOPOLOGY@@CLASS@ new_from_space_dimension3
    = new @TOPOLOGY@@CLASS@(0, Degenerate_Element.EMPTY);
@TOPOLOGY@@CLASS@ new_from_space_dimension4
    = new @TOPOLOGY@@CLASS@(6, Degenerate_Element.EMPTY);
if (new_from_space_dimension1.OK()
    && new_from_space_dimension2.OK()
    && new_from_space_dimension3.OK()
    && new_from_space_dimension4.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
{
System.out.print("Testing ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@: ");
@FRIEND@ test_from_@LFRIEND@1
    = new @FRIEND@(@CONSTRAINER@s1);
@TOPOLOGY@@CLASS@ new_from_@FRIEND@1
    = new @TOPOLOGY@@CLASS@(test_from_@LFRIEND@1);
if (new_from_@FRIEND@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity_code',
`dnl
{
if (("@FRIEND@" != "@TOPOLOGY@@CLASS@")) {
System.out.print("Testing ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity: ");
@FRIEND@ test_from_@LFRIEND@1
    = new @FRIEND@(@CONSTRAINER@s1);
@TOPOLOGY@@CLASS@ new_from_@FRIEND@1pc
    = new @TOPOLOGY@@CLASS@(test_from_@LFRIEND@1,
   Complexity_Class.POLYNOMIAL_COMPLEXITY);
@TOPOLOGY@@CLASS@ new_from_@FRIEND@1sc
    = new @TOPOLOGY@@CLASS@(test_from_@LFRIEND@1,
   Complexity_Class.SIMPLEX_COMPLEXITY);
@TOPOLOGY@@CLASS@ new_from_@FRIEND@1ac
    = new @TOPOLOGY@@CLASS@(test_from_@LFRIEND@1,
   Complexity_Class.ANY_COMPLEXITY);
}
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
{
System.out.print("Testing ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s: ");
@TOPOLOGY@@CLASS@ new_from_@BUILD_REPRESENT@s1
    = new @TOPOLOGY@@CLASS@(@BUILD_REPRESENT@s1);
if (new_from_@BUILD_REPRESENT@s1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_bounds_from_@ABOVEBELOW@: ");
boolean @LTOPOLOGY@@CLASS@1_bounds_from_@ABOVEBELOW@
    = @LTOPOLOGY@@LCLASS@1.bounds_from_@ABOVEBELOW@(le_A);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_hashcode_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_hashcode: ");
System.out.println(
                   "The hashcode is: " + @LTOPOLOGY@@LCLASS@1.hashCode());
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@HAS_PROPERTY@: ");
if (@LTOPOLOGY@@LCLASS@1.@HAS_PROPERTY@())
  System.out.println(
    "@HAS_PROPERTY@ is true for @LTOPOLOGY@@LCLASS@1.");
else
    System.out.println(
      "@HAS_PROPERTY@ is false for @LTOPOLOGY@@LCLASS@1.");
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@DIMENSION@: ");
System.out.print("@DIMENSION@ of @LTOPOLOGY@@LCLASS@1 = ");
System.out.println(@LTOPOLOGY@@LCLASS@1.@DIMENSION@());
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@BINOP@: ");
@LTOPOLOGY@@LCLASS@1.@BINOP@(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@BINMINOP@: ");
boolean @LTOPOLOGY@@LCLASS@1_@BINMINOP@
  = @LTOPOLOGY@@LCLASS@1.@BINMINOP@(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_simplify_using_context_assign: ");
boolean @LTOPOLOGY@@LCLASS@1_simplify_using_context_assign
  =
    @LTOPOLOGY@@LCLASS@1.simplify_using_context_assign(
      @LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_get_@GET_REPRESENT@s: ");
@UGET_REPRESENT@_System @LTOPOLOGY@@LCLASS@1_@GET_REPRESENT@
  = @LTOPOLOGY@@LCLASS@1.@GET_REPRESENT@s();
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@COMPARISON@_@CLASS@: ");
boolean @LTOPOLOGY@@LCLASS@1_@COMPARISON@
  = @LTOPOLOGY@@LCLASS@2.@COMPARISON@(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign: ");
@LTOPOLOGY@@LCLASS@1.@EXTRAPOLATION@_narrowing_assign(
  @LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_relation_with_@RELATION_REPRESENT@: ");
Poly_@UALT_RELATION_REPRESENT@_Relation
  @LTOPOLOGY@@LCLASS@1_@UALT_RELATION_REPRESENT@_Relation_with_@RELATION_REPRESENT@
  = @LTOPOLOGY@@LCLASS@1.relation_with(@RELATION_REPRESENT@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_add_@ADD_REPRESENT@: ");
@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@(@ADD_REPRESENT@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_refine_with_@REFINE_REPRESENT@: ");
@LTOPOLOGY@@LCLASS@1.refine_with_@REFINE_REPRESENT@(@REFINE_REPRESENT@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize: ");
@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@_and_minimize(@ADD_REPRESENT@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

 m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_add_@ADD_REPRESENT@s: ");
@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@s(@ADD_REPRESENT@s1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

 ')

 m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s: ");
@LTOPOLOGY@@LCLASS@1.refine_with_@REFINE_REPRESENT@s(@REFINE_REPRESENT@s1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

 ')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize: ");
boolean @LTOPOLOGY@@LCLASS@1_add_@ADD_REPRESENT@s_and_minimize
  = @LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@s_and_minimize(
      @ADD_REPRESENT@s1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@AFFIMAGE@: ");
@LTOPOLOGY@@LCLASS@1.@AFFIMAGE@(var_C, le_A, coeff_5);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs: ");
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@(le_A, Relation_Symbol.EQUAL,
                                           le_A);
 if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

 m4_define(`ppl_@CLASS@_@UB_EXACT@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@UB_EXACT@: ");
boolean @LTOPOLOGY@@LCLASS@1_@UB_EXACT@
  = @LTOPOLOGY@@LCLASS@1.@UB_EXACT@(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence: ");
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@_with_congruence(
  var_C,
  Relation_Symbol.EQUAL,
  le_A, coeff_5, coeff_5);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence: ");
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@_lhs_rhs_with_congruence(le_A,
                                                Relation_Symbol.EQUAL,
						le_A, coeff_5);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}
')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_equals_@CLASS@: ");
boolean @LTOPOLOGY@@LCLASS@1_equals
  = @LTOPOLOGY@@LCLASS@1.equals(@LTOPOLOGY@@LCLASS@1);

if (!@LTOPOLOGY@@LCLASS@1.equals(new Object()))
   System.out.println("A generic object is not equal to @LTOPOLOGY@@LCLASS@1");
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_OK: ");
boolean @LTOPOLOGY@@LCLASS@1_OK
  = @LTOPOLOGY@@LCLASS@1.OK();
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')


m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_bounded_@AFFIMAGE@: ");
@LTOPOLOGY@@LCLASS@1.bounded_@AFFIMAGE@(var_C, le_A, le_A, coeff_5);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@SIMPLIFY@: ");
@LTOPOLOGY@@LCLASS@1.@SIMPLIFY@();
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_unconstrain_space_dimension: ");
@LTOPOLOGY@@LCLASS@3.unconstrain_space_dimension(var_C);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_unconstrain_space_dimensions: ");
@LTOPOLOGY@@LCLASS@4.unconstrain_space_dimensions(var_set_A);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_constrains: ");
boolean @LTOPOLOGY@@LCLASS@1_constrains
  = @LTOPOLOGY@@LCLASS@1.constrains(var_C);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@MAXMIN@: ");
boolean @LTOPOLOGY@@LCLASS@1_@MAXMIN@
  = @LTOPOLOGY@@LCLASS@1.@MAXMIN@(le_A, coeff_0, coeff_5, bool_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@MAXMIN@_with_point: ");
boolean @LTOPOLOGY@@LCLASS@1_@MAXMIN@_with_point
  = @LTOPOLOGY@@LCLASS@1.@MAXMIN@(le_A, coeff_0, coeff_5, bool_by_ref2,
                                  generator1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

');

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@WIDEN@_widening_assign: ");
@LTOPOLOGY@@LCLASS@1.@WIDEN@_widening_assign(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
{
System.out.print("Testing : ppl_@CLASS@_@WIDEN@_widening_assign");
@LTOPOLOGY@@LCLASS@1.@WIDEN@_widening_assign(@LTOPOLOGY@@LCLASS@1,
                                             int_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_widening_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_widening_assign: ");
@LTOPOLOGY@@LCLASS@1.widening_assign(@LTOPOLOGY@@LCLASS@1,
                                     int_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign: ");
@LTOPOLOGY@@LCLASS@1.@EXTRAPOLATION@_extrapolation_assign(
  @LTOPOLOGY@@LCLASS@1,
  int_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign: ");
@LTOPOLOGY@@LCLASS@1.@EXTRAPOLATION@_narrowing_assign(@LTOPOLOGY@@LCLASS@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign: ");
@LTOPOLOGY@@LCLASS@1.@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(
  @LTOPOLOGY@@LCLASS@1,
  @CONSTRAINER@s1,
  zero_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign: ");
@LTOPOLOGY@@LCLASS@1.BGP99_@DISJUNCT_WIDEN@_extrapolation_assign(
  @LTOPOLOGY@@LCLASS@1,
  2);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign: ");
@LTOPOLOGY@@LCLASS@2.BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign(
  @LTOPOLOGY@@LCLASS@2);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_string_code',
`dnl
System.out.println(@LTOPOLOGY@@LCLASS@1.toString());

');

m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
`dnl
System.out.println("@UMEMBYTES@ of @LTOPOLOGY@@LCLASS@1: ");
System.out.print(@LTOPOLOGY@@LCLASS@1.@MEMBYTES@());

');

m4_define(`ppl_@CLASS@_ascii_dump_code',
`dnl
System.out.println(@LTOPOLOGY@@LCLASS@1.ascii_dump());

');

m4_define(`ppl_@CLASS@_@PARTITION@_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@PARTITION@: ");
@CLASSTOPOLOGY@@DISJUNCT@ @LCLASSTOPOLOGY@@DISJUNCT@1
    = new @CLASSTOPOLOGY@@DISJUNCT@(constraints1);
Pair p
    = @CLASS@.@PARTITION@(@LCLASSTOPOLOGY@@DISJUNCT@1,
                          @LCLASSTOPOLOGY@@DISJUNCT@1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
System.out.println("Printing Pair from @PARTITION@");
System.out.println(p.getFirst());
System.out.println(p.getSecond());
}

');

m4_define(`ppl_@CLASS@_approximate_partition_code',
`dnl
{
System.out.print("Testing @CLASS@_approximate_partition: ");
@CLASSTOPOLOGY@@DISJUNCT@ @LCLASSTOPOLOGY@@LDISJUNCT@1
    = new @CLASSTOPOLOGY@@DISJUNCT@(@LCONSTRAINER@s1);
Pair p
    = @CLASS@.approximate_partition(@LCLASSTOPOLOGY@@LDISJUNCT@1,
                          @LCLASSTOPOLOGY@@LDISJUNCT@1, bool_by_ref1);
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
System.out.println("Printing Pair from approximate_partition");
System.out.println(p.getFirst());
System.out.println(p.getSecond());
System.out.println(bool_by_ref1);
}

');

m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_@BEGINEND@_iterator: ");
@TOPOLOGY@@CLASS@_Iterator it_@LTOPOLOGY@@LCLASS@ =
    @LTOPOLOGY@@LCLASS@1.begin_iterator();
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`dnl
{
System.out.print("Testing ppl_@CLASS@_get_disjunct_code: ");
@TOPOLOGY@@CLASS@_Iterator it_@LTOPOLOGY@@LCLASS@ =
    @LTOPOLOGY@@LCLASS@1.begin_iterator();
@TOPOLOGY@@DISJUNCT@ @LTOPOLOGY@@LCLASS@get_disjunct
    = it_@LTOPOLOGY@@LCLASS@.get_disjunct();
if (@LTOPOLOGY@@LCLASS@1.OK())
   System.out.println("Success");
}

')
m4_divert`'dnl
