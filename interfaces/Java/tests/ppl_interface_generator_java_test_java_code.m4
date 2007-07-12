dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

m4_divert(-1)`'dnl
m4_define(`m4_run_class_code',
`	test1.run_@CLASS@_test();`'dnl
')

m4_define(`m4_run_class_test_code',
`
    public boolean run_@CLASS@_test() {
try {
')

m4_define(`m4_new_class_element_code',
`
 @TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
@TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@2 = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',`
boolean @LTOPOLOGY@@CLASS@1_bounds_from_@ABOVEBELOW@ = @LTOPOLOGY@@LCLASS@1.bounds_from_@ABOVEBELOW@(le);
')


m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code', `
        if (@LTOPOLOGY@@LCLASS@1.@HAS_PROPERTY@())
           System.out.println(
             "@HAS_PROPERTY@ is true for @LTOPOLOGY@@LCLASS@1.");
        else
           System.out.println(
             "@HAS_PROPERTY@ is false for @LTOPOLOGY@@LCLASS@1.");

')

m4_define(`ppl_@CLASS@_hashcode_code', `
              System.out.println("The hashcode is: " + @LTOPOLOGY@@LCLASS@1.hashCode());

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code', `
        if (@LTOPOLOGY@@LCLASS@1.@HAS_PROPERTY@())
           System.out.println(
             "@HAS_PROPERTY@ is true for @LTOPOLOGY@@LCLASS@1.");
        else
           System.out.println(
             "@HAS_PROPERTY@ is false for @LTOPOLOGY@@LCLASS@1.");

')


m4_define(`ppl_@CLASS@_@DIMENSION@_code', `
           System.out.print("@DIMENSION@ of @LTOPOLOGY@@LCLASS@1 = ");
           System.out.println(@LTOPOLOGY@@LCLASS@1.@DIMENSION@());

')

m4_define(`ppl_@CLASS@_@BINOP@_code',`
@LTOPOLOGY@@LCLASS@1.@BINOP@(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',`
boolean @LTOPOLOGY@@LCLASS@1_@BINMINOP@
  = @LTOPOLOGY@@LCLASS@1.@BINMINOP@(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code', `
@UGET_REPRESENT@_System @LTOPOLOGY@@LCLASS@1_@GET_REPRESENT@
  = @LTOPOLOGY@@LCLASS@1.@GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code', `
boolean @LTOPOLOGY@@LCLASS@1_@COMPARISON@
  = @LTOPOLOGY@@LCLASS@2.@COMPARISON@(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code', `
@LTOPOLOGY@@LCLASS@1.@EXTRAPOLATION@_narrowing_assign(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code', `
Poly_@UALT_RELATION_REPRESENT@_Relation @LTOPOLOGY@@LCLASS@1_@UALT_RELATION_REPRESENT@_Relation_with_@RELATION_REPRESENT@
  = @LTOPOLOGY@@LCLASS@1.relation_with(@RELATION_REPRESENT@1);

 ')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code', `
@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@(@ADD_REPRESENT@1);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code', `
@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@_and_minimize(@ADD_REPRESENT@1);

')

 m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code', `

@LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@s(@ADD_REPRESENT@s1);

 ')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code', `
boolean @LTOPOLOGY@@LCLASS@1_add_@ADD_REPRESENT@s_and_minimize
  = @LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@s_and_minimize(@ADD_REPRESENT@s1);

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code', `
@LTOPOLOGY@@LCLASS@1.@AFFIMAGE@(var, le, coeff1);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code', `
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@(le, Relation_Symbol.EQUAL , le);

')

 m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code', `
boolean @LTOPOLOGY@@LCLASS@1_@UB_EXACT@
  = @LTOPOLOGY@@LCLASS@1.@UB_EXACT@(@LTOPOLOGY@@LCLASS@1);

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_code', `
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@(var, Relation_Symbol.EQUAL,
						    le, coeff1, coeff1);

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code', `
boolean @LTOPOLOGY@@LCLASS@1_equals
  = @LTOPOLOGY@@LCLASS@1.equals(@LTOPOLOGY@@LCLASS@1);

if (!@LTOPOLOGY@@LCLASS@1.equals(new Object()))
   System.out.println("A generic object is not equal to @LTOPOLOGY@@LCLASS@1");
')

m4_define(`ppl_@CLASS@_OK_code', `
boolean @LTOPOLOGY@@LCLASS@1_OK
  = @LTOPOLOGY@@LCLASS@1.OK();

')


m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code', `
@LTOPOLOGY@@LCLASS@1.bounded_@AFFIMAGE@(var, le, le, coeff1);


')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',`
@LTOPOLOGY@@LCLASS@1.@SIMPLIFY@();

')


m4_define(`ppl_@CLASS@_@MAXMIN@_code', `
boolean @LTOPOLOGY@@LCLASS@1_@MAXMIN@
  = @LTOPOLOGY@@LCLASS@1.@MAXMIN@(le, coeff0, coeff1, bool_by_ref1);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code', `
boolean @LTOPOLOGY@@LCLASS@1_@MAXMIN@_with_point
  = @LTOPOLOGY@@LCLASS@1.@MAXMIN@(le, coeff0, coeff1, bool_by_ref1, @GENERATOR@1);

');

m4_define(`ppl_@CLASS@_string_code', `
System.out.println(@LTOPOLOGY@@LCLASS@1.toString());

');

m4_divert`'dnl
