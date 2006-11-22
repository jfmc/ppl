m4_divert(-1)`'dnl
m4_define(`m4_run_class_code',
`	test1.run_@CLASS@_test();`'dnl
')

m4_define(`m4_run_class_test_code',
`
    public boolean run_@CLASS@_test() {
')

m4_define(`m4_new_class_element_code',
`
 @TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1); @TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@2 = new @TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@1);
')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',`
 @LTOPOLOGY@@LCLASS@1.bounds_from_@ABOVEBELOW@(le);
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
 @LTOPOLOGY@@LCLASS@1.@BINMINOP@(@LTOPOLOGY@@LCLASS@1);
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code', `
	  @LTOPOLOGY@@LCLASS@1.@GET_REPRESENT@s();
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code', `
@LTOPOLOGY@@LCLASS@2.@COMPARISON@(@LTOPOLOGY@@LCLASS@1);
')

 m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code', `
 	  @LTOPOLOGY@@LCLASS@1.relation_with(@RELATION_REPRESENT@1);
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
 @LTOPOLOGY@@LCLASS@1.add_@ADD_REPRESENT@s_and_minimize(@ADD_REPRESENT@s1);
 ')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code', `
 @LTOPOLOGY@@LCLASS@1.@AFFIMAGE@(var, le, coeff1);
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code', `
 @LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@(le, Relation_Symbol.EQUAL , le);
')

 m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code', `
 @LTOPOLOGY@@LCLASS@1.@UB_EXACT@(@LTOPOLOGY@@LCLASS@1);
 ')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_code', `
@LTOPOLOGY@@LCLASS@1.generalized_@AFFIMAGE@(var, Relation_Symbol.EQUAL, le,
coeff1, coeff1);
')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code', `
@LTOPOLOGY@@LCLASS@1.equals(@LTOPOLOGY@@LCLASS@1);
')

m4_define(`ppl_@CLASS@_OK_code', `
@LTOPOLOGY@@LCLASS@1.OK();
')


m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code', `
@LTOPOLOGY@@LCLASS@1.bounded_@AFFIMAGE@(var, le, le, coeff1);
')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',`
@LTOPOLOGY@@LCLASS@1.@SIMPLIFY@();
')

m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code', `
@LTOPOLOGY@@LCLASS@1.@UB_EXACT@(@LTOPOLOGY@@LCLASS@1);
')

m4_define(`ppl_@CLASS@_@MAXMIN@_code', `
@LTOPOLOGY@@LCLASS@1.@MAXMIN@(le, coeff0, coeff1, bool_by_ref1);
')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code', `
@LTOPOLOGY@@LCLASS@1.@MAXMIN@(le, coeff0, coeff1, bool_by_ref1, @GENERATOR@1);
');
m4_divert`'dnl