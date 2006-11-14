m4_divert(-1)dnl

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
    public native boolean @HAS_PROPERTY@();

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
    public native void @SIMPLIFY@();

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
    public native boolean bounds_from_@ABOVEBELOW@(Linear_Expression expr);

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
    public native boolean @COMPARISON@(@CLASS@ y);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
    public native void add_@ADD_REPRESENT@(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
    public native void add_@ADD_REPRESENT@_and_minimize(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
    public native void add_@ADD_REPRESENT@s(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
    public native void add_@ADD_REPRESENT@s_and_minimize(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
    public native void @BINOP@(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
    public native boolean @BINOP@_and_minimize(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
    public native void @AFFIMAGE@(Variable var, Linear_Expression expr,
				    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
    public native void bounded_@AFFIMAGE@(Variable var,
					    Linear_Expression lb_expr,
					    Linear_Expression ub_expr,
					    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
    public native void generalized_@AFFIMAGE@(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
    public native void generalized_@AFFIMAGE@(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs);

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
    public native void add_space_dimensions_@EMBEDPROJECT@(long m);

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
    public native void remove_space_dimensions(Variables_Set to_be_removed);

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
    public native void remove_higher_space_dimensions(long
						      new_dimension);

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
    public native void expand_space_dimension(Variable var, long m);

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
    public native void fold_space_dimensions(Variables_Set to_be_folded,
					     Variable var);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   Boolean maximum);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   Boolean maximum,
				   Generator point);

')


m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
public native void @WIDEN@_widening_assign(@CLASS@ y, Integer tp);

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
    public native
	void @LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(@CLASS@ y,
						 @UCONSTRAINER@_System cs,
						 Integer tp);

')


m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
    public native @UGET_REPRESENT@_System @GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
    public native @UGET_REPRESENT@_System minimized_@GET_REPRESENT@s();

')


m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
    public native Poly_Con_Relation relation_with(@URELATION_REPRESENT@ c);


')
