m4_divert(-1)dnl

m4_define(`m4_class_build_cpp_object_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    private native void build_cpp_object(@UBUILD_REPRESENT@_System cs);

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public C_Polyhedron(long num_dimensions,
			Degenerate_Element kind) {
	build_cpp_object(num_dimensions, kind);
    }

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public @TOPOLOGY@@CLASS@(@TOPOLOGY@@CLASS@ y) {
        build_cpp_object(y);
    }

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public @TOPOLOGY@@CLASS@(@UBUILD_REPRESENT@_System cs) {
	build_cpp_object(cs);
    }

')

m4_define(`ppl_delete_@CLASS@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    protected native void finalize();

')

m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public native boolean @UB_EXACT@(@TOPOLOGY@@CLASS@ y);

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public native void swap(Polyhedron y);

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native long @DIMENSION@();

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @UGET_REPRESENT@_System @GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @UGET_REPRESENT@_System minimized_@GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native Poly_Con_Relation relation_with(@URELATION_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @HAS_PROPERTY@();

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @SIMPLIFY@();

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean bounds_from_@ABOVEBELOW@(Linear_Expression expr);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   Boolean maximum);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   Boolean maximum,
				   Generator point);

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @COMPARISON@(@CLASS@ y);

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean equals(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean OK(boolean check_not_empty);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@_and_minimize(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@s(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@s_and_minimize(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @BINOP@(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @BINOP@_and_minimize(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @AFFIMAGE@(Variable var, Linear_Expression expr,
				    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void bounded_@AFFIMAGE@(Variable var,
					    Linear_Expression lb_expr,
					    Linear_Expression ub_expr,
					    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs);

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
%<--%<--%<-- @CLASS@.java
public native void @WIDEN@_widening_assign(@CLASS@ y, Integer tp);

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native
	void @LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(@CLASS@ y,
						 @UCONSTRAINER@_System cs,
						 Integer tp);

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_space_dimensions_@EMBEDPROJECT@(long m);

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void remove_space_dimensions(Variables_Set to_be_removed);

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void remove_higher_space_dimensions(long
						      new_dimension);

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void expand_space_dimension(Variable var, long m);

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void fold_space_dimensions(Variables_Set to_be_folded,
					     Variable var);

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void map_space_dimensions(Partial_Function pfunc);

')
