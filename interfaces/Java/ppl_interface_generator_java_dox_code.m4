m4_divert(-1)

dnl This m4 file contains the program code for generating ppl_prolog.icc

dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
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

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
<CODE>    public @TOPOLOGY@@CLASS@(long num_dimensions,
			Degenerate_Element kind) </CODE><BR>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
<CODE>    public @TOPOLOGY@@CLASS@(@FRIEND@ y)  </CODE><BR>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
<CODE>    public @TOPOLOGY@@CLASS@(@UBUILD_REPRESENT@_System cs) </CODE><BR>
')

m4_define(`ppl_delete_@CLASS@_code',
`dnl
<CODE>    protected native void finalize()</CODE><BR>
')

m4_define(`ppl_free_@CLASS@_code',
`dnl
<CODE>    public native void free()</CODE>
')

m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
<CODE>    public native boolean @UB_EXACT@(@TOPOLOGY@@CLASS@ y)</CODE><BR>
')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
<CODE>    public native void swap(@CLASS@ y)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
<CODE>    public native long @DIMENSION@()</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
<CODE>    public native @UGET_REPRESENT@_System @GET_REPRESENT@s()</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
<CODE>    public native @UGET_REPRESENT@_System minimized_@GET_REPRESENT@s()
</CODE><BR>
')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
<CODE>    public native Poly_@UALT_RELATION_REPRESENT@_Relation
                         relation_with(@URELATION_REPRESENT@ c)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
<CODE>    public native boolean @HAS_PROPERTY@()</CODE><BR>
')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
<CODE>    public native void @SIMPLIFY@()</CODE><BR>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`dnl
<CODE>    public native void unconstrain_space_dimension(Variable var)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`dnl
<CODE>    public native void
      unconstrain_space_dimensions(Variables_Set to_be_constrained)</CODE><BR>
')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
<CODE>    public native boolean bounds_from_@ABOVEBELOW@(Linear_Expression
                                                           expr)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
<CODE>    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> @MAXMIN@)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
<CODE>    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> @MAXMIN@,
				   Generator point)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
<CODE>    public native boolean @COMPARISON@(@CLASS@ y)</CODE><BR>
')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
<CODE>    public native boolean equals(@CLASS@ p)</CODE><BR>
')

m4_define(`ppl_@CLASS@_hashcode_code',
`dnl
<CODE>    public native int hashCode()</CODE><BR>
')


m4_define(`ppl_@CLASS@_OK_code',
`dnl
<CODE>    public native boolean OK()</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
<CODE>    public native void add_@ADD_REPRESENT@(@UADD_REPRESENT@ c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
`dnl
<CODE>    public native void refine_with_@REFINE_REPRESENT@(@UREFINE_REPRESENT@ c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
<CODE>    public native boolean add_@ADD_REPRESENT@_and_minimize(@UADD_REPRESENT@ c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
<CODE>    public native void add_@ADD_REPRESENT@s(@UADD_REPRESENT@_System c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
<CODE>    public native void add_@ADD_REPRESENT@s(@UADD_REPRESENT@_System c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
<CODE>    public native void refine_with_@REFINE_REPRESENT@s(@UREFINE_REPRESENT@_System c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
<CODE>    public native boolean add_@ADD_REPRESENT@s_and_minimize(@UADD_REPRESENT@_System c)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
<CODE>    public native void @BINOP@(@CLASS@ p)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
<CODE>    public native boolean @BINMINOP@(@CLASS@ p)</CODE><BR>
')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_code',
`dnl
<CODE>    public native boolean simplify_using_context_assign(@CLASS@ p)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
<CODE>    public native void @AFFIMAGE@(Variable var, Linear_Expression expr,
				    Coefficient denominator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
<CODE>    public native void bounded_@AFFIMAGE@(Variable var,
					    Linear_Expression lb_expr,
					    Linear_Expression ub_expr,
					    Coefficient denominator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
<CODE>    public native void generalized_@AFFIMAGE@(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator)</CODE><BR>
')


m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
<CODE>    public native void generalized_@AFFIMAGE@(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
<CODE>    public native void
    @EXTRAPOLATION@_narrowing_assign(@TOPOLOGY@@CLASS@ arg)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
`dnl
<CODE>    public native void generalized_@AFFIMAGE@_with_congruence(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator,
				Coefficient modulus)</CODE><BR>
')


m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
`dnl
<CODE>    public native void generalized_@AFFIMAGE@_lhs_rhs_with_congruence(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs,
				Coefficient modulus)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
<CODE>public native void @WIDEN@_widening_assign(@CLASS@ y,
                                By_Reference<Integer> tp)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
<CODE>    public native
	void @LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(@CLASS@ y,
						 @UCONSTRAINER@_System cs,
						 By_Reference<Integer> tp)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
<CODE>    public native void add_space_dimensions_@EMBEDPROJECT@(long m)</CODE><BR>
')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
<CODE>    public native void remove_space_dimensions(Variables_Set to_be_removed)</CODE><BR>
')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
<CODE>    public native void remove_higher_space_dimensions(long
						      new_dimension)</CODE><BR>
')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
<CODE>    public native void expand_space_dimension(Variable var, long m)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
<CODE>    public native void fold_space_dimensions(Variables_Set to_be_folded,
					     Variable var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
<CODE>    public native void map_space_dimensions(Partial_Function pfunc)
</CODE><BR>
')

m4_define(`ppl_@CLASS@_string_code',
`dnl
<CODE>    public native String toString()</CODE><BR>
')

m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
`dnl
<CODE>    public native @CLASS@_Iterator @BEGINEND@_iterator()</CODE><BR>
')

m4_define(`ppl_@CLASS@_delete_iterator_code',
`dnl
<CODE>    public native void delete_iterator()</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`dnl
<CODE>  public native @CLASSTOPOLOGY@@DISJUNCT@ get_disjunct()</CODE><BR>
')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
`dnl
<CODE>  public native void drop_disjunct(@CLASS@_Iterator itr)</CODE><BR>
')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
`dnl
<CODE>  public native boolean equals(@CLASS@_Iterator itr)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code',
`dnl
<CODE>  public native void @ALT_INCDEC@()</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
`dnl
<CODE>  public native long @MEMBYTES@()</CODE><BR>
')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
<CODE>  public native boolean constrains(Variable var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_ascii_dump_code',
`dnl
<CODE>  public native String ascii_dump()</CODE><BR>
')

m4_define(`ppl_@CLASS@_linear_@PARTITION@_code',
`dnl
<CODE>  public static native Pair <@TOPOLOGY@@CLASS@@COMMA@ Pointset_Powerset<NNC_Polyhedron> linear_@PARTITION@(@CLASS@ p, @CLASS@ q)</CODE><BR>
')

m4_define(`ppl_@CLASS@_approximate_@PARTITION@_code',
`dnl
<CODE>  public static native Pair <@@CLASS@@COMMA@ Pointset_Powerset<Grid> approximate_@PARTITION@(@CLASS@ p, @CLASS@ q, By_Reference<Boolean> is_finite)</CODE><BR>
')
