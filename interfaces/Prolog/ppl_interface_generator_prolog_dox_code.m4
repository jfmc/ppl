m4_divert(-1)

dnl This m4 file contains the program code for generating Prolog_interface.dox

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

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code', `dnl
<CODE>ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(+Dimension_Type, +Universe_or_Empty, -Handle)</CODE><BR>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code', `dnl
<CODE>ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@(+Handle, -Handle)</CODE><BR>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity_code', `dnl
<CODE>ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@(+Handle, -Handle, +Complexity)</CODE><BR>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
<CODE>ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(+@UBUILD_REPRESENT@, -Handle)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@UB_EXACT@_code',
`dnl
<CODE>ppl_@CLASS@_@UB_EXACT@(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
<CODE>ppl_@CLASS@_swap(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
<CODE>ppl_@CLASS@_@DIMENSION@(+Handle, ?Dimension_Type)</CODE><BR>
')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
<CODE>ppl_@CLASS@_relation_with_@RELATION_REPRESENT@(+Handle, +@URELATION_REPRESENT@, ?Relation_List)</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
<CODE>ppl_@CLASS@_get_@GET_REPRESENT@s(+Handle, +Complexity, @UGET_REPRESENT@_System?)</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
<CODE>ppl_@CLASS@_get_minimized_@GET_REPRESENT@s(+Handle, +Complexity, @UGET_REPRESENT@_System?)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
<CODE>ppl_@CLASS@_@HAS_PROPERTY@(+Handle)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
<CODE>ppl_@CLASS@_@SIMPLIFY@(+Handle)</CODE><BR>
')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
<CODE>ppl_@CLASS@_bounds_from_@ABOVEBELOW@(+Handle, +Lin_Expr)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
<CODE>ppl_@CLASS@_@MAXMIN@(+Handle, +Lin_Expr, ?Coefficient_1, ?Coefficient_2, ?Boolean)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
<CODE>ppl_@CLASS@_@MAXMIN@_with_point(+Handle, +Lin_Expr, ?Coefficient_1, ?Coefficient_2, ?Boolean, ?Point)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
<CODE>ppl_@CLASS@_@COMPARISON@_@CLASS@(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
<CODE>ppl_@CLASS@_equals_@CLASS@(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
<CODE>ppl_@CLASS@_OK(+Handle)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
<CODE>ppl_@CLASS@_add_@ADD_REPRESENT@(+Handle, +@UADD_REPRESENT@)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
<CODE>ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize(+Handle, +@UADD_REPRESENT@)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
<CODE>ppl_@CLASS@_add_@ADD_REPRESENT@s(+Handle, +@UADD_REPRESENT@_System)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
<CODE>ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(+Handle, +@UADD_REPRESENT@_System)</CODE><BR>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
`dnl
<CODE>ppl_@CLASS@_refine_with_@REFINE_REPRESENT@(+Handle, +@UREFINE_REPRESENT@)</CODE><BR>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
<CODE>ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s(+Handle, +@UREFINE_REPRESENT@_System)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
<CODE>ppl_@CLASS@_@BINOP@(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
<CODE>ppl_@CLASS@_@BINMINOP@(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_code',
`dnl
<CODE>ppl_@CLASS@_simplify_using_context_assign(+Handle_1, +Handle_2, ?Boolean)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
<CODE>ppl_@CLASS@_@AFFIMAGE@(+Handle, +PPL_Var, +Lin_Expr, +Coefficient)</CODE><BR>
')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
<CODE>ppl_@CLASS@_bounded_@AFFIMAGE@(+Handle, +PPL_Var, +Lin_Expr_1, +Lin_Expr_2, +Coefficient)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
<CODE>ppl_@CLASS@_generalized_@AFFIMAGE@(+Handle, +PPL_Var, +Relation_Symbol, +Lin_Expr, +Coefficient)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
<CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs(+Handle, +Lin_Expr_1, +Relation_Symbol, +Lin_Expr_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
 `dnl
<CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence(+Handle, +PPL_Var, +Relation_Symbol, +Lin_Expr, +Coefficient_1, +Coefficient_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
`dnl
<CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence(+Handle, +Lin_Expr_1, +Relation_Symbol, +Lin_Expr_2, +Coefficient)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
<CODE>ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@(+Handle, +Dimension_Type)</CODE><BR>
')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
<CODE>ppl_@CLASS@_remove_space_dimensions(+Handle, +List_of_PPL_Vars)</CODE><BR>
')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
<CODE>ppl_@CLASS@_remove_higher_space_dimensions(+Handle, +Dimension_Type)</CODE><BR>
')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
<CODE>ppl_@CLASS@_expand_space_dimension(+Handle, +PPL_Var, +Dimension_Type)</CODE><BR>
')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
<CODE>ppl_@CLASS@_fold_space_dimensions(+Handle, +List_of_PPL_Vars, +PPL_Var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
<CODE>ppl_@CLASS@_map_space_dimensions(+Handle, +P_Func)</CODE><BR>
')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
<CODE>ppl_@CLASS@_constrains(+Handle, +PPL_Var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`dnl
<CODE>ppl_@CLASS@_unconstrain_space_dimension(+Handle, +PPL_Var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`dnl
<CODE>ppl_@CLASS@_unconstrain_space_dimensions(+Handle, +List_of_PPL_Var)</CODE><BR>
')

m4_define(`ppl_@CLASS@_ascii_dump_code',
 `dnl
<CODE>ppl_@CLASS@_ascii_dump(+Handle)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
 `dnl
<CODE>ppl_@CLASS@_@MEMBYTES@(+Handle, ?Number)</CODE><BR>
')

m4_define(`ppl_@CLASS@_widening_assign_with_tokens_code',
`dnl
<CODE>ppl_@CLASS@_widening_assign_with_tokens(+Handle_1, +Handle_2, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_widening_assign_code',
`dnl
<CODE>ppl_@CLASS@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`dnl
<CODE>ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens(+Handle_1, +Handle_2, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
<CODE>ppl_@CLASS@_@WIDEN@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code',
`dnl
<CODE>ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens(+Handle_1, +Handle_2, +Constraint_System, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
<CODE>ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(+Handle_1, +Handle_2, +Constraint_System)</CODE><BR>
')

 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
 `dnl
<CODE>ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
 `dnl
<CODE>ppl_@CLASS@_iterator_equals_iterator(+Iterator_1, +Iterator_2)</CODE><BR>
')

 m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
 `dnl
<CODE>ppl_@CLASS@_@BEGINEND@_iterator(+Handle, ?Iterator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_delete_iterator_code',
 `dnl
<CODE>ppl_@CLASS@_iterator_@INCDEC@(+Iterator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code',
 `dnl
<CODE>ppl_@CLASS@_iterator_@INCDEC@(+Iterator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_get_disjunct_code',
 `dnl
<CODE>ppl_@CLASS@_iterator_get_disjunct(+Handle_1, -Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
 `dnl
<CODE>ppl_@CLASS@_drop_disjunct(+Handle, +Iterator)</CODE><BR>
')

m4_define(`ppl_@CLASS@_add_disjunct_code',
 `dnl
<CODE>ppl_@CLASS@_add_disjunct(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_linear_@PARTITION@_code',
 `dnl
<CODE>ppl_@CLASS@_linear_@PARTITION@(+Handle_1, +Handle_2, -Handle_3, -Handle_4)</CODE><BR>
')

m4_define(`ppl_@CLASS@_approximate_@PARTITION@',
 `dnl
<CODE>ppl_@CLASS@_approximate_@PARTITION@(+Handle_1, +Handle_2, ?Boolean, -Handle_3, -Handle_4)</CODE><BR>
')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_code',
 `dnl
<CODE>ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_code',
 `dnl
<CODE>ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign(+Handle_1, +Handle_2)</CODE><BR>
')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_EXTRAPOLATION@_extrapolation_assign_code',
 `dnl
<CODE>ppl_@CLASS@_BGP99_@DISJUNCT_EXTRAPOLATION@_extrapolation_assign(+Handle_1, +Handle_2)</CODE><BR>
')
