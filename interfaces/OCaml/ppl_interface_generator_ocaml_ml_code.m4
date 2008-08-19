dnl  -*- Tuareg -*-
m4_divert(-1)

dnl This m4 file contains the program code for generating ppl_ocaml.ml

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
external ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension:
  int -> degenerate_element -> @LTOPOLOGY@@LCLASS@
  = "ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension"

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
external ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s:
  @BUILD_REPRESENT@_system -> @LTOPOLOGY@@LCLASS@
  = "ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_relation_with_@RELATION_REPRESENT@:
  @LTOPOLOGY@@LCLASS@ -> linear_@RELATION_REPRESENT@
  -> poly_@ALT_RELATION_REPRESENT@_relation list
  = "ppl_@TOPOLOGY@@CLASS@_relation_with_@RELATION_REPRESENT@"

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@DIMENSION@:
	    @LTOPOLOGY@@LCLASS@ -> int = "ppl_@TOPOLOGY@@CLASS@_@DIMENSION@"
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@HAS_PROPERTY@:
  @LTOPOLOGY@@LCLASS@ -> bool = "ppl_@TOPOLOGY@@CLASS@_@HAS_PROPERTY@"


')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@SIMPLIFY@:
  @LTOPOLOGY@@LCLASS@ -> unit = "ppl_@TOPOLOGY@@CLASS@_@SIMPLIFY@"


')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_bounds_from_@ABOVEBELOW@:
  @LTOPOLOGY@@LCLASS@ -> linear_expression -> bool
  = "ppl_@TOPOLOGY@@CLASS@_bounds_from_@ABOVEBELOW@"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@:
  @LTOPOLOGY@@LCLASS@ -> linear_@ADD_REPRESENT@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@_and_minimize:
  @LTOPOLOGY@@LCLASS@ -> linear_@ADD_REPRESENT@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@_and_minimize"
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s:
  @LTOPOLOGY@@LCLASS@ -> @ADD_REPRESENT@_system -> unit
  = "ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s_and_minimize:
  @LTOPOLOGY@@LCLASS@ -> @ADD_REPRESENT@_system -> unit
  = "ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s_and_minimize"

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_refine_with_@REFINE_REPRESENT@:
  @LTOPOLOGY@@LCLASS@ -> linear_@REFINE_REPRESENT@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_refine_with_@REFINE_REPRESENT@"

')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_refine_with_@REFINE_REPRESENT@s:
  @LTOPOLOGY@@LCLASS@ -> @REFINE_REPRESENT@_system -> unit
  = "ppl_@TOPOLOGY@@CLASS@_refine_with_@REFINE_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@COMPARISON@_@TOPOLOGY@@CLASS@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
  = "ppl_@TOPOLOGY@@CLASS@_@COMPARISON@_@TOPOLOGY@@CLASS@"

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_equals_@TOPOLOGY@@CLASS@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
  = "ppl_@TOPOLOGY@@CLASS@_equals_@TOPOLOGY@@CLASS@"

')


m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@BINOP@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_@BINOP@"

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@BINMINOP@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
  = "ppl_@TOPOLOGY@@CLASS@_@BINMINOP@"

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_add_space_dimensions_@EMBEDPROJECT@:
  @LTOPOLOGY@@LCLASS@ -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_add_space_dimensions_@EMBEDPROJECT@"

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_remove_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> int list -> unit
  = "ppl_@TOPOLOGY@@CLASS@_remove_space_dimensions"

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_remove_higher_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_remove_higher_space_dimensions"

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_fold_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> int list -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_fold_space_dimensions"

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_map_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> (int*int) list -> unit
  = "ppl_@TOPOLOGY@@CLASS@_map_space_dimensions"

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_expand_space_dimension:
  @LTOPOLOGY@@LCLASS@ -> int -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_expand_space_dimension"

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_get_@GET_REPRESENT@s:
  @LTOPOLOGY@@LCLASS@ -> @GET_REPRESENT@_system
  = "ppl_@TOPOLOGY@@CLASS@_get_@GET_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_get_minimized_@GET_REPRESENT@s:
  @LTOPOLOGY@@LCLASS@ -> @GET_REPRESENT@_system
  = "ppl_@TOPOLOGY@@CLASS@_get_minimized_@GET_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_constrains:
  @LTOPOLOGY@@LCLASS@ -> int -> bool
  = "ppl_@TOPOLOGY@@CLASS@_constrains"

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimension:
  @LTOPOLOGY@@LCLASS@ -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimension"

')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> int list -> unit
  = "ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimensions"

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_bounded_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> linear_expression
  -> linear_expression -> Z.t -> unit
  = "ppl_@TOPOLOGY@@CLASS@_bounded_@AFFIMAGE@"

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> linear_expression -> Z.t -> unit
  = "ppl_@TOPOLOGY@@CLASS@_@AFFIMAGE@"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@_lhs_rhs:
  @LTOPOLOGY@@LCLASS@ -> linear_expression
  -> relation_symbol -> linear_expression -> unit
  = "ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@1"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> relation_symbol
  -> linear_expression -> Z.t -> unit
  = "ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@2"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@_with_congruence:
  @LTOPOLOGY@@LCLASS@ -> int -> relation_symbol
  -> linear_expression -> Z.t -> Z.t -> unit
  = "ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@1_with_congruence"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence:
  @LTOPOLOGY@@LCLASS@ -> linear_expression -> relation_symbol
  -> linear_expression -> Z.t -> unit
  = "ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@1_lhs_rhs_with_congruence"

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign"

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign_with_tokens:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> int -> int
  = "ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign_with_tokens"

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> @CONSTRAINER@_system -> unit
  = "ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign"

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> @CONSTRAINER@_system -> int -> int
  = "ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens"

')


m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@MAXMIN@:
  @LTOPOLOGY@@LCLASS@ -> linear_expression
  -> bool * Z.t * Z.t * bool
  = "ppl_@TOPOLOGY@@CLASS@_@MAXMIN@"

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@MAXMIN@_with_point:
  @LTOPOLOGY@@LCLASS@ -> linear_expression
  -> bool * Z.t * Z.t * bool * linear_generator
  = "ppl_@TOPOLOGY@@CLASS@_@MAXMIN@_with_point"

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_OK:
  @LTOPOLOGY@@LCLASS@ -> bool = "ppl_@TOPOLOGY@@CLASS@_OK"

')


m4_define(`ppl_@CLASS@_@MEMBYTES@_code',
 `
external ppl_@TOPOLOGY@@CLASS@_@MEMBYTES@:
  @LTOPOLOGY@@LCLASS@  -> int = "ppl_@TOPOLOGY@@CLASS@_@MEMBYTES@"

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_swap:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_swap"

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
external ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@:
  @LFRIEND@ -> @LTOPOLOGY@@LCLASS@
  = "ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@"

')


m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
  = "ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@"

')


 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_code',
 `dnl
 external ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens:
   @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> int -> int
   = "ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens"

 ')


 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_code',
 `dnl
 external ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign:
   @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit
   = "ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign"

 ')

 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
 `dnl
 external ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign:
   @LTOPOLOGY@@LCLASS@  -> @LTOPOLOGY@@LCLASS@ -> unit
   = "ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign"

 ')

 m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_code',
 `
external ppl_@CLASS@_@BEGINEND@_iterator:
  @LCLASS@  -> @LCLASS@_iterator
  = "ppl_@CLASS@_@BEGINEND@_iterator"

')

m4_define(`ppl_@CLASS@_get_disjunct_code',
 `
external ppl_@CLASS@_iterator_get_disjunct:
   @LCLASS@_iterator  ->  @LCLASSTOPOLOGY@@LDISJUNCT@
   = "ppl_@CLASS@_iterator_get_disjunct"


')

m4_define(`ppl_@CLASS@_add_disjunct_code',
 `
external ppl_@CLASS@_add_disjunct:
   @LCLASS@  ->  @LCLASSTOPOLOGY@@LDISJUNCT@ -> unit
   = "ppl_@CLASS@_add_disjunct"


')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
 `
 external ppl_@CLASS@_drop_disjunct:
   @LCLASS@  ->  @LCLASS@_iterator -> unit
   = "ppl_@CLASS@_drop_disjunct"


')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_code',
 `
 external ppl_@CLASS@_iterator_@INCDEC@:
   @LCLASS@_iterator -> unit
   = "ppl_@CLASS@_iterator_@INCDEC@"


')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
 `
type @LCLASS@_iterator

external ppl_@CLASS@_iterator_equals_iterator:
  @LCLASS@_iterator ->  @LCLASS@_iterator -> bool
  = "ppl_@CLASS@_iterator_equals_iterator"

')

# m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_code',
# `dnl
# external ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@:
  # @LFRIEND@ -> @LTOPOLOGY@@LCLASS@
    # = "ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@"

# ')

# m4_define(`ppl_@CLASS@_get_disjuncts_code',
# `dnl
# external ppl_@CLASS@_get_disjuncts:
  # @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
    # = "ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@"

# ')

# m4_define(`ppl_@CLASS@_get_bounding_box_code',
# `dnl
# external ppl_@CLASS@_get_bounding_box:
  # @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
    # = "ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@"

# ')

# m4_define(`ppl_@CLASS@_get_covering_box_code',
# `dnl
# external ppl_@CLASS@_get_covering_box:
  # @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool
    # = "ppl_@CLASS@_get_covering_box"

# ')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit
  = "ppl_@TOPOLOGY@@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign"

')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_code',
`dnl
external ppl_@TOPOLOGY@@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> int -> unit
  = "ppl_@TOPOLOGY@@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign"

')
