m4_divert(-1)dnl

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
val ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension:
  int -> @LTOPOLOGY@@LCLASS@

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
val ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s:
  @BUILD_REPRESENT@_system -> @LTOPOLOGY@@LCLASS@

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_relation_with_@RELATION_REPRESENT@:
      @LTOPOLOGY@@LCLASS@ -> linear_@RELATION_REPRESENT@ -> poly_@ALT_RELATION_REPRESENT@_relation list

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@DIMENSION@:
  @LTOPOLOGY@@LCLASS@ -> int
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@HAS_PROPERTY@:
  @LTOPOLOGY@@LCLASS@ -> bool

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_bounds_from_@ABOVEBELOW@:
  @LTOPOLOGY@@LCLASS@ -> linear_expression -> bool

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@:
  @LTOPOLOGY@@LCLASS@ -> linear_@ADD_REPRESENT@ -> unit
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@_and_minimize:
  @LTOPOLOGY@@LCLASS@ -> linear_@ADD_REPRESENT@ -> unit
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s:
  @LTOPOLOGY@@LCLASS@ -> @ADD_REPRESENT@_system -> unit

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@s_and_minimize:
  @LTOPOLOGY@@LCLASS@ -> @ADD_REPRESENT@_system -> unit

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@COMPARISON@_@TOPOLOGY@@CLASS@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_equals_@TOPOLOGY@@CLASS@:
  @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool

')


m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@BINOP@:
   @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@BINMINOP@:
   @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_add_space_dimensions_@EMBEDPROJECT@:
  @LTOPOLOGY@@LCLASS@ -> int -> unit

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_remove_space_dimensions:
     @LTOPOLOGY@@LCLASS@ -> int list -> unit

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_remove_higher_space_dimensions:
  @LTOPOLOGY@@LCLASS@ -> int -> unit

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_fold_space_dimensions:
     @LTOPOLOGY@@LCLASS@ -> int list -> int -> unit

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_map_space_dimensions:
 @LTOPOLOGY@@LCLASS@ -> (int*int) list -> unit
')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_expand_space_dimension:
 @LTOPOLOGY@@LCLASS@ -> int -> int -> unit
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_get_@GET_REPRESENT@s:
@LTOPOLOGY@@LCLASS@ -> @GET_REPRESENT@_system

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_get_minimized_@GET_REPRESENT@s:
   @LTOPOLOGY@@LCLASS@ -> @GET_REPRESENT@_system

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_bounded_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> linear_expression -> linear_expression
      -> Z.t -> unit
')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> linear_expression -> Z.t -> unit

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@_lhs_rhs:
  @LTOPOLOGY@@LCLASS@ -> linear_expression -> relation_symbol -> linear_expression
    -> unit

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@:
  @LTOPOLOGY@@LCLASS@ -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit

')


m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign:
 @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign_with_tokens:
 @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> int -> int

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens:
 @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> constraint_system -> int -> int

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign:
 @LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> constraint_system -> unit

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@MAXMIN@:
 @LTOPOLOGY@@LCLASS@ -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_OK:
 @LTOPOLOGY@@LCLASS@ -> bool
')


m4_define(`ppl_@CLASS@_swap_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_swap:
@LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> unit

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`dnl
val ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@:
@LINTOPOLOGY@@LFRIEND@ -> @LTOPOLOGY@@LCLASS@

')


m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
val ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@:
@LTOPOLOGY@@LCLASS@ -> @LTOPOLOGY@@LCLASS@ -> bool

')
 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
 `dnl
 val ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign:
 @LTOPOLOGY@@LCLASS@  -> @LTOPOLOGY@@LCLASS@ -> unit

 ')
