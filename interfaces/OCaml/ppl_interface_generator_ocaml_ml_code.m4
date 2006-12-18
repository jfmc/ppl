m4_divert(-1)dnl

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
external ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension:
  int -> @LCLASS@ = "ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension"

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
external ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s:
  constraint_system -> @LCLASS@ = "ppl_new_C_@CLASS@_from_@BUILD_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
external ppl_@CLASS@_relation_with_@RELATION_REPRESENT@:
      @LCLASS@ -> linear_constraint -> relation_with_@RELATION_REPRESENT@ list
	  = "ppl_@CLASS@_relation_with_@RELATION_REPRESENT@"

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
external ppl_@CLASS@_@DIMENSION@:
  @LCLASS@ -> int = "ppl_@CLASS@_@DIMENSION@"

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
external ppl_@CLASS@_@HAS_PROPERTY@:
  @LCLASS@ -> bool = "ppl_@CLASS@_@HAS_PROPERTY@"


')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
external ppl_@CLASS@_bounds_from_@ABOVEBELOW@:
  @LCLASS@ -> linear_expression -> bool = "ppl_@CLASS@_bounds_from_@ABOVEBELOW@"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
external ppl_@CLASS@_add_@ADD_REPRESENT@:
  @LCLASS@ -> linear_@ADD_REPRESENT@ -> unit = "ppl_@CLASS@_add_@ADD_REPRESENT@"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
external ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize:
  @LCLASS@ -> linear_@ADD_REPRESENT@ -> unit
      = "ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
external ppl_@CLASS@_add_@ADD_REPRESENT@s:
  @LCLASS@ -> @ADD_REPRESENT@_system -> unit = "ppl_@CLASS@_add_@ADD_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
external ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize:
  @LCLASS@ -> @ADD_REPRESENT@_system -> unit
      = "ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize"

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
external ppl_@CLASS@_is_disjoint_from:
  @LCLASS@ -> @LCLASS@ -> bool
      = "ppl_@CLASS@_is_disjoint_from"

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
external ppl_@CLASS@_@BINMINOP@:
   @LCLASS@ -> @LCLASS@ -> bool
       = "ppl_@CLASS@_@BINMINOP@"

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
external ppl_@CLASS@_@BINOP@:
   @LCLASS@ -> @LCLASS@ -> unit
       = "ppl_@CLASS@_@BINOP@"

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
external ppl_@CLASS@_add_space_dimensions_and_@EMBEDPROJECT@:
  @LCLASS@ -> int -> unit = "ppl_@CLASS@_add_space_dimensions_and_@EMBEDPROJECT@"

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
external ppl_@CLASS@_remove_space_dimensions:
     @LCLASS@ -> int list -> unit
	 = "ppl_@CLASS@_remove_space_dimensions"

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
external ppl_@CLASS@_remove_higher_space_dimensions:
  @LCLASS@ -> int -> unit = "ppl_@CLASS@_remove_higher_space_dimensions"

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
external ppl_@CLASS@_fold_space_dimensions:
     @LCLASS@ -> int list -> int -> unit
	 = "ppl_@CLASS@_fold_space_dimensions"

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
external ppl_@CLASS@_map_space_dimensions:
 @LCLASS@ -> (int*int) list -> unit = "ppl_@CLASS@_map_space_dimensions"

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
external ppl_@CLASS@_get_@GET_REPRESENT@s:
   @LCLASS@ -> @GET_REPRESENT@_system = "ppl_@CLASS@_get_@GET_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
external ppl_@CLASS@_get_minimized_@GET_REPRESENT@s:
   @LCLASS@ -> @GET_REPRESENT@_system = "ppl_@CLASS@_get_minimized_@GET_REPRESENT@s"

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
external ppl_@CLASS@_@AFFIMAGE@:
  @LCLASS@ -> int -> linear_expression -> Z.t -> unit
      = "ppl_@CLASS@_@AFFIMAGE@"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
external ppl_@CLASS@_generalized_@AFFIMAGE@:
  @LCLASS@ -> linear_expression -> relation_symbol -> linear_expression
    -> unit
      = "ppl_@CLASS@_generalized_@AFFIMAGE@1"

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
external ppl_@CLASS@_generalized_@AFFIMAGE@:
  @LCLASS@ -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit
      = "ppl_@CLASS@_generalized_@AFFIMAGE@2"

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
external ppl_@CLASS@_bounded_@AFFIMAGE@:
  @LCLASS@ -> int -> linear_expression -> linear_expression
      -> Z.t -> unit = "ppl_@CLASS@_bounded_@AFFIMAGE@"

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
external ppl_@CLASS@_@WIDEN@_widening_assign:
 @LCLASS@ -> @LCLASS@ -> int -> int
     = "ppl_@CLASS@_@WIDEN@_widening_assign"

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
external ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign:
 @LCLASS@ -> @LCLASS@ -> constraint_system -> int -> int
     = "ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign"

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
external ppl_@CLASS@_@MAXMIN@:
 @LCLASS@ -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator
     = "ppl_@CLASS@_@MAXMIN@"

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
external ppl_@CLASS@_OK:
 @LCLASS@ -> bool = "ppl_@CLASS@_OK"

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
external ppl_@CLASS@_swap:
@LCLASS@ -> @LCLASS@ -> unit = "ppl_@CLASS@_swap"

')

external test_linear_expression:
  linear_expression -> unit = "test_linear_expression"

external test_linear_constraint:
  linear_constraint -> unit = "test_linear_constraint"

external test_linear_generator:
  linear_generator -> unit = "test_linear_generator"

external test_constraint_system:
  constraint_system -> unit = "test_constraint_system"

external test_generator_system:
  generator_system -> unit = "test_generator_system"
