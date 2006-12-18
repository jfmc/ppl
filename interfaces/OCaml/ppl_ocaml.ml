include Ppl_ocaml_globals
include Ppl_ocaml_types
open Gmp

exception Error of string
let _ = Callback.register_exception "PPL_arithmetic_overflow" (Error "any string")
let _ = Callback.register_exception "PPL_internal_error" (Error "any string")
let _ = Callback.register_exception "PPL_unknown_standard_exception" (Error "any string")
let _ = Callback.register_exception "PPL_unexpected_error" (Error "any string")

type polyhedron

external ppl_new_C_Polyhedron_from_space_dimension:
  int -> polyhedron = "ppl_new_C_Polyhedron_from_space_dimension"

external ppl_new_C_Polyhedron_from_constraints:
  constraint_system -> polyhedron = "ppl_new_C_Polyhedron_from_constraints"
external ppl_new_C_Polyhedron_from_generators:
  generator_system -> polyhedron = "ppl_new_C_Polyhedron_from_generators"

external ppl_new_C_Polyhedron_from_congruences:
  congruence_system -> polyhedron = "ppl_new_C_Polyhedron_from_congruences"

external ppl_Polyhedron_relation_with_constraint:
      polyhedron -> linear_constraint -> relation_with_constraint list
	  = "ppl_Polyhedron_relation_with_constraint"

external ppl_Polyhedron_relation_with_generator:
      polyhedron -> linear_generator -> relation_with_generator list
	  = "ppl_Polyhedron_relation_with_generator"

external ppl_Polyhedron_space_dimension:
  polyhedron -> int = "ppl_Polyhedron_space_dimension"

external ppl_Polyhedron_affine_dimension:
  polyhedron -> int = "ppl_Polyhedron_affine_dimension"

external ppl_Polyhedron_is_empty:
  polyhedron -> bool = "ppl_Polyhedron_is_empty"

external ppl_Polyhedron_is_universe:
  polyhedron -> bool = "ppl_Polyhedron_is_universe"

external ppl_Polyhedron_contains_interger_point:
  polyhedron -> bool = "ppl_Polyhedron_contains_integer_point"

external ppl_Polyhedron_is_topologically_closed:
  polyhedron -> bool = "ppl_Polyhedron_is_topologically_closed"

external ppl_Polyhedron_is_bounded:
  polyhedron -> bool = "ppl_Polyhedron_is_bounded"

external ppl_Polyhedron_bounds_from_above:
  polyhedron -> linear_expression -> bool = "ppl_Polyhedron_bounds_from_above"

external ppl_Polyhedron_bounds_from_below:
  polyhedron -> linear_expression -> bool = "ppl_Polyhedron_bounds_from_below"

external ppl_Polyhedron_add_constraint:
  polyhedron -> linear_constraint -> unit = "ppl_Polyhedron_add_constraint"

external ppl_Polyhedron_add_constraint_and_minimize:
  polyhedron -> linear_constraint -> unit
      = "ppl_Polyhedron_add_constraint_and_minimize"

external ppl_Polyhedron_add_constraints:
  polyhedron -> constraint_system -> unit = "ppl_Polyhedron_add_constraints"

external ppl_Polyhedron_add_constraints_and_minimize:
  polyhedron -> constraint_system -> unit
      = "ppl_Polyhedron_add_constraints_and_minimize"

external ppl_Polyhedron_add_generator:
  polyhedron -> linear_generator -> unit = "ppl_Polyhedron_add_generator"

external ppl_Polyhedron_add_generator_and_minimize:
  polyhedron -> linear_generator -> unit
      = "ppl_Polyhedron_add_generator_and_minimize"

external ppl_Polyhedron_add_generators:
  polyhedron -> generator_system -> unit = "ppl_Polyhedron_add_generators"

external ppl_Polyhedron_add_generators_and_minimize:
  polyhedron -> generator_system -> unit
      = "ppl_Polyhedron_add_generators_and_minimize"

external ppl_Polyhedron_add_congruences:
  polyhedron -> congruence_system -> unit
      = "ppl_Polyhedron_add_congruences"

external ppl_Polyhedron_is_disjoint_from:
  polyhedron -> polyhedron -> bool
      = "ppl_Polyhedron_is_disjoint_from"

external ppl_Polyhedron_contains:
  polyhedron -> polyhedron -> bool
      = "ppl_Polyhedron_contains"

external ppl_Polyhedron_intersection_assign_and_minimize:
   polyhedron -> polyhedron -> bool
       = "ppl_Polyhedron_intersection_assign_and_minimize"

external ppl_Polyhedron_intersection_assign:
   polyhedron -> polyhedron -> unit
       = "ppl_Polyhedron_intersection_assign"

external ppl_Polyhedron_poly_hull_assign_and_minimize:
    polyhedron -> polyhedron -> bool
	= "ppl_Polyhedron_poly_hull_assign_and_minimize"

external ppl_Polyhedron_poly_hull_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_poly_hull_assign"

external ppl_Polyhedron_upper_bound_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_upper_bound_assign"

external ppl_Polyhedron_poly_difference_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_poly_difference_assign"

external ppl_Polyhedron_difference_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_difference_assign"

external ppl_Polyhedron_time_elapse_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_time_elapse_assign"

external ppl_Polyhedron_concatenate_assign:
    polyhedron -> polyhedron -> unit
	= "ppl_Polyhedron_concatenate_assign"

external ppl_Polyhedron_add_space_dimensions_and_embed:
  polyhedron -> int -> unit = "ppl_Polyhedron_add_space_dimensions_and_embed"

external ppl_Polyhedron_add_space_dimensions_and_project:
  polyhedron -> int -> unit = "ppl_Polyhedron_add_space_dimensions_and_project"

external ppl_Polyhedron_remove_higher_space_dimensions:
  polyhedron -> int -> unit = "ppl_Polyhedron_remove_higher_space_dimensions"

external ppl_Polyhedron_remove_higher_space_dimensions:
  polyhedron -> int -> unit = "ppl_Polyhedron_remove_higher_space_dimensions"

external ppl_Polyhedron_space_dimension:
  polyhedron -> int = "ppl_Polyhedron_space_dimension"

external ppl_Polyhedron_get_constraints:
   polyhedron -> constraint_system = "ppl_Polyhedron_get_constraints"

external ppl_Polyhedron_get_minimized_constraints:
   polyhedron -> constraint_system = "ppl_Polyhedron_get_minimized_constraints"

external ppl_Polyhedron_get_generators:
   polyhedron -> generator_system = "ppl_Polyhedron_get_generators"

external ppl_Polyhedron_get_minimized_generators:
   polyhedron -> generator_system = "ppl_Polyhedron_get_minimized_generators"

external ppl_Polyhedron_get_congruences:
   polyhedron -> congruence_system = "ppl_Polyhedron_get_congruences"

external ppl_Polyhedron_get_minimized_congruences:
   polyhedron -> congruence_system = "ppl_Polyhedron_get_minimized_congruences"

external ppl_Polyhedron_affine_image:
  polyhedron -> int -> linear_expression -> Z.t -> unit
      = "ppl_Polyhedron_affine_image"

external ppl_Polyhedron_affine_preimage:
  polyhedron -> int -> linear_expression -> Z.t -> unit
      = "ppl_Polyhedron_affine_preimage"

external ppl_Polyhedron_generalized_affine_image:
  polyhedron -> linear_expression -> relation_symbol -> linear_expression
    -> unit
      = "ppl_Polyhedron_generalized_affine_image1"

external ppl_Polyhedron_generalized_affine_image:
  polyhedron -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit
      = "ppl_Polyhedron_generalized_affine_image2"

external ppl_Polyhedron_generalized_affine_preimage:
  polyhedron -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit
      = "ppl_Polyhedron_generalized_affine_preimage2"

external ppl_Polyhedron_generalized_affine_preimage:
  polyhedron -> linear_expression -> relation_symbol -> linear_expression
    -> unit
      = "ppl_Polyhedron_generalized_affine_preimage1"

external ppl_Polyhedron_bounded_affine_image:
  polyhedron -> int -> linear_expression -> linear_expression
      -> Z.t -> unit = "ppl_Polyhedron_bounded_affine_image"

external ppl_Polyhedron_bounded_affine_preimage:
  polyhedron -> int -> linear_expression -> linear_expression
      -> Z.t -> unit = "ppl_Polyhedron_bounded_affine_preimage"

external ppl_Polyhedron_BHRZ03_widening_assign:
 polyhedron -> polyhedron -> int -> int
     = "ppl_Polyhedron_BHRZ03_widening_assign"

external ppl_Polyhedron_limited_BHRZ03_extrapolation_assign:
 polyhedron -> polyhedron -> constraint_system -> int -> int
     = "ppl_Polyhedron_limited_BHRZ03_extrapolation_assign"

external ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign:
 polyhedron -> polyhedron -> constraint_system -> int -> int
     = "ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign"

external ppl_Polyhedron_limited_H79_extrapolation_assign:
 polyhedron -> polyhedron -> constraint_system -> int -> int
     = "ppl_Polyhedron_limited_H79_extrapolation_assign"

external ppl_Polyhedron_bounded_H79_extrapolation_assign:
 polyhedron -> polyhedron -> constraint_system -> int -> int
     = "ppl_Polyhedron_bounded_H79_extrapolation_assign"

external ppl_Polyhedron_H79_widening_assign:
 polyhedron -> polyhedron -> int -> int
     = "ppl_Polyhedron_H79_widening_assign"

external ppl_Polyhedron_maximize:
 polyhedron -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator
     = "ppl_Polyhedron_maximize"

external ppl_Polyhedron_minimize:
 polyhedron -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator
     = "ppl_Polyhedron_minimize"

external ppl_Polyhedron_remove_space_dimensions:
     polyhedron -> int list -> unit
	 = "ppl_Polyhedron_remove_space_dimensions"

external ppl_Polyhedron_fold_space_dimensions:
     polyhedron -> int list -> int -> unit
	 = "ppl_Polyhedron_fold_space_dimensions"

external ppl_Polyhedron_OK:
 polyhedron -> bool = "ppl_Polyhedron_OK"

external ppl_Polyhedron_map_space_dimensions:
 polyhedron -> (int*int) list -> unit = "ppl_Polyhedron_map_space_dimensions"

external ppl_Polyhedron_swap:
polyhedron -> polyhedron -> unit = "ppl_Polyhedron_swap"

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
