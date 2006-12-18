(**Interfaces file to define new excpeptions, new types and interfaces function to PPL*)

open Gmp

exception Error of string

type linear_expression =
    Variable of int
  | Coefficient of Z.t
  | Unary_Plus of linear_expression
  | Unary_Minus of linear_expression
  | Plus of linear_expression * linear_expression
  | Minus of linear_expression * linear_expression
  | Times of Z.t * linear_expression

type linear_constraint =
    Less_Than of linear_expression * linear_expression
  | Less_Than_Or_Equal of linear_expression * linear_expression
  | Equal of linear_expression * linear_expression
  | Greater_Than of linear_expression * linear_expression
  | Greater_Than_Or_Equal of linear_expression * linear_expression

type linear_generator =
    Line of linear_expression
  | Ray of linear_expression
  | Point of linear_expression * Z.t
  | Closure_Point of linear_expression * Z.t

type relation_with_generator =
    Subsumes

type relation_with_constraint =
    Is_Disjoint
  | Strictly_Intersects
  | Is_Included
  | Saturates


type linear_congruence = linear_expression * linear_expression * Z.t

type constraint_system = linear_constraint list

type generator_system = linear_generator list

type congruence_system = linear_congruence list

(* Declared temporarily in this way to avoid name clashes. *)
type relation_symbol = Less_Than_RS | Less_Than_Or_Equal_RS | Equal_RS
                       | Greater_Than_RS | Greater_Than_Or_Equal_RS

type polyhedron

val ppl_new_C_Polyhedron_from_space_dimension:
  int -> polyhedron

val ppl_new_C_Polyhedron_from_constraints:
  constraint_system -> polyhedron

val ppl_new_C_Polyhedron_from_generators:
  generator_system -> polyhedron

val ppl_new_C_Polyhedron_from_congruences:
  congruence_system -> polyhedron

val ppl_Polyhedron_relation_with_constraint:
      polyhedron -> linear_constraint -> relation_with_constraint list

val ppl_Polyhedron_relation_with_generator:
      polyhedron -> linear_generator -> relation_with_generator list

val ppl_Polyhedron_space_dimension:
    polyhedron -> int

val ppl_Polyhedron_affine_dimension:
    polyhedron -> int

val ppl_Polyhedron_is_empty:
    polyhedron -> bool

val ppl_Polyhedron_is_universe:
    polyhedron -> bool

val ppl_Polyhedron_contains_interger_point:
    polyhedron -> bool

val ppl_Polyhedron_is_topologically_closed:
    polyhedron -> bool

val ppl_Polyhedron_is_bounded:
    polyhedron -> bool

val ppl_Polyhedron_bounds_from_above:
    polyhedron -> linear_expression -> bool

val ppl_Polyhedron_bounds_from_below:
    polyhedron -> linear_expression -> bool

val ppl_Polyhedron_add_constraint:
    polyhedron -> linear_constraint -> unit

val ppl_Polyhedron_add_constraint_and_minimize:
    polyhedron -> linear_constraint -> unit

val ppl_Polyhedron_add_constraints:
    polyhedron -> constraint_system -> unit

val ppl_Polyhedron_add_constraints_and_minimize:
    polyhedron -> constraint_system -> unit

val ppl_Polyhedron_add_generator:
    polyhedron -> linear_generator -> unit

val ppl_Polyhedron_add_generator_and_minimize:
    polyhedron -> linear_generator -> unit

val ppl_Polyhedron_add_generators:
    polyhedron -> generator_system -> unit

val ppl_Polyhedron_add_generators_and_minimize:
    polyhedron -> generator_system -> unit

val ppl_Polyhedron_add_congruences:
    polyhedron -> congruence_system -> unit

val ppl_Polyhedron_is_disjoint_from:
    polyhedron -> polyhedron -> bool

val ppl_Polyhedron_contains:
    polyhedron -> polyhedron -> bool

val ppl_Polyhedron_intersection_assign_and_minimize:
    polyhedron -> polyhedron -> bool

val ppl_Polyhedron_intersection_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_poly_hull_assign_and_minimize:
    polyhedron -> polyhedron -> bool

val ppl_Polyhedron_poly_hull_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_upper_bound_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_poly_difference_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_difference_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_time_elapse_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_concatenate_assign:
    polyhedron -> polyhedron -> unit

val ppl_Polyhedron_add_space_dimensions_and_embed:
    polyhedron -> int -> unit

val ppl_Polyhedron_add_space_dimensions_and_project:
    polyhedron -> int -> unit

val ppl_Polyhedron_remove_higher_space_dimensions:
    polyhedron -> int -> unit

val ppl_Polyhedron_remove_higher_space_dimensions:
    polyhedron -> int -> unit

val ppl_Polyhedron_space_dimension:
    polyhedron -> int

val ppl_Polyhedron_get_constraints:
    polyhedron -> constraint_system

val ppl_Polyhedron_get_minimized_constraints:
    polyhedron -> constraint_system

val ppl_Polyhedron_get_generators:
    polyhedron -> generator_system

val ppl_Polyhedron_get_minimized_generators:
    polyhedron -> generator_system

val ppl_Polyhedron_get_congruences:
    polyhedron -> congruence_system

val ppl_Polyhedron_get_minimized_congruences:
    polyhedron -> congruence_system

val ppl_Polyhedron_affine_image:
    polyhedron -> int -> linear_expression -> Z.t -> unit

val ppl_Polyhedron_affine_preimage:
    polyhedron -> int -> linear_expression -> Z.t -> unit

val ppl_Polyhedron_generalized_affine_image:
    polyhedron -> linear_expression -> relation_symbol -> linear_expression
    -> unit

val ppl_Polyhedron_generalized_affine_image:
    polyhedron -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit

val ppl_Polyhedron_generalized_affine_preimage1:
    polyhedron -> int -> relation_symbol -> linear_expression
      -> Z.t -> unit

val ppl_Polyhedron_generalized_affine_preimage2:
    polyhedron -> linear_expression -> relation_symbol -> linear_expression
    -> unit

val ppl_Polyhedron_bounded_affine_image:
    polyhedron -> int -> linear_expression -> linear_expression
      -> Z.t -> unit

val ppl_Polyhedron_bounded_affine_preimage:
    polyhedron -> int -> linear_expression -> linear_expression
      -> Z.t -> unit

val ppl_Polyhedron_BHRZ03_widening_assign:
    polyhedron -> polyhedron -> int -> int

val ppl_Polyhedron_limited_BHRZ03_extrapolation_assign:
    polyhedron -> polyhedron -> constraint_system -> int -> int

val ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign:
    polyhedron -> polyhedron -> constraint_system -> int -> int

val ppl_Polyhedron_limited_H79_extrapolation_assign:
    polyhedron -> polyhedron -> constraint_system -> int -> int

val ppl_Polyhedron_bounded_H79_extrapolation_assign:
    polyhedron -> polyhedron -> constraint_system -> int -> int

val ppl_Polyhedron_H79_widening_assign:
    polyhedron -> polyhedron -> int -> int

val ppl_Polyhedron_maximize:
    polyhedron -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator

val ppl_Polyhedron_minimize:
    polyhedron -> linear_expression -> bool * Z.t * Z.t * bool * linear_generator

val ppl_Polyhedron_remove_space_dimensions:
     polyhedron -> int list -> unit

val ppl_Polyhedron_fold_space_dimensions:
     polyhedron -> int list -> int -> unit

val ppl_Polyhedron_OK:
    polyhedron -> bool

val ppl_Polyhedron_map_space_dimensions:
    polyhedron -> (int*int) list -> unit

val ppl_Polyhedron_swap:
    polyhedron -> polyhedron -> unit

val test_linear_expression:
    linear_expression -> unit

val test_linear_constraint:
    linear_constraint -> unit

val test_linear_generator:
    linear_generator -> unit

val test_constraint_system:
    constraint_system -> unit

val test_generator_system:
    generator_system -> unit
