:- module(ppl_ciao,
[
	ppl_initialize/0,
	ppl_finalize/0,
	ppl_new_C_Polyhedron_from_dimension/2,
	ppl_new_NNC_Polyhedron_from_dimension/2,
	ppl_new_C_Polyhedron_empty_from_dimension/2,
	ppl_new_NNC_Polyhedron_empty_from_dimension/2,
	ppl_new_C_Polyhedron_from_C_Polyhedron/2,
	ppl_new_C_Polyhedron_from_NNC_Polyhedron/2,
	ppl_new_NNC_Polyhedron_from_C_Polyhedron/2,
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron/2,
	ppl_new_C_Polyhedron_from_ConSys/2,
	ppl_new_NNC_Polyhedron_from_ConSys/2,
	ppl_new_C_Polyhedron_from_GenSys/2,
	ppl_new_NNC_Polyhedron_from_GenSys/2,
	ppl_delete_Polyhedron/1,
	ppl_Polyhedron_space_dimension/2,
	ppl_Polyhedron_intersection_assign/2,
	ppl_Polyhedron_intersection_assign_and_minimize/2,
	ppl_Polyhedron_poly_hull_assign/2,
	ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	ppl_Polyhedron_poly_difference_assign/2,
	ppl_Polyhedron_poly_difference_assign_and_minimize/2,
	ppl_Polyhedron_widening_assign/2,
	ppl_Polyhedron_limited_widening_assign/3,
	ppl_Polyhedron_get_constraints/2,
	ppl_Polyhedron_get_minimized_constraints/2,
	ppl_Polyhedron_get_generators/2,
	ppl_Polyhedron_get_minimized_generators/2,
	ppl_Polyhedron_add_constraint/2,
	ppl_Polyhedron_add_generator/2,
	ppl_Polyhedron_add_constraints/2,
	ppl_Polyhedron_add_constraints_and_minimize/2,
	ppl_Polyhedron_add_generators/2,
	ppl_Polyhedron_add_generators_and_minimize/2,
	ppl_Polyhedron_add_dimensions_and_constraints/2,
	ppl_Polyhedron_add_dimensions_and_project/2,
	ppl_Polyhedron_add_dimensions_and_embed/2,
	ppl_Polyhedron_remove_dimensions/2,
	ppl_Polyhedron_remove_higher_dimensions/2,
	ppl_Polyhedron_affine_image/4,
	ppl_Polyhedron_affine_preimage/4,
	ppl_Polyhedron_relation_with_constraint/3,
	ppl_Polyhedron_relation_with_generator/3,
	ppl_Polyhedron_check_empty/1,
	ppl_Polyhedron_check_universe/1,
	ppl_Polyhedron_is_bounded/1,
	ppl_Polyhedron_contains_Polyhedron/2,
	ppl_Polyhedron_strictly_contains_Polyhedron/2,
	ppl_Polyhedron_equals_Polyhedron/2
],
[
	assertions,
	basic_modes,
	regtypes,
	foreign_interface
]).

:- use_foreign_source(ppl_ciao).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIXME: what is the exact syntax ?
:- use_foreign_library([ppl]).

:- extra_compiler_opts(['-O2']).

%% :- extra_linker_opts('').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- impl_defined(
[
%%FIXME: Initialization and finalization.
	ppl_init/1,
	ppl_deinit/1,

	ppl_new_polyhedron/2,
	ppl_new_empty_polyhedron/2,
	ppl_copy_polyhedron/2,
	ppl_delete_polyhedron/1,
	ppl_space_dimension/1,
	ppl_add_constraint/2,
	ppl_add_generator/2,
	ppl_add_constraints_and_minimize/3,
	ppl_remove_dimensions/2,
	ppl_remove_higher_dimensions/2,
	ppl_add_dimensions_and_embed/2,
	ppl_add_dimensions_and_project/2,
	ppl_check_empty/2,
	ppl_get_constraints/2,
	ppl_get_minimized_constraints/2,
	ppl_get_generators/2,
	ppl_get_minimized_generators/2,
	ppl_intersection_assign/2,
	ppl_poly_hull_assign/2,
	ppl_widening_assign/2
]).

:- true pred ppl_new_polyhedron(go(Polyhedron), in(NumDims))
             :: address * int
             +  (foreign, returns(Polyhedron)).

:- true pred ppl_new_empty_polyhedron(go(Polyhedron), in(NumDims))
             :: address * int
             +  (foreign, returns(Polyhedron)).

:- true pred ppl_copy_polyhedron(in(Polyhedron), out(PolyhedronCopy))
             :: address * address
             +  (foreign, returns(PolyhedronCopy)).

:- true pred ppl_delete_polyhedron(in(Polyhedron))
             :: address
             +  (foreign).

:- true pred ppl_space_dimension(in(Polyhedron), out(NumDims))
             :: address * int
             +  (foreign, returns(NumDims)).

:- true pred ppl_add_constraint(in(Polyhedron), in(Constraint))
%%FIXME : term ?
             :: address * term
             +  (foreign).

:- true pred ppl_add_constraints_and_minimize(
	in(Polyhedron),
	in(Constraints),
	go(Empty))
%%FIXME : list of terms ?
             :: address * term * int
             +  (foreign, returns(Empty)).

:- true pred ppl_add_generator(in(Polyhedron), in(Generator))
%%FIXME : term ?
             :: address * term
             +  (foreign).

%%FIXME : here we could use `int_list' as the type of the second
%%        argument, but this would require the addition of another
%%        argument for the length of the list.
%%:- true pred ppl_remove_dimensions(in(Polyhedron), in(Length), in(DimList))
%%             :: address * int * int_list
%%             +  (foreign, size_of(DimList, Length)).
%%
%% Using `term' (which still does not work) for the moment.
:- true pred ppl_remove_dimensions(in(Polyhedron), in(DimList))
             :: address * term
             +  (foreign).

:- true pred ppl_remove_higher_dimensions(in(Polyhedron), in(NumDims))
             :: address * int
             +  (foreign).

:- true pred ppl_add_dimensions_and_project(in(Polyhedron), in(NumDims))
             :: address * int
             +  (foreign).

:- true pred ppl_add_dimensions_and_embed(in(Polyhedron), in(NumDims))
             :: address * int
             +  (foreign).

:- true pred ppl_check_empty(in(Polyhedron), go(Empty))
             :: address * int
             +  (foreign, returns(Empty)).

:- true pred ppl_get_constraints(in(Polyhedron), go(Constraints))
%%FIXME: list of terms ?
             :: address * term
             +  (foreign, returns(Constraints)).

:- true pred ppl_get_minimized_constraints(in(Polyhedron), go(Constraints))
%%FIXME: list of terms ?
             :: address * term
             +  (foreign, returns(Constraints)).

:- true pred ppl_get_generators(in(Polyhedron), go(Generators))
%%FIXME: list of terms ?
             :: address * term
             +  (foreign, returns(Generators)).

:- true pred ppl_get_minimized_generators(in(Polyhedron), go(Generators))
%%FIXME: list of terms ?
             :: address * term
             +  (foreign, returns(Generators)).

:- true pred ppl_intersection_assign(in(Polyhedron1), in(Polyhedron2))
             :: address * address
             +  (foreign).

:- true pred ppl_poly_hull_assign(in(Polyhedron1), in(Polyhedron2))
             :: address * address
             +  (foreign).

:- true pred ppl_widening_assign(in(Polyhedron1), in(Polyhedron2))
             :: address * address
             +  (foreign).

ppl_check_empty(Polyhedron) :-
  ppl_check_empty(Polyhedron, 1).

ppl_add_constraints(_Polyhedron, []).
ppl_add_constraints(Polyhedron, [C|Constraints]) :-
  ppl_add_constraint(Polyhedron, C),
  ppl_add_constraints(Polyhedron, Constraints).

ppl_add_generators(_Polyhedron, []).
ppl_add_generators(Polyhedron, [G|Generators]) :-
  ppl_add_generator(Polyhedron, G),
  ppl_add_generators(Polyhedron, Generators).

ppl_add_constraints_and_minimize(Polyhedron, Constraints) :-
  ppl_add_constraints_and_minimize(Polyhedron, Constraints, 1).
