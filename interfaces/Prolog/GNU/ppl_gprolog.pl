:- foreign(ppl_init).
:- foreign(ppl_new_polyhedron(+term, +term)).
:- foreign(ppl_new_empty_polyhedron(+term, +term)).
:- foreign(ppl_copy_polyhedron(+term, +term)).
:- foreign(ppl_delete_polyhedron(+term)).
:- foreign(ppl_space_dimension(+term, +term)).
:- foreign(ppl_insert_constraint(+term, +term)).
:- foreign(ppl_insert_generator(+term, +term)).
:- foreign(ppl_add_constraints_and_minimize(+term, +term)).
:- foreign(ppl_check_empty(+term)).
:- foreign(ppl_intersection_assign(+term, +term)).
:- foreign(ppl_intersection_assign_and_minimize(+term, +term)).
:- foreign(ppl_convex_hull_assign(+term, +term)).
:- foreign(ppl_convex_hull_assign_and_minimize(+term, +term)).
:- foreign(ppl_convex_difference_assign(+term, +term)).
:- foreign(ppl_convex_difference_assign_and_minimize(+term, +term)).
:- foreign(ppl_widening_assign(+term, +term)).
:- foreign(ppl_get_constraints(+term, +term)).
:- foreign(ppl_get_generators(+term, +term)).
:- foreign(ppl_remove_dimensions(+term, +term)).
:- foreign(ppl_remove_higher_dimensions(+term, +term)).
:- foreign(ppl_add_dimensions_and_project(+term, +term)).
:- foreign(ppl_add_dimensions_and_embed(+term, +term)).
:- foreign(ppl_polyhedron_included_or_equal(+term, +term)).
:- foreign(ppl_polyhedron_equal(+term, +term)).
:- foreign(ppl_polyhedron_strictly_included(+term, +term)).
:- foreign(ppl_relation_with_constraint(+term, +term, +term)).
:- foreign(ppl_relation_with_generator(+term, +term, +term)).
:- foreign(ppl_affine_image(+term, +term, +term, +term)).
:- foreign(ppl_affine_preimage(+term, +term, +term, +term)).
