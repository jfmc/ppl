foreign_resource(ppl_sicstus,
	[init(ppl_init),
	deinit(ppl_deinit),
	ppl_new_polyhedron,
	ppl_delete_polyhedron,
	ppl_insert_constraint,
	ppl_insert_generator]).

foreign(ppl_new_polyhedron, c, ppl_new_polyhedron([-address])).
foreign(ppl_delete_polyhedron, c, ppl_delete_polyhedron(+address)).
foreign(ppl_insert_constraint, c, ppl_insert_constraint(+address, +term)).
foreign(ppl_insert_generator, c, ppl_insert_generator(+address, +term)).

:- load_foreign_resource(ppl_sicstus).
