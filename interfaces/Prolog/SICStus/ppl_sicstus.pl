foreign_resource(ppl_sicstus,
	[init(ppl_init),
	deinit(ppl_deinit),
	ppl_new_polyhedron,
	ppl_delete_polyhedron,
	ppl_insert_constraint]).

foreign(ppl_new_polyhedron, c, new_polyhedron([-address])).
foreign(ppl_delete_polyhedron, c, delete_polyhedron(+address)).
foreign(ppl_insert_constraint, c, insert_constraint(+address, +term)).

:- load_foreign_resource(ppl_sicstus).
