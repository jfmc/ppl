:- ensure_loaded(ppl_sicstus).

null :- ppl_new_polyhedron(X),
	ppl_delete_polyhedron(X).

go :-
	ppl_new_polyhedron(X),
	numbervars((A,B), 0, _),
	ppl_insert_constraint(X, 4*A >= 5),
	ppl_insert_generator(X, ray(A+9*B)),
	ppl_delete_polyhedron(X).

:- null, write(null), nl, go, write(go), nl, halt.
