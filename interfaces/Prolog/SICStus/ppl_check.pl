:- ensure_loaded(ppl_sicstus).

null :- ppl_new_polyhedron(X, 0),
	ppl_delete_polyhedron(X).

check_empty(X) :-
	ppl_space_dimension(X, D),
	write(D), write(' * '),
	ppl_get_constraints(X, CS),
	write(CS), write(' % '),
	ppl_get_generators(X, GS),
	write(GS), write(' '),
        (ppl_check_empty(X) ->
	    write(empty)
	;
	    write(nonempty)
	),
	write(' ').

go :-
	ppl_new_polyhedron(X, 3),
	numbervars([A,B,C], 0, _),
	check_empty(X),
	nl,
	ppl_insert_constraint(X, 4*A+B-2*C >= 5),
	check_empty(X),
	nl,
	ppl_insert_generator(X, vertex(-1000*A - 9*B+7,16)),
	check_empty(X),
	nl,
	ppl_insert_constraint(X, A >= 5),
	check_empty(X),
	nl, write(remove_dimensions), nl,
	ppl_remove_dimensions(X, []),
	check_empty(X),
	nl,
	ppl_remove_dimensions(X, [A,B]),
	check_empty(X),
	nl,
	ppl_delete_polyhedron(X).


go1 :-
	ppl_new_empty_polyhedron(X, 3),
	numbervars([A,B,C], 0, _),
	ppl_insert_generator(X, vertex(-1000*A - 9*B, 16)),
	check_empty(X),
	nl,
	ppl_delete_polyhedron(X).

:- go1, nl, null, write(null), nl, go, write(go), nl, halt.
