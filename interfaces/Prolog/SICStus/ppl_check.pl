:- ensure_loaded(ppl_sicstus).

null :- ppl_new_polyhedron(X, 0),
	ppl_delete_polyhedron(X).

check_empty(X) :-
	ppl_space_dimension(X, D),
	write(D), write(' '),
        (ppl_check_empty(X) ->
	    write(empty)
	;
	    write(nonempty)
	),
	write(' ').

go :-
	ppl_new_polyhedron(X, 2),
	numbervars((A,B), 0, _),
	check_empty(X),
	ppl_insert_constraint(X, 4*A >= 5),
	check_empty(X),
	ppl_insert_generator(X, ray(A + 9*B)),
	check_empty(X),
	ppl_insert_constraint(X, A >= 5),
	check_empty(X),
	ppl_insert_constraint(X, A =< 2),
	check_empty(X),
	nl,
	ppl_delete_polyhedron(X).

:- null, write(null), nl, go, write(go), nl, halt.
