:- ensure_loaded(ppl_sicstus).

null :- new_polyhedron(X), delete_polyhedron(X).

go :- new_polyhedron(X), numbervars(A, 0, _), insert_constraint(X, 4*A >= 5).

:- null, write(null), nl, go, write(go), nl, halt.
