:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).

solve(Goal):-
    numbervars(Goal,0,M),
    ppl_new_polyhedron(P, M),
    solve(Goal,P,M,_),
    check_constraints(P).

solve(true,_P,M,M):-
    !.
solve((A,B),P,M,N):- 
    !, 
    solve(A,P,M,M1),
    solve(B,P,M1,N).
solve({Cs},P,M,N):- 
    !,
    numbervars(Cs,M,N),
    solve_constraints(Cs,P).
solve(A,P,M,N):-
    clause(A,B), 
    numbervars(B,M,N1),
    M1 is N1-M,
    ppl_add_dimensions_and_embed(P, M1),
    solve(B,P,N1,N).

solve_constraints((C,D),P):- 
    !,
    solve_constraints(C,P),
    solve_constraints(D,P).
solve_constraints(C,P):- 
    ppl_insert_constraint(P,C).


check_constraints(X) :-
	ppl_space_dimension(X, D),
        (ppl_check_empty(X) ->
	    write(no_solution)
	;
	write(D), write(' * '),
	ppl_get_constraints(X, CS),
	write(CS), write(' % '),
	ppl_get_generators(X, GS),
	write(GS), write(' ')
	).

% ?- solve({X+Y>=3,Y>=0,X=<2}).

:- dynamic p1/2, p2/3.
p1(A,B):- {A>=B}, p2(A,B,C).
p2(X,Y,Z):- {X+Y=<4}.
