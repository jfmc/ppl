:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).
?- use_module(library(lists)).

solve(Goal):-
    call_residue(Goal,CCs),
    numbervars((Goal,CCs),0,_),
    length(CCs,L),
    ppl_new_polyhedron(P, L),
    solve(Goal,P),
    check_constraints(P).

solve(true,_P):-
    !.
solve((A,B),P):- 
    !, 
    solve(A,P),
    solve(B,P).
solve({Cs},P):- 
    !,
    solve_constraints(Cs,P).
solve(A,P):-
    clause(A,B), 
    solve(B,P).

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
