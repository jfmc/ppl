:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).

solve(Goal):-
    ppl_new_polyhedron(P, 0),
    solve(Goal,P,0,_),
    check_constraints(P).

solve(true,_P,M,M):-
    !.

solve(A=B,P,M,N):-
    solve({A=B},P,M,N).

solve((A,B),P,M,N):- 
    !, 
    solve(A,P,M,M1),
    solve(B,P,M1,N).

solve({Cs},P,M,N):- 
    !,
    numbervars(Cs,M,N),
    M1 is N-M,
    ppl_add_dimensions_and_embed(P, M1),
    solve_constraints(Cs,P).

solve(A,P,M,N):-
    functor(A,Pred,Arity),
    functor(A1,Pred,Arity),
    clause(A1,B1),
    rename(Args,Args1),
    numbervars((Args,Args1),M,M1),
    M2 is M1-M,
    ppl_add_dimensions_and_embed(P, M2),
    solve_equal_list(Args,Args1,P),
    solve(B1,P,M1,N).

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
    
rename([],[]).
rename([A|As],[B|Bs]):-
    var(A),
    !,
    rename(As,Bs).
rename([A|As],[A|Bs]):-
    rename(As,Bs).

solve_equal_list([],[],P).
solve_equal_list([A|As],[B|Bs],P):-
    solve_constraints(A=B,P),
    solve_equal_list(As,Bs,P).

% ?- solve({X+Y>=3,Y>=0,X=<2}).

:- dynamic p1/2, p2/3, p3/2.
p1(A,B):- {A>=B}, p2(A,B,C).
p2(X,Y,Z):- {X+Y=<4}.
p3(X,Y):- {X+Y=4}.

%%% WARNING (eg) solve(fib(4,A)) does not work yet%%%%
:- dynamic fib/2.
fib(X, Y) :-
  { X >= 0, X =< 1, Y = 1 }.
fib(X, Y) :-
  { X >= 2, Xm1 = X-1, Xm2 = X-2, Y = Y1+Y2 },
  fib(Xm1, Y1),
  fib(Xm2, Y2).                                                                 
