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
    ppl_copy_polyhedron(P,Q),
    solve(A,P,M,M1),
    solve(B,P,M1,N),
    (ppl_check_empty(P)
     ->
     ppl_renew_polyhedron(P,M),
     ppl_get_constraints(Q,Cs),
     ppl_insert_constraints(P,Cs),
     check, fail
     ;
     true
    ).

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
    A =.. [Pred|Args],
    A1 =.. [Pred|Args1],
    rename(Args,Args1),
    numbervars((Args,Args1),M,M1),
    M2 is M1-M,
    try_clause(Args,Args1,B1,M1,M2,N,P).


try_clause(Args,Args1,B1,M1,M2,N,P):-
    ppl_copy_polyhedron(P,Q),
    ppl_add_dimensions_and_embed(Q, M2),
    solve_equal_list(Args,Args1,Q),
    solve(B1,Q,M1,N),
    (ppl_check_empty(Q)
     ->
     fail
     ;
     M3 is N- (M1-M2),
     ppl_add_dimensions_and_embed(P, M3),
     ppl_get_constraints(Q,CQs),
     ppl_insert_constraints(P,CQs)
     ).

solve_constraints((C,D),P):- 
    !,
    solve_constraints(C,P),
    solve_constraints(D,P).
solve_constraints(C,P):- 
    ppl_insert_constraint(P,C).

check.

check_constraints(X) :-
	ppl_space_dimension(X, D),
        (ppl_check_empty(X) ->
	    fail
	;
	write(D), write(' * '),
	ppl_get_constraints(X, CS),
	write(CS), write(' % ')
%,
%	ppl_get_generators(X, GS),
%	write(GS), write(' ')
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

:- dynamic p1/2, p2/3, p3/2, p4/2.
p1(A,B):- {A>=B}, p2(A,B,C).
p2(X,Y,Z):- {X+Y=<4}.
p3(X,Y):- {X+Y=4}.

:- dynamic fib/2.
fib(X, Y) :-
  { X >= 0, X =< 1, Y = 1 }.
fib(X, Y) :-
  { X >= 2, Xm1 = X-1, Xm2 = X-2, Y = Y1+Y2 },
  fib(Xm1, Y1),
  fib(Xm2, Y2).     
                                                           
    p4(A,B):- {A +1 =< B}.
    p4(A,B):- {A >= B + 1}.
    p4(A,B):- {A = B}.

%% to check the backtracking
run(A,B):- solve((p4(A,B),{A=1,B=1})).
run1(A,B):- solve((p4(A,B),{A=1})).

ppl_renew_polyhedron(P,M):-
    ppl_space_dimension(P,D),
    MaxCode is D-1,
    MinCode is M,
    make_var_list(MaxCode,MinCode,VarList),
    ppl_new_polyhedron(Q,D),
    ppl_convex_hull_assign(P,Q),
    ppl_remove_dimensions(P, VarList).

make_var_list(MaxCode,MaxCode,[]).
make_var_list(MaxCode,VarCode,['$VAR'(VarCode)|VarList]):-
    VarCode < MaxCode,
    VarCode1 is VarCode+1,
    make_var_list(MaxCode,VarCode1,VarList).

go :-
	ppl_new_polyhedron(X, 3),
	numbervars([A,B,C], 0, _),
%	ppl_insert_constraint(X, 4*A+B-2*C >= 5),
%	ppl_insert_constraint(X, 4*A+B>= 0),
        check_constraints(X),
%        ppl_remove_dimensions(X,[C]),
        ppl_renew_polyhedron(X,2),
        check_constraints(X).
        
