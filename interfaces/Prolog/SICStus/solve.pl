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
     ppl_copy_polyhedron(Q,P),
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
    numbervars(Args,M,N),
    numbervars(Args1,N,_),
    NewDims is N - M,
    try_clause(Args,Args1,B1,M,NewDims,P).


try_clause(Args,Args1,B1,M,NewDims,P):-
    ppl_copy_polyhedron(P,Q),
    M2 is 2* NewDims,
    ppl_add_dimensions_and_embed(Q, M2),
    solve_equal_list(Args,Args1,Q),
    M3 is M + M2,
    solve(B1,Q,M3,_),
    (ppl_check_empty(Q)
     ->
     (ppl_renew_polyhedron(Q,M3),
     fail)
     ;
     M1 is M + NewDims,
     ppl_project_dimensions(Q,M1),
     ppl_get_constraints(Q,CQs),
     ppl_add_dimensions_and_embed(P, NewDims),
     ppl_insert_constraints(P,CQs)
    ).

solve_constraints((C,D),P):- 
    !,
    solve_constraints(C,P),
    solve_constraints(D,P).
solve_constraints(C,P):- 
    ppl_insert_constraint(P,C).
    
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

ppl_renew_polyhedron(P,M):-
    ppl_space_dimension(P,D),
    MaxCode is D,
    MinCode is M,
    make_var_list(MaxCode,MinCode,VarList),
    ppl_remove_dimensions(P, VarList),
    ppl_new_polyhedron(Q,M),
    ppl_convex_hull_assign(P,Q).

/*
ppl_renew_polyhedron(P,M):-
    ppl_delete_polyhedron(P),
    ppl_new_polyhedron(P,M).
*/
    

ppl_project_dimensions(P,M):-
    ppl_space_dimension(P,D),
    MaxCode is D,
    MinCode is M,
    make_var_list(MaxCode,MinCode,VarList),
    ppl_remove_dimensions(P, VarList).

make_var_list(MaxCode,MaxCode,[]).
make_var_list(MaxCode,VarCode,['$VAR'(VarCode)|VarList]):-
    VarCode < MaxCode,
    VarCode1 is VarCode+1,
    make_var_list(MaxCode,VarCode1,VarList).

check_constraints(X) :-
    ppl_space_dimension(X, D),
    write(D), write(' * '),
    ppl_get_constraints(X, CS),
    write(CS), write(' % ')
%,
%	ppl_get_generators(X, GS),
%	write(GS), write(' ')
.

%%%%%%%%%%% code for debugging %%%%%%%%%%%%%%%%%
check.


%%%%%%%%%%% various tests %%%%%%%%%%%%%%%%%%%%%%%
% ?- solve({X+Y>=3,Y>=0,X=<2}).

% Some basic checks
:- dynamic p1/2, p2/3, p3/2, p4/2.
p1(A,B):- {A>=B}, p2(A,B,C).
p2(X,Y,Z):- {X+Y=<4}.
p3(X,Y):- {X+Y=4}.

runp1(A,B):- solve(p1(A,B)).
runp3(A,B):- solve(p3(A,B)).

% fibonacci test (tests shallow backtracking)
:- dynamic fib/2.
fib(X, Y) :-
  { X >= 0, X =< 1, Y = 1 }.
fib(X, Y) :-
  { X >= 2, Xm1 = X-1, Xm2 = X-2, Y = Y1+Y2 },
  fib(Xm1, Y1),
  fib(Xm2, Y2). 

runfib(A,B):- solve(fib(A,B)).    
                                                           
%% test to further check the backtracking
p4(A,B):- {A +1 =< B}.
p4(A,B):- {A >= B + 1}.
p4(A,B):- {A = B}.

runp4(A,B):- solve((p4(A,B),{A=1,B=1})).
