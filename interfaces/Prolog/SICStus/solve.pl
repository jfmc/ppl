:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).

solve(Goal):-
    ppl_new_polyhedron(Polyhedron, 0),
    solve(Goal,Polyhedron,0,_),
    check_constraints(Polyhedron).

solve(true,_Polyhedron,M,M):-
    !.

solve(A=B,Polyhedron,M,N):-
    solve({A=B},Polyhedron,M,N).

solve((A,B),Polyhedron,M,N):- 
    !, 
    ppl_copy_polyhedron(Polyhedron,Q),
    solve(A,Polyhedron,M,M1),
    solve(B,Polyhedron,M1,N),
    (ppl_check_empty(Polyhedron)
     ->
     ppl_renew_polyhedron(Polyhedron,M),
     ppl_copy_polyhedron(Q,Polyhedron),
     fail
     ;
     true
    ),
    ppl_delete_polyhedron(Q).

solve({Cs},Polyhedron,M,N):- 
    !,
    numbervars(Cs,M,N),
    M1 is N-M,
    ppl_add_dimensions_and_embed(Polyhedron, M1),
    solve_constraints(Cs,Polyhedron).

solve(Atom,Polyhedron,M,N):-
    functor(Atom,Pred,Arity),
    functor(Atom1,Pred,Arity),
    clause(Atom1,Body1),
    Atom =.. [Pred|Args],
    Atom1 =.. [Pred|Args1],
    rename(Args,Args1),
    numbervars(Args,M,N),
    numbervars(Args1,N,_),
    NewDims is N - M,
    try_clause(Args,Args1,Body1,M,NewDims,Polyhedron).


try_clause(Args,Args1,Body1,M,NewDims,Polyhedron):-
    ppl_copy_polyhedron(Polyhedron,Q),
    M2 is 2* NewDims,
    ppl_add_dimensions_and_embed(Q, M2),
    solve_equal_list(Args,Args1,Q),
    M3 is M + M2,
    solve(Body1,Q,M3,_),
    (ppl_check_empty(Q)
     ->
     (ppl_renew_polyhedron(Q,M3),
     fail)
     ;
     M1 is M + NewDims,
     ppl_project_dimensions(Q,M1),
     ppl_get_constraints(Q,CQs),
     ppl_add_dimensions_and_embed(Polyhedron, NewDims),
     ppl_insert_constraints(Polyhedron,CQs)
    ),
    ppl_delete_polyhedron(Q).

solve_constraints((C,D),Polyhedron):- 
    !,
    solve_constraints(C,Polyhedron),
    solve_constraints(D,Polyhedron).
solve_constraints(C,Polyhedron):- 
    ppl_insert_constraint(Polyhedron,C).
    
rename([],[]).
rename([A|As],[_B|Bs]):-
    var(A),
    !,
    rename(As,Bs).
rename([A|As],[A|Bs]):-
    rename(As,Bs).

solve_equal_list([],[],_Polyhedron).
solve_equal_list([A|As],[B|Bs],Polyhedron):-
    solve_constraints(A=B,Polyhedron),
    solve_equal_list(As,Bs,Polyhedron).

ppl_renew_polyhedron(Polyhedron,M):-
    ppl_space_dimension(Polyhedron,D),
    MaxCode is D,
    MinCode is M,
    make_var_list(MaxCode,MinCode,VarList),
    ppl_remove_dimensions(Polyhedron, VarList),
    ppl_new_polyhedron(Q,M),
    ppl_convex_hull_assign(Polyhedron,Q),
    ppl_delete_polyhedron(Q).

/*
ppl_renew_polyhedron(Polyhedron,M):-
    ppl_delete_polyhedron(Polyhedron),
    ppl_new_polyhedron(Polyhedron,M).
*/
    

ppl_project_dimensions(Polyhedron,M):-
    ppl_space_dimension(Polyhedron,D),
    MaxCode is D,
    MinCode is M,
    make_var_list(MaxCode,MinCode,VarList),
    ppl_remove_dimensions(Polyhedron, VarList).

make_var_list(MaxCode,MaxCode,[]).
make_var_list(MaxCode,VarCode,['$VAR'(VarCode)|VarList]):-
    VarCode < MaxCode,
    VarCode1 is VarCode+1,
    make_var_list(MaxCode,VarCode1,VarList).

check_constraints(Polyhedron) :-
    ppl_space_dimension(Polyhedron, D),
    write(D), write(' * '),
    ppl_get_constraints(Polyhedron, CS),
    write(CS), write(' % ')
%,
%	ppl_get_generators(Polyhedron, GS),
%	write(GS), write(' ')
.

%%%%%%%%%%% code for debugging %%%%%%%%%%%%%%%%%
check.


%%%%%%%%%%% various tests %%%%%%%%%%%%%%%%%%%%%%%
% ?- solve({X+Y>=3,Y>=0,X=<2}).

% Some basic checks
:- dynamic p1/2, p2/3, p3/2, p4/2.
p1(A,B):- {A>=B}, p2(A,B,_C).
p2(X,Y,_Z):- {X+Y=<4}.
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
