:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).

solve(Goal):-
    ppl_new_polyhedron(Polyhedron, 0),
    solve(Goal,Polyhedron,0,_),
    check_constraints(Polyhedron),
    ppl_delete_polyhedron(Polyhedron).

solve(true,_Polyhedron,Dims,Dims):-
    !.

solve(A=B,Polyhedron,InDims,OutDims):-
    solve({A=B},Polyhedron,InDims,OutDims).

solve((A,B),Polyhedron,InDims,OutDims):- 
    !, 
    ppl_copy_polyhedron(Polyhedron,Q),
    (conjunct_solve(A,B,Polyhedron,InDims,OutDims,Q)
     ->
     ppl_delete_polyhedron(Q)
     ;
     ppl_delete_polyhedron(Q),
     fail
    ).

solve({Cs},Polyhedron,InDims,OutDims):- 
    !,
    numbervars(Cs,InDims,OutDims),
    AddedDims is OutDims-InDims,
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
    solve_constraints(Cs,Polyhedron).

solve(Atom,Polyhedron,InDims,OutDims):-
    functor(Atom,Pred,Arity),
    functor(Atom1,Pred,Arity),
    clause(Atom1,Body1),
    Atom =.. [Pred|Args],
    Atom1 =.. [Pred|Args1],
    rename(Args,Args1),
    numbervars(Args,InDims,OutDims),
    numbervars(Args1,OutDims,_),
%    AddedDims is OutDims - InDims,
%    try_clause(Args,Args1,Body1,InDims,AddedDims,Polyhedron).
    try_clause(Args,Args1,Body1,InDims,OutDims,Polyhedron).


try_clause(Args,Args1,Body1,InDims,OutDims,Polyhedron):-
    ppl_copy_polyhedron(Polyhedron,Q),
    AddedDims is 2 * (OutDims - InDims),
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
    solve_equal_list(Args,Args1,Polyhedron),
    NewInDims is InDims + AddedDims,
  (
    (solve(Body1,Polyhedron,NewInDims,_),
    (ppl_check_empty(Polyhedron)
     ->
     ppl_renew_polyhedron(Polyhedron,InDims),
     ppl_get_constraints(Q,QConstraints),
     ppl_insert_constraints(Polyhedron,QConstraints),
     fail
     ;
     ppl_project_dimensions(Polyhedron,OutDims)
    ))
  ->
    ppl_delete_polyhedron(Q)
  ;
    ppl_delete_polyhedron(Q),
    fail
  ).

conjunct_solve(A,B,Polyhedron,InDims,OutDims,Q):-
    solve(A,Polyhedron,InDims,AOutDims),
    solve(B,Polyhedron,AOutDims,OutDims),
    (ppl_check_empty(Polyhedron)
     ->
     ppl_renew_polyhedron(Polyhedron,InDims),
     ppl_get_constraints(Q,QConstraints),
     ppl_insert_constraints(Polyhedron,QConstraints),
     fail
     ;
     true
    ).

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

ppl_renew_polyhedron(Polyhedron,NewDims):-
    ppl_space_dimension(Polyhedron,CurrentDims),
    make_var_list(CurrentDims,NewDims,VarList),
    ppl_remove_dimensions(Polyhedron, VarList),
    ppl_new_polyhedron(Q,NewDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    ppl_delete_polyhedron(Q).  

ppl_project_dimensions(Polyhedron,NewDims):-
    ppl_space_dimension(Polyhedron,CurrentDims),
    MaxCode is CurrentDims,
    MinCode is NewDims,
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
runp4a(A,B):- solve(({A=1}, p4(A,B),{B=1})).

% To run all the tests at once.
runall:- 
   runp1(_A,_B),nl, 
   runp3(_C,_D),nl, 
   runp4(_E,_F),nl, 
   runp4a(_E1,_F1),nl, 
   runfib(4,_X),nl.

/*
Test results
| ?- runall.
2 * [1*A+ -1*B>=0,-1*A+ -1*B>= -4] %
2 * [1*A+1*B=4] %
2 * [1*A=1,1*B=1] %
2 * [1*A=1,1*B=1] %
1 * [1*A=5] %

*/
