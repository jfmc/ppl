/*
A non-ground meta-interpreter for CLP(Q) for use with the 
Parma Polyhdra Library.
It is based on the well-known "solve" or "vanilla" interpreter.
The object program must be be loaded and all object predicates declared as
dynamic.
*/

:- ensure_loaded(ppl_sicstus).
?- use_module(library(clpq)).

/*
solve/1 is the top-level predicate.
A top level query has the form:
?- solve(+query).
For example, 
?- solve(p1(A,B)).
*/
solve(Goal):-
% Polyhedron: the main polyhedron is initialised with 0 dimensions.
    ppl_new_polyhedron(Polyhedron, 0), 
    solve(Goal,Polyhedron,0,_),
    check_constraints(Polyhedron),
    ppl_delete_polyhedron(Polyhedron).

/*
solve/4 is the main meta-interpreter.
solve(+Goal, +Polyhedron, +Integer1, -Integer2)
Goal: The query to be solved.
Polyhedron: A reference to a polyhedron which 
            represents the current set of constraints.
Integer1: The initial number of dimensions of the Polyhedron.
Integer2: The final number of dimensions of the Polyhedron.
*/

%%% The base case.
solve(true,_Polyhedron,Dims,Dims):-
    !.

%%% The case when the query is equality.
         % The special constraint solver is called.
         % A and B must both be linear rational expressions.
solve(A=B,Polyhedron,InDims,OutDims):-
    solve_constraints(A=B,Polyhedron,InDims,OutDims).

%%% The case when the query is a conjunction.
solve((A,B),Polyhedron,InDims,OutDims):- 
    !, 
    ppl_copy_polyhedron(Polyhedron,Q), % A copy of Polyhedron is kept.
  (
    (solve(A,Polyhedron,InDims,AOutDims),
    solve(B,Polyhedron,AOutDims,OutDims),
    (ppl_check_empty(Polyhedron)
     ->   % If B fails, then the Polyhedron will be empty.
          % The orginal Polyhedron is restored for backtracking.
     ppl_project_dimensions(Polyhedron,InDims),
     ppl_convex_hull_assign(Polyhedron,Q),
     fail
     ;    % If B succeeds, then the Polyhedron will be non-empty.
     true
    ))
  ->
    ppl_delete_polyhedron(Q)  % Q is now unwanted and removed.
    ;
    ppl_delete_polyhedron(Q), % Q is now unwanted and removed.
    fail
  ).

%%% The case when the query is a set of constraints.
solve({Cs},Polyhedron,InDims,OutDims):- 
    !,
         % The variables are now frozen for using with the PPL.
    numbervars(Cs,InDims,OutDims),
         % The number of extra dimensions is the number of new variables.
    AddedDims is OutDims-InDims,
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % Solve the constraints using the constraint solver.
    solve_constraints(Cs,Polyhedron). 

%%% The remaining case when it is a user-defined atomic goal.
solve(Atom,Polyhedron,InDims,OutDims):-
    functor(Atom,Pred,Arity),
    functor(Atom1,Pred,Arity),
    clause(Atom1,Body1),
    Atom =.. [Pred|Args],
    Atom1 =.. [Pred|Args1],
    rename(Args,Args1),
    numbervars(Args,InDims,OutDims),
    numbervars(Args1,OutDims,_),
    try_clause(Args,Args1,Body1,InDims,OutDims,Polyhedron).

/*
In try_clause/6, the selected clause is solved after 
making a copy of the polyhedron. 
If it fails, then the polyhedron is restored and the copy destroyed.
Args: The arguments of the call.
Args1: A copy of Args where the unbound variables are new.
Body1: The body of the call where the arguments Args are replaced by Args1.
InDims: The initial number of variables before the call.
OutDims: The number of variables after binding with the head (Args).
Polyhedron: Represents the current set of constraints.
*/

try_clause(Args,Args1,Body1,InDims,OutDims,Polyhedron):-
    ppl_copy_polyhedron(Polyhedron,Q), % A copy of Polyhedron is kept.
    AddedDims is 2 * (OutDims - InDims), 
         % OutDims - InDims is the number of new variables in Args.
         % There are the same number of new variables in Args1 as in Args.
         % Hence we need twice this number.
         % The Polyhedron has the correct number of dimensions added.
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % The new constraints that make Args = Args1 are solved.
    solve_equal_list(Args,Args1,Polyhedron),
    NewInDims is InDims + AddedDims,
         % Now we can solve the body.
  (
    (solve(Body1,Polyhedron,NewInDims,_),
    (ppl_check_empty(Polyhedron)
     ->
         % If the body fails, the polyhedron is empty.
         % The orginal Polyhedron is restored for backtracking.
         % First eliminate any new dimensions.
     ppl_project_dimensions(Polyhedron,InDims),
         % Then take the convex hull with the copy before failing.
     ppl_convex_hull_assign(Polyhedron,Q),
     fail
     ;
         % On success, project away all but the head variables.
     ppl_project_dimensions(Polyhedron,OutDims)
    ))
  ->
    ppl_delete_polyhedron(Q)   % Q is now unwanted and removed.
  ;
    ppl_delete_polyhedron(Q),  % Q is now unwanted and removed.
    fail
  ).

/*
The constraints are solved by inserting them into the polyhedron.
*/
solve_constraints((C,D),Polyhedron):- 
    !,
    solve_constraints(C,Polyhedron),
    solve_constraints(D,Polyhedron).
solve_constraints(C,Polyhedron):- 
    ppl_insert_constraint(Polyhedron,C).

/*
The two lists are made equal in the polyhedron.
*/
solve_equal_list([],[],_Polyhedron).
solve_equal_list([A|As],[B|Bs],Polyhedron):-
    solve_constraints(A=B,Polyhedron),
    solve_equal_list(As,Bs,Polyhedron).
    
/*
Rename makes the second list a copy of the first except 
for the unbound variables.
*/
rename([],[]).
rename([A|As],[_B|Bs]):-
    var(A),
    !,
    rename(As,Bs).
rename([A|As],[A|Bs]):-
    rename(As,Bs).

/*
The Polyhedron is projected to have NewDims variables (dimensions).
That is all variables with codes greater than NewDims are projected away.
*/
ppl_project_dimensions(Polyhedron,NewDims):-
    ppl_space_dimension(Polyhedron,SpaceDims),
    make_var_list(SpaceDims,NewDims,VarList),
    ppl_remove_dimensions(Polyhedron, VarList).

/*
Given the minimum and maximum variable codes,
this sets up a list of variables in the right syntax for using
ppl_remove_dimensions in the PPL.
*/
make_var_list(MaxCode,MaxCode,[]).
make_var_list(MaxCode,VarCode,['$VAR'(VarCode)|VarList]):-
    VarCode < MaxCode,
    VarCode1 is VarCode+1,
    make_var_list(MaxCode,VarCode1,VarList).

/*
Displays the constraints.
Useful for debugging and also used at the moment for the output.
*/
check_constraints(Polyhedron) :-
    ppl_space_dimension(Polyhedron, D),
    write(D), write(' * '),
    ppl_get_constraints(Polyhedron, CS),
    write(CS), write(' % ').

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
