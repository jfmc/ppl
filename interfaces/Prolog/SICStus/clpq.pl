:- ensure_loaded(ppl_sicstus).

/*
  A non-ground meta-interpreter for CLP(Q) for use with the Parma
  Polyhedra Library.  It is based on the well-known "solve" or
  "vanilla" interpreter.
*/

/*
solve/1 is the top-level predicate.
A top level query has the form:
?- solve(+query).
For example, 
?- solve(p1(A,B)).
*/
solve(Goal,Polyhedron,[Q|Qs]):-
% Polyhedron: the main polyhedron is initialised with 0 dimensions.
    numbervars(Goal,0,Dims),
    ppl_new_polyhedron(Polyhedron, Dims), 
    ppl_copy_polyhedron(Polyhedron,Q), % A copy of Polyhedron is kept.
    solve(Goal,Polyhedron,Dims,_,Q,[],Qs),
    output_constraints(Polyhedron,Dims).


output_constraints(Polyhedron,Dims):-
    ppl_copy_polyhedron(Polyhedron,Q),
    ppl_remove_higher_dimensions(Q,Dims),
    check_constraints(Q),
    ppl_delete_polyhedron(Q).

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
solve(true,Polyhedron,Dims,Dims,Q,Qs,Qs):-
    !,
    ppl_remove_higher_dimensions(Polyhedron,Dims),
    ppl_convex_hull_assign(Polyhedron,Q).

%%% The case when the query is equality.
         % The special constraint solver is called.
         % A and B must both be linear rational expressions.
solve(A=B,Polyhedron,InDims,OutDims,Q,Qs,Qs):-
    !, 
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    solve({A=B},Polyhedron,InDims,OutDims,Q,Qs,Qs).

%%% The case when the query is a set of constraints.
solve({Cs},Polyhedron,InDims,OutDims,Q,Qs,Qs):- 
    !,
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
         % The variables are now frozen for using with the PPL.
    numbervars(Cs,InDims,OutDims),
         % The number of extra dimensions is the number of new variables.
    AddedDims is OutDims-InDims,
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % Solve the constraints using the constraint solver.
    solve_constraints(Cs,Polyhedron,Q). 

%%% The case when the query is a conjunction.

solve((A,B),Polyhedron,InDims,OutDims,Q,InQs,[QB|Qs]):-
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    solve(A,Polyhedron,InDims,AOutDims,Q,InQs,QAs),
    ppl_remove_higher_dimensions(Polyhedron,AOutDims),
    ppl_copy_polyhedron(Polyhedron,QB), % A copy of Polyhedron is kept.
    solve(B,Polyhedron,AOutDims,OutDims,QB,QAs,Qs).

%%% The remaining case when it is a user-defined atomic goal.
solve(Atom,Polyhedron,InDims,OutDims,Q,InQs,OutQs):-
    user_clause(Atom, Body),
    numbervars(Atom,InDims,OutDims),
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    try_clause(Body,InDims,OutDims,Polyhedron,InQs,OutQs).

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

try_clause(Body,InDims,OutDims,Polyhedron,InQs,[Q1|Q1s]):-
    AddedDims is OutDims - InDims, 
         % OutDims - InDims is the number of new variables in Args.
         % The Polyhedron has the correct number of dimensions added.
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % Now we can solve the body.
    ppl_copy_polyhedron(Polyhedron,Q1), % A copy of Polyhedron is kept.
    solve(Body,Polyhedron,OutDims,_,Q1,InQs,Q1s).

/*
The constraints are solved by inserting them into the polyhedron.
*/
solve_constraints((C,D),Polyhedron,Q):- 
    !,
    solve_constraints(C,Polyhedron,Q),
    solve_constraints(D,Polyhedron,Q).
solve_constraints(C,Polyhedron,Q):- 
    ppl_insert_constraint(Polyhedron,C),
    (
     ppl_check_empty(Polyhedron)
    ->
     ppl_delete_polyhedron(Q),
     fail
    ;
     true
    ).

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
Displays the constraints.
Useful for debugging and also used at the moment for the output.
*/
check_constraints(Polyhedron) :-
    ppl_space_dimension(Polyhedron, D),
    write(D), write(' * '),
    ppl_get_constraints(Polyhedron, CS),
    write(CS), write(' % ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic user_clause/2.

write_error(Message) :-
  write('clpq error: '),
  write_error_aux(Message).

write_error_aux([]) :-
  nl.
write_error_aux([H|T]) :-
  write(H),
  write_error_aux(T).

read_programs([]).
read_programs([P|Ps]) :-
  read_program(P),
  read_programs(Ps).

read_program(Program) :-
  (atom(Program) ->
    true
  ;
    write_error(['read_program/1 - arg 1: expected file name, found ',
                 Program]),
    fail
  ),
  (open(Program, read, Stream) ->
    FileName = Program
  ;
    atom_concat(Program, '.clpq', FileName),
    (open(FileName, read, Stream) ->
      true
    ;
      write_error(['read_program/1 - arg 1: file ',
                   Program, ' does not exist']),
      fail
    )
  ),
  (read_clauses(Stream) ->
    close(Stream)
  ;
    write_error(['read_program/1 - arg 1: syntax error reading ', Program]),
    close(Stream),
    fail
  ).

read_clauses(Stream) :-
  read(Stream, Clause),
  (Clause \== end_of_file ->
    (Clause = (Head :- Body) ->
      assertz(user_clause(Head, Body))
    ;
      assertz(user_clause(Head, true))
    ),
    read_clauses(Stream)
  ;
    true
  ).

main_loop :-
  write('PPL clpq ?- '),
  read(Command),
  do_command(Command).
main_loop :- 
  main_loop.

clear_program :-
  retract(user_clause(_, _)),
  fail.
clear_program.

list_program :-
  user_clause(Head, Body),
  pp(Head, Body),
  fail.
list_program.

pp(Head, Body) :-
  write(Head),
  (Body == true ->
    write('.')
  ;
    write(' :- '),
    write(Body)
  ),
  nl.

do_command(end_of_file) :-
  !.
do_command(halt) :-
  !.
do_command(trace) :-
  !,
  trace,
  main_loop.
do_command(notrace) :-
  !,
  notrace,
  main_loop.
do_command(spy) :-
  !,
  read(PredList),
  Spy =.. [spy|PredList],
  Spy,
  main_loop.
do_command([]) :-
  !,
  (read_programs([]) ; true),
  main_loop.
do_command([H|T]) :-
  !,
  (read_programs([H|T]) ; true),
  main_loop.
do_command(consult(Program)) :-
  !,
  (read_program(Program) ; true),
  main_loop.
do_command(reconsult(Program)) :-
  !,
  clear_program,
  do_command(consult(Program)).
do_command(listing) :-
  !,
  list_program,
  main_loop.
do_command(Query) :-
  solve(Query,Polyhedron,Qs),
  query_next_solution,
  delete_polyhedra([Polyhedron|Qs]),
  main_loop.


delete_polyhedra([]).
delete_polyhedra([Q|Qs]):-
   ppl_delete_polyhedron(Q),
   delete_polyhedra(Qs).

query_next_solution :-
  write(' more? '),
  repeat,
  flush_output(user_output),
  get0(user_input, C),
  (
    C == 59, get0(user_input, _EOL)
  ;
    C == 10
  ;
    get0(user_input, _EOL),
    write('Action (";" for more choices, otherwise <return>): '),
    fail
  ),
  !,
  C = 10.

:-
  nofileerrors,
  main_loop.
