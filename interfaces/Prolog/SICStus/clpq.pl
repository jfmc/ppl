:- ensure_loaded(ppl_sicstus).

/*
  A non-ground meta-interpreter for CLP(Q) for use with the Parma
  Polyhedra Library.  It is based on the well-known "solve" or
  "vanilla" interpreter.
  At present, the object programs must be normalised.
*/

/*
solve/2 is the top-level predicate.
solve(+Goal,-Polyhedron,-Qs).
Goal: the goal which can be a conjunction of atoms and constraints.
      The atoms can have rationals or variables as arguments.
      Note that repeated variables in an atom are currently not supported.
Qs: a stack of copies of polyhdron to enable backtracking.
    These are to be removed once it is known that
    no more solutions are required.
*/

solve(Goal,[Polyhedron,Q|Qs]):-
    numbervars(Goal,0,Dims),
         % Polyhedron: the main polyhedron is initialised with 
         % dimensions = Dims, the number of variables in Goal.
    ppl_new_polyhedron(Polyhedron, Dims), 
         % A copy of Polyhedron is required by solve/7.
    ppl_copy_polyhedron(Polyhedron,Q),
    solve(Goal,Polyhedron,Dims,_,Q,[],Qs),
    output_constraints(Polyhedron,Dims).

/*
output_constraints/2 prints the results.
Before printing the results, the constraints are projected 
onto the variables the in Goal.
To allow for backtracking the projection is done on a
temporary copy of Polyhedron.
*/
output_constraints(Polyhedron,Dims):-
    ppl_copy_polyhedron(Polyhedron,Q),
    ppl_remove_higher_dimensions(Q,Dims),
    check_constraints(Q),
    ppl_delete_polyhedron(Q).

/*
solve/7 is the main meta-interpreter.
solve(+Goal, +Polyhedron, +Integer1, -Integer2, 
      +Q:polyhedron, +Qs1:[polyhedra], -Qs2:[polyhedra]).
Goal:       The query to be solved.
Polyhedron: A reference to a polyhedron which 
            represents the current set of constraints.
Integer1:   The initial number of dimensions of the Polyhedron.
Integer2:   The final number of dimensions of the Polyhedron.
Q:          A copy of Polyhedron.
Qs1:        The current list of temporary polyhedra.
Qs2:        The resulting list of temporary polyhedra.
*/

%%% The base case.
solve(true,_Polyhedron,Dims,Dims,_Q,Qs,Qs):-
    !.

%%% The case when the query is equality.
%%% A and B must both be linear rational expressions.
solve(A=B,Polyhedron,InDims,OutDims,Q,Qs,Qs):-
    !, 
         % In case of backtracking, the original state must be restored.
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
         % Interpret equality as a constraint.
    solve({A=B},Polyhedron,InDims,OutDims,Q,Qs,Qs).

%%% The case when the query is a set of constraints.
solve({Cs},Polyhedron,InDims,InDims,Q,Qs,Qs):- 
    !,
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
         % The number of extra dimensions is the number of new variables.
         % Solve the constraints using the constraint solver.
    solve_constraints(Cs,Polyhedron,Q). 

%%% The case when the query is a conjunction.
solve((A,B),Polyhedron,InDims,OutDims,Q,InQs,[QB|Qs]):-
         % On backtracking, the original state must be restored.
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    solve(A,Polyhedron,InDims,AOutDims,Q,InQs,QAs),
         % In case of backtracking, the previous state must be restored.
%    ppl_remove_higher_dimensions(Polyhedron,AOutDims),
         % A current copy of Polyhedron is needed for the call to solve.
    ppl_copy_polyhedron(Polyhedron,QB), 
    solve(B,Polyhedron,AOutDims,OutDims,QB,QAs,Qs).

%%% The remaining case when it is a user-defined atomic goal.
solve(Atom,Polyhedron,InDims,OutDims,Q,InQs,[Q1|Q1s]):-
    user_clause(Atom, Body),
         % New variables are frozen for using with the PPL.
    numbervars(Body,InDims,BOutDims),
         % On backtracking, the original state must be restored.
    ppl_remove_higher_dimensions(Polyhedron,InDims),
    ppl_convex_hull_assign(Polyhedron,Q),
    AddedDims is BOutDims - InDims, 
         % OutDims - InDims is the number of new variables in Args.
         % The Polyhedron has the correct number of dimensions added.
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % A copy of Polyhedron is needed for the call to solve.
    ppl_copy_polyhedron(Polyhedron,Q1), 
         % Now we can solve the body.
    solve(Body,Polyhedron,BOutDims,OutDims,Q1,InQs,Q1s).

/*
The constraints are solved by inserting them into the polyhedron.
This fails if the constraints are unsatisfiable.
*/
solve_constraints((C,D),Polyhedron,Q):- 
    !,
    solve_constraints(C,Polyhedron,Q),
    solve_constraints(D,Polyhedron,Q).
solve_constraints(C,Polyhedron,Q):- 
    ppl_insert_constraint(Polyhedron,C),
         % If the Polyhedron is empty, then we fail 
         % and delete the copy of the polyhedron.
    (
     ppl_check_empty(Polyhedron)
    ->
     ppl_delete_polyhedron(Q),
     fail
    ;
     true
    ).
check.
/*
The two lists are made equal in the polyhedron.
*/
solve_equal_list([],[],_Polyhedron).
solve_equal_list([A|As],[B|Bs],Polyhedron):-
    solve_constraints(A=B,Polyhedron),
    solve_equal_list(As,Bs,Polyhedron).

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
  nl,
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
  solve(Query,Qs),
  query_next_solution,
      % If query_next_solution succeeds,
      % then no more solutions are required and stack of
      % temporary polyhedra can be removed.
  delete_polyhedra(Qs),
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
