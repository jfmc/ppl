:- ensure_loaded(ppl_sicstus).

/*
  A non-ground meta-interpreter for CLP(Q) for use with the Parma
  Polyhedra Library.  It is based on the well-known "solve" or
  "vanilla" interpreter.
  At present, the object programs must be normalised.
*/

/*
solve/1 is the top-level predicate.
solve(+Goal).
Goal: the goal which can be a conjunction of atoms and constraints.
      The atoms can have rationals or variables as arguments.
      Note that repeated variables in an atom are currently not supported.
*/

solve(Goal):-
    numvars(Goal,0,Dims),
         % Polyhedron: the main polyhedron is initialised with 
         % dimensions = Dims, the number of variables in Goal.
    ppl_new_polyhedron(Polyhedron, Dims), 
         % A copy of Polyhedron is required by solve/7.
    solve(Goal,Polyhedron,Dims).
  
solve(Goal,Polyhedron,Dims):-
    ppl_get_generators(Polyhedron,GS),
    solve(Goal,Polyhedron,Dims,_,GS),
    output_constraints(Polyhedron,Dims).
solve(_,Polyhedron,_):-
    !,
    ppl_delete_polyhedron(Polyhedron),
    fail.


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
    ppl_get_constraints(Q, CS),
    write_constraints(CS),
    ppl_delete_polyhedron(Q).

/*
solve/7 is the main meta-interpreter.
solve(+Goal, +Polyhedron, +Integer1, -Integer2, 
      +GS:List_of_Generators).
Goal:       The query to be solved.
Polyhedron: A reference to a polyhedron which 
            represents the current set of constraints.
Integer1:   The initial number of dimensions of the Polyhedron.
Integer2:   The final number of dimensions of the Polyhedron.
GS:         Generators of Polyhedron when called.
*/

%%% The base case.
solve(true,_Polyhedron,Dims,Dims,_GS):-
    !.

%%% The case when the query is equality.
%%% A and B must both be linear rational expressions.
solve(A=B,Polyhedron,InDims,OutDims,GS):-
    !, 
         % Interpret equality as a constraint.
    solve({A=B},Polyhedron,InDims,OutDims,GS).

%%% The case when the query is a set of constraints.
solve({Constraints},Polyhedron,InDims,InDims,GS):-
    !,
    recover_original_polyhedron(Polyhedron,GS,InDims),
         % Solve the constraints using the constraint solver.
    solve_constraints(Constraints,Polyhedron),
         % If the Polyhedron is empty, then we fail. 
    (
     ppl_check_empty(Polyhedron)
    ->
     fail
    ;
     true
    ).

%%% The case when the query is a conjunction.
solve((A,B),Polyhedron,InDims,OutDims,GS):-
         % On backtracking, the original state must be restored.
    recover_original_polyhedron(Polyhedron,GS,InDims),
    solve(A,Polyhedron,InDims,AOutDims,GS),
         % A current copy of Polyhedron is needed for the call to solve.
    ppl_get_generators(Polyhedron,GSB), 
    solve(B,Polyhedron,AOutDims,OutDims,GSB).

%%% The remaining case when it is a user-defined atomic goal.
solve(Atom,Polyhedron,InDims,OutDims,GS):-
    user_clause(Atom, Body),
         % New variables are frozen for using with the PPL.
    numvars(Body,InDims,BOutDims),
         % On backtracking, the original state must be restored.
    recover_original_polyhedron(Polyhedron,GS,InDims),
    AddedDims is BOutDims - InDims, 
         % OutDims - InDims is the number of new variables in Args.
         % The Polyhedron has the correct number of dimensions added.
    ppl_add_dimensions_and_embed(Polyhedron, AddedDims),
         % A copy of Polyhedron is needed for the call to solve.
    ppl_get_generators(Polyhedron,GS1), 
         % Now we can solve the body.
    solve(Body,Polyhedron,BOutDims,OutDims,GS1).

recover_original_polyhedron(Polyhedron,GS,QDims):-
    !,
    ppl_remove_higher_dimensions(Polyhedron,QDims),
    insert_generators(Polyhedron,GS).

insert_generators(_Polyhedron,[]).
insert_generators(Polyhedron,GS):-
    GS \= [],
    find_vertex(GS,V,GS1),
    ppl_insert_generator(Polyhedron,vertex(V)),
    ppl_insert_generators(Polyhedron,GS1).

find_vertex([vertex(V)|GS],V,GS):-
    !.
find_vertex([line(L)|GS],L,[line(L)|GS1]):-
    !,
    find_vertex(GS,L,GS1).
find_vertex([ray(L)|GS],R,[ray(L)|GS1]):-
    !,
    find_vertex(GS,R,GS1).
   
/*
The constraints are solved by inserting them into the polyhedron.
*/
solve_constraints((C,D),Polyhedron):- 
    !,
    solve_constraints(C,Polyhedron),
    solve_constraints(D,Polyhedron).
solve_constraints(C,Polyhedron):- 
    ppl_insert_constraint(Polyhedron,C).

numvars(A,InN,OutN):-
    var(A),
    !,
    A = '$VAR'(InN),
    OutN is InN + 1.
numvars(A,InN,OutN):-
    A =.. [_|Args],
    numvars_list(Args,InN,OutN).

numvars_list([],InN,InN).
numvars_list([Arg|Args],InN,OutN):-
    numvars(Arg,InN,N),
    numvars_list(Args,N,OutN).

write_constraints([]).
write_constraints([C|CS]):-
   write(C),
   nl,
   write_constraints(CS).

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
  % write(Head),
  (Body == true ->
    % write('.')
    portray_clause(Head)
  ;
    % write(' :- '),
    % write(Body)
    portray_clause((Head :- Body))
  ),
  nl.

do_command(end_of_file) :-
  !.
do_command(halt) :-
  !.
do_command(trace) :-
  !,
  trace,
  main_loop_yes.
do_command(notrace) :-
  !,
  notrace,
  main_loop_yes.
do_command(spy) :-
  !,
  read(PredList),
  Spy =.. [spy|PredList],
  Spy,
  main_loop_yes.
do_command([]) :-
  !,
  (read_programs([]) ; true),
  main_loop_yes.
do_command([H|T]) :-
  !,
  (read_programs([H|T]) ; true),
  main_loop_yes.
do_command(consult(Program)) :-
  !,
  (read_program(Program) ; true),
  main_loop_yes.
do_command(reconsult(Program)) :-
  !,
  clear_program,
  do_command(consult(Program)).
do_command(listing) :-
  !,
  list_program,
  main_loop_yes.
do_command(statistics) :-
  !,
  statistics,
  main_loop_yes.
do_command(Query) :-
  solve(Query),
  query_next_solution,
      % If query_next_solution succeeds,
      % then no more solutions are required and stack of
      % temporary polyhedra can be removed.
  main_loop_yes.
do_command(_) :-
  main_loop_no.

main_loop_no :-
  write(no),
  nl,
  main_loop.

main_loop_yes :-
  write(yes),
  nl,
  main_loop.

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
