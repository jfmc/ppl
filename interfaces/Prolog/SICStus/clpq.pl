% A toy, non-ground meta-interpreter for CLP(Q)
% for testing the Parma Polyhedra Library and its Prolog interface.
%
% Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
%
% This file is part of the Parma Polyhedra Library (PPL).
%
% The PPL is free software; you can redistribute it and/or modify it
% under the terms of the GNU General Public License as published by the
% Free Software Foundation; either version 2 of the License, or (at your
% option) any later version.
%
% The PPL is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
% USA.
%
% For the most up-to-date information see the Parma Polyhedra Library
% site: http://www.cs.unipr.it/ppl/ .

:- ensure_loaded(ppl_sicstus).

% Object-level clauses are stored as user_clause(Head, Body) facts.
:- dynamic user_clause/2.

% Used to store the names of variables occurring in the original goal
% as a list of the form [ Name1 = Variable1, ... ]
:- dynamic original_goal_variables/1.


% solve(+Goals, +VariableNames)
%
% Tries to solve the query `Goals' and to present the results
% to the user by referring to the original variable names
% contained in `VariableNames'.

solve(Goals, VariableNames) :-
    numvars(Goals, 0, Dims),
    assertz(original_goal_variables(VariableNames)),
    % The initial polyhedron is initialised with
    % `Dims' dimensions, the number of variables in `Goals'.
    ppl_new_polyhedron(Polyhedron, Dims),
    % Try to reduce `Goals' to the empty continuation.
    solve(Goals, true, Polyhedron),
    % The one who creates the polyhedron must delete it.
    ppl_delete_polyhedron(Polyhedron),
    % Further cleaning.
    retract(original_goal_variables(_)).


solve(true, true, Polyhedron) :-
    !,
    % It is time to print the result and see if the user
    % wants to look for more solutions.
    ppl_copy_polyhedron(Polyhedron, Q),
    original_goal_variables(VariableNames),
    length(VariableNames, Dims),
    ppl_remove_higher_dimensions(Q, Dims),
    ppl_get_constraints(Q, CS),
    write_constraints(CS, VariableNames),
    ppl_delete_polyhedron(Q),
    % More?
    % If query_next_solution succeeds,
    % then no more solutions are required.
    query_next_solution.

solve(true, (G, Goals), Polyhedron) :-
    !,
    solve(G, Goals, Polyhedron).

solve((A, B), Goals, Polyhedron) :-
    !,
    solve(A, (B, Goals), Polyhedron).

solve({}, Goals, Polyhedron) :-
    !,
    % The empty set of constraints is equivalent to true.
    solve(true, Goals, Polyhedron).
    
solve({ Constraints }, Goals, Polyhedron) :-
    !,
    % Solve the constraints using the constraint solver.
    solve_constraints(Constraints, Polyhedron),
    solve(true, Goals, Polyhedron).

% Built-ins may be added here.

solve(Atom, Goals, Polyhedron) :-
    % Here is a choicepoint: possibly different clauses
    % will be selected on backtracking.
    % NOTE: we may fail to find (another) clause,
    %       but we have allocated nothing yet.
    select_clause(Atom, Head, Body),

    % Copy the current polyhedron and work on the copy.
    % NOTE: the copy is under our responsibility, i.e.,
    %       it is our job to delete it, sooner or later.
    ppl_copy_polyhedron(Polyhedron, PolyCopy),

    % Rename the selected clause apart and extend the polyhedron.
    ppl_space_dimension(PolyCopy, Dims),
    numvars((Head, Body), Dims, NewDims),
    AddedDims is NewDims - Dims,
    ppl_add_dimensions_and_embed(PolyCopy, AddedDims),

    % Parameter passing.
    parameter_passing(Atom, Head, PP_Constraints),

    % Try to solve the body augmented with the parameter passing equations.
    (solve(PP_Constraints, (Body, Goals), PolyCopy) ->
	true
    ;
	ppl_delete_polyhedron(PolyCopy),
	fail
    ),
    % Our copy must be thrown anyway.
    ppl_delete_polyhedron(PolyCopy).


parameter_passing(Atom, Head, PP_Constraints) :-
    Atom =.. [_|Actuals],
    Head =.. [_|Formals],
    (Actuals == [] ->
	PP_Constraints = true
    ;
	build_pp_constraints(Actuals, Formals, Equations),
	PP_Constraints = ({ Equations })
    ).

build_pp_constraints([A|Actuals], [F|Formals], Equations) :-
    (Actuals == [] ->
	Equations = (A = F)
    ;
	Equations = ((A = F), More_Equations),
	build_pp_constraints(Actuals, Formals, More_Equations)
    ).

select_clause(Atom, Head, Body) :-
    functor(Atom, F, N),
    functor(Head, F, N),
    user_clause(Head, Body).

% The constraints are solved by inserting them into the polyhedron.
solve_constraints(Constraints, Polyhedron) :-
    insert_constraints(Constraints, Polyhedron),
    \+ ppl_check_empty(Polyhedron).

insert_constraints((C, D), Polyhedron) :-
    !,
    solve_constraints(C, Polyhedron),
    solve_constraints(D, Polyhedron).
insert_constraints(C, Polyhedron) :-
    ppl_insert_constraint(Polyhedron, C).

%%%%%%%%%%%%%%%%%% Query the User for More Solutions %%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%% Reading Programs %%%%%%%%%%%%%%%%%%%%%%%%%%%

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
      assertz(user_clause(Clause, true))
    ),
    read_clauses(Stream)
  ;
    true
  ).

%%%%%%%%%%%%%%%%%%%%% The User's Interaction Loop %%%%%%%%%%%%%%%%%%%%%%

write_error(Message) :-
  write('clpq error: '),
  write_error_aux(Message).

write_error_aux([]) :-
  nl.
write_error_aux([H|T]) :-
  write(H),
  write_error_aux(T).

main_loop :-
  write('PPL clpq ?- '),
  read_term(Command, [variable_names(VN)]),
  do_command(Command, VN).

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

do_command(end_of_file, _VN) :-
  !.
do_command(halt, _VN) :-
  !.
do_command(trace, _VN) :-
  !,
  trace,
  main_loop_yes.
do_command(notrace, _VN) :-
  !,
  notrace,
  main_loop_yes.
do_command(spy, _VN) :-
  !,
  read(PredList),
  Spy =.. [spy|PredList],
  Spy,
  main_loop_yes.
do_command([], _VN) :-
  !,
  (read_programs([]) ; true),
  main_loop_yes.
do_command([H|T], _VN) :-
  !,
  (read_programs([H|T]) ; true),
  main_loop_yes.
do_command(consult(Program), _VN) :-
  !,
  (read_program(Program) ; true),
  main_loop_yes.
do_command(reconsult(Program), _VN) :-
  !,
  clear_program,
  do_command(consult(Program)).
do_command(listing, _VN) :-
  !,
  list_program,
  main_loop_yes.
do_command(statistics, _VN) :-
  !,
  statistics,
  main_loop_yes.
do_command(Query, VN) :-
  solve(Query, VN),
  main_loop_yes.
do_command(_, _VN) :-
  main_loop_no.

main_loop_no :-
  write(no),
  nl,
  main_loop.

main_loop_yes :-
  write(yes),
  nl,
  main_loop.

%%%%%%%%%%%%%%%%% Writing Computed Answer Constraints %%%%%%%%%%%%%%%%%%

write_var(Var, VariableNames) :-
    member(Name=Var, VariableNames),
    !,
    write(Name).

negate_expr(Num*Var, NegExpr) :-
    (Num < 0 ->
	NegNum is -Num,
	NegExpr = NegNum*Var
    ;
	NegExpr = Num*Var
    ).
negate_expr(Expr1 + Expr2, NegExpr1 + NegExpr2) :-
    negate_expr(Expr1, NegExpr1),
    negate_expr(Expr2, NegExpr2).

write_expr(Num*Var, VariableNames) :-
    (Num =:= 1 ->
	true
    ;
	(Num =:= -1 ->
	    write('-')
	;
	    write(Num),
	    write('*')
	)
    ),
    write_var(Var, VariableNames).
write_expr(E + Num*Var, VariableNames) :-
    write_expr(E, VariableNames),
    (Num < 0 ->
	write(' - '),
	NegNum is -Num,
        write_expr(NegNum*Var, VariableNames)
    ;
	write(' + '),
        write_expr(Num*Var, VariableNames)
    ).

write_constraint(Expr = Num, VariableNames) :-
    write_expr(Expr, VariableNames),
    write(' = '),
    write(Num).
write_constraint(Expr >= Num, VariableNames) :-
    (Num < 0 ->
	negate_expr(Expr, NegExpr),
	write_expr(NegExpr, VariableNames),
	write(' =< '),
	NegNum is -Num,
        write(NegNum)
    ;
	write_expr(Expr, VariableNames),
	write(' >= '),
	write(Num)
    ).

write_constraints([], _VariableNames).
write_constraints([C|CS], VariableNames) :-
    write_constraint(C, VariableNames),
    nl,
    write_constraints(CS, VariableNames).

%%%%%%%%%%%%%%%%%%%%%%%%%% Utility Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%

% member(?Element, +List)
%
% Suceeds when Element is a member of List.  It may be used to test 
% for membership in a list, but it can also be used to enumerate all 
% the elements in List.

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% Auxiliary to avoid the creation of a choicepoint for the last element,
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).


% numvars(?Term, +InN, ?OutN)
%
% Unifies each of the variables in Term with the special terms
% '$VAR'(k), where k ranges from InN to OutN-1.

numvars('$VAR'(InN), InN, OutN) :-
    !,
    OutN is InN + 1.
numvars(Term, InN, OutN) :-
    Term =.. [_|Args],
    numvars_list(Args, InN, OutN).

numvars_list([], InN, InN).
numvars_list([Arg|Args], InN, OutN) :-
    numvars(Arg, InN, TmpN),
    numvars_list(Args, TmpN, OutN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Startup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-
  nofileerrors, % FIXME: this is not ISO Prolog
  main_loop.
