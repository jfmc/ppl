% A toy, non-ground meta-interpreter for CLP(Q)
% for testing the Parma Polyhedra Library and its Prolog interface.
%
% Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>
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

% Object-level clauses are stored as user_clause(Head, Body) facts.
:- dynamic(user_clause/2).

% solve_query(+Goals, +Variable_Name_Table, -Polyhedra)
%
% Tries to solve the query 'Goals'.
% 'Variable_Name_Table' is a list of input variable names and
% the corresponding variable.
% 'Polyhedra' are the live polyhedra; the head of the list
% represents the result of the computation.

solve_query(Goals, VN, PolysOut) :-
    % Create a table "FrozeVN" between the names of variables as
    % input and those as used by the ppl in the polyhedra.
    % Also another table "VarNames" between the ppl names of variables
    % and the actual variables.
    freezevars(VN, FrozeVN, 0, Dims, [], VarNames),
    % Use the latter table to create a copy of the Goals
    % but where the actual variables are replaced by their ppl names.
    freezevars(Goals, FrozeGoals, Dims, _, VarNames, _),
    % The initial polyhedron is initialised with
    % "Dims" dimensions, the number of variables in "Goals".
    % We use the NNC topology so that we can handle strict constraints.
    Topology = nnc,
    ppl_new_Polyhedron_from_dimension(Topology, Dims, Poly),
    % Try to reduce "Goals".
    solve(Topology, FrozeGoals, [Poly], PolysOut, VarNames),
    % Use the last polyhedron PolyOut that has been added to the list
    % for generating the resulting set of constraints.
    PolysOut = [PolyOut|_],
    % First project onto the variables of interest
    % before getting the constraints.
    ppl_new_Polyhedron_from_Polyhedron(Topology, PolyOut, Topology, Q),
    ppl_Polyhedron_remove_higher_dimensions(Q, Dims),
    ppl_Polyhedron_get_constraints(Q, CS),
    % Print the result.
    write_constraints(CS, FrozeVN),
    ppl_delete_Polyhedron(Q).

solve(_, true, Polys, Polys, _VarNames) :-
    % If the goal is true, we can return the input list of
    % non-empty polyhedron as output.
    % The head of the list will contain the solution to the query.
    !.

solve(T, (A, B), PolysIn, PolysOut, VarNames) :-
    !,
    % conjunction is solved using the output list of non-empty
    % polyhedra from the first component for input to the second.
    solve(T, A, PolysIn, Polys1, VarNames),
    solve(T, B, Polys1, PolysOut, VarNames).

solve(T, (A; B), PolysIn, PolysOut, VarNames) :-
    % disjunction is dealt with by making a copy of the polyhedron
    % before starting each branch.
    PolysIn = [Poly|_],
    ((ppl_new_Polyhedron_from_Polyhedron(T, Poly, T, Q),
    solve(T, A, [Q|PolysIn], PolysOut, VarNames));
    (ppl_new_Polyhedron_from_Polyhedron(T, Poly, T, Q),
    solve(T, B, [Q|PolysIn], PolysOut, VarNames))).

solve(_, {}, Polys, Polys, _VarNames) :-
    % If the goal is an empty set of constraints, then this is
    % the same as for 'true' and we can return the input list of
    % non-empty polyhedron as output.
    !.

solve(_, { Constraints }, [Poly|Polys], [Poly|Polys], _VarNames) :-
    !,
    % Solve the constraints using the constraint solver.
    constraints2list(Constraints, ConstraintsList),
    % Fails if 'Poly' becomes empty.
    (ppl_Polyhedron_add_constraints_and_minimize(Poly, ConstraintsList)
    ->
    true
    ;
    % if the constraints are unsatisfiable,
    % first throw the empty polyhedron away and then fail.
     ppl_delete_Polyhedron(Poly),
     fail
    ).

% Built-ins may be added here.

% read/1
solve(_, read(N), Polys, Polys, VarNames) :-
    Polys = [Poly|_],
    meltvars(N, MeltN, VarNames),
    read(MeltN),
    get_code(user_input, _C),
    (integer(MeltN) ->
       true
    ;
       ppl_delete_polyhedron(Poly),
       fail
    ),

    % add the new binding to the polyhedron.
    (ppl_Polyhedron_add_constraints_and_minimize(Poly, [N = MeltN])
    ->
       true
    ;
       % if the new value makes the constraints unsatisfiable,
       % first throw the empty polyhedron away and then fail.
       ppl_delete_polyhedron(Poly),
       fail
    ).

% write/1
solve(_, write(Message), Polys, Polys, _VarNames) :-
    write(Message).
% nl/0
solve(_, nl, Polys, Polys, _VarNames) :-
    nl.

solve(Topology, Atom, [Poly|Polys], PolysOut, VarNames) :-
    % Here is a choicepoint: possibly different clauses
    % will be selected on backtracking.
    % NOTE: we may fail to find (another) clause,
    %       but we have allocated nothing yet.
    select_clause(Atom, Head, Body),

    % Copy the current polyhedron and work on the copy.
    ppl_new_Polyhedron_from_Polyhedron(Topology, Poly, Topology, PolyCopy),

    % Rename the selected clause apart and extend the polyhedron.
    ppl_Polyhedron_space_dimension(PolyCopy, Dims),

    % Parameter passing.
    parameter_passing(Atom, Head, PP_ConstraintsList),

    numvars(Body, Dims, NewDims),
    AddedDims is NewDims - Dims,
    ppl_Polyhedron_add_dimensions_and_embed(PolyCopy, AddedDims),

    % First solve the parameter passing equations.
    (ppl_Polyhedron_add_constraints_and_minimize(PolyCopy, PP_ConstraintsList)
    ->
    % If satisfiable, try to solve the body.
    % The input list of used polyhedra is augmented with the new copy.
      solve(Topology, Body, [PolyCopy,Poly|Polys], PolysOut, VarNames)
    ;
    % if the parameter passing constraints are unsatisfiable,
    % first throw the empty polyhedron away and then fail.
     ppl_delete_Polyhedron(PolyCopy),
     fail
    ).

parameter_passing(Atom, Head, PP_Constraints) :-
    Atom =.. [_|Actuals],
    Head =.. [_|Formals],
    build_pp_constraints(Actuals, Formals, PP_Constraints).

% When the direct binding exists, we use unification.
% Otherwise, we add the constraint.
% By only adding new variables when needed, the computation
% is much more efficient.
build_pp_constraints([], [], []).
build_pp_constraints([A|Actuals], [F|Formals], NewEquations) :-
    build_pp_constraints(Actuals, Formals, Equations),
    (A = F ->
      NewEquations = Equations
    ;
      ((integer(A) ; integer(F) ; A = '$VAR'(_) ; F = '$VAR'(_)) ->
        NewEquations = [(A = F)|Equations]
      ;
        (A =.. [AFunct|Aargs],
        F =.. [FFunct|Fargs],
        (AFunct == FFunct ->
          % functors agree so we process the arguments.
          (build_pp_constraints(Aargs, Fargs, Equations1),
          append(Equations1, Equations, NewEquations))
	;
          % unification fails so we force the constraints to fail
          NewEquations = [0 = 1]
        ))        
      )
    ).

select_clause(Atom, Head, Body) :-
    functor(Atom, F, N),
    functor(Head, F, N),
    user_clause(Head, Body).

delete_all_polyhedra([]).
delete_all_polyhedra([Polyhedron|Polyhedra]):-
    ppl_delete_Polyhedron(Polyhedron),
    delete_all_polyhedra(Polyhedra).

%%%%%%%%%%%%%%%%%% Query the User for More Solutions %%%%%%%%%%%%%%%%%%%

query_next_solution :-
  write(' more? '),
  repeat,
  flush_output(user_output),
  get_code(user_input, C),
  (
    C == 59,
    % Get rid of the EOL character.
    get_code(user_input, _EOL)
  ;
    C == 10
  ;
    write('Action (";" for more choices, otherwise <return>): '),
    eat_to_eol,
    fail
  ),
  !,
  C = 10.

eat_to_eol :-
  get_code(user_input, C),
  (C == 10 ->
    true
  ;
    eat_to_eol
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%% Reading Programs %%%%%%%%%%%%%%%%%%%%%%%%%%%

open_file_for_reading(File, Stream) :-
  catch(open(File, read, Stream), _, fail).

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
  (open_file_for_reading(Program, Stream) ->
    FileName = Program
  ;
    atom_concat(Program, '.clpq', FileName),
    (open_file_for_reading(FileName, Stream) ->
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
      (Clause \= (:- _) ->
       assertz(user_clause(Clause, true))
      ;
       true
      )
    ),
    read_clauses(Stream)
  ;
    true
  ).

%%%%%%%%%%%%%%%%%%%%% The Interaction Loop %%%%%%%%%%%%%%%%%%%%%%

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
  eat_eol,
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
  !,
  clear_program.
do_command(trace, _VN) :-
  !,
  trace,
  main_loop_yes.
do_command(notrace, _VN) :-
  !,
  notrace,
  main_loop_yes.
do_command(debug, _VN) :-
  !,
  debug,
  main_loop_yes.
do_command(nodebug, _VN) :-
  !,
  nodebug,
  main_loop_yes.
do_command(spy(Spec), _VN) :-
  !,
  spy(Spec),
  main_loop_yes.
do_command(nospy(Spec), _VN) :-
  !,
  nospy(Spec),
  main_loop_yes.
do_command(nospyall, _VN) :-
  !,
  nospyall,
  main_loop_yes.
do_command([], _VN) :-
  !,
  (read_programs([]) ; true),
  main_loop_yes.
do_command([H|T], _VN) :-
  !,
  (read_programs([H|T]); true),
  main_loop_yes.
do_command(consult(Program), _VN) :-
  !,
  (read_program(Program) ; true),
  main_loop_yes.
do_command(reconsult(Program), _VN) :-
  !,
  clear_program,
  do_command(consult(Program), _VN).
do_command(listing, _VN) :-
  !,
  list_program,
  main_loop_yes.
do_command(statistics, _VN) :-
  !,
  statistics,
  main_loop_yes.

do_command(Query, VN) :-
  solve_query(Query, VN, PolysOut),
  % See if the user wants to look for more solutions.
  query_next_solution,
  % When finished, remove remaining polyhedra.
  delete_all_polyhedra(PolysOut),
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

write_var('$VAR'(N), NameList) :-
    member(Name = '$VAR'(N), NameList),
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

write_expr('$VAR'(N), VariableNames) :-
    write_var('$VAR'(N), VariableNames).
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
   (var(Num) ->
    fail
   ;
    write_expr(Expr, VariableNames),
    write(' = '),
    (integer(Num) ->
     write(Num)
    ;
     Num = rat(Int,1),
     write(Int)
    )
   ).
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

write_constraint(Expr > Num, VariableNames) :-
    (Num < 0 ->
	negate_expr(Expr, NegExpr),
	write_expr(NegExpr, VariableNames),
	write(' < '),
	NegNum is -Num,
        write(NegNum)
    ;
	write_expr(Expr, VariableNames),
	write(' > '),
	write(Num)
    ).
write_constraints([], _VariableNames).
write_constraints([C|CS], VariableNames) :-
    (write_constraint(C, VariableNames)
    ->
     nl
    ;
     true
    ),
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


% freezevars(?Term, -Term, +InN, ?OutN, ?List)
%
% Pairs each of the variables in Term with the special terms
% '$VAR'(k), where k ranges from InN to OutN-1.
% A frozen version of the term is also returned.
% The original goal and its variables are not bound.

freezevars(X, '$VAR'(NX), InN, OutN, VarNames, VarNamesOut) :-
    var(X),
    !,
    (var_member(('$VAR'(NX) = X), VarNames)
    ->
    VarNamesOut = VarNames,
    OutN = InN
     ;
    NX = InN,
    append(VarNames, ['$VAR'(InN) = X], VarNamesOut),
    OutN is InN + 1
    ).
freezevars(Term, FrozenTerm, InN, OutN, VarNamesIn, VarNamesOut) :-
    Term =.. [F|Args],
    (F = rat ->
     Args = [FrozenTerm,_],
     OutN = InN,
     VarNamesOut = VarNamesIn
    ;
     freezevars_list(Args, FrozenArgs, InN, OutN, VarNamesIn, VarNamesOut),
     FrozenTerm =.. [F|FrozenArgs]
    ).

freezevars_list([], [], InN, InN, VarNames, VarNames):-
    !.
freezevars_list([Arg|Args], [FrozenArg|FrozenArgs], InN, OutN,
                 VarNamesIn, VarNamesOut) :-
    freezevars(Arg, FrozenArg, InN, TmpN, VarNamesIn, VarNames1),
    freezevars_list(Args, FrozenArgs, TmpN, OutN, VarNames1, VarNamesOut).

var_member(FX = X, [FX = Var|_VarNames]) :-
    X == Var,
    !.
var_member(VarPair, [_|VarNames]) :-
    var_member(VarPair, VarNames).

append([],Bs,Bs).
append([A|As],Bs,[A|Cs]) :-
    append(As,Bs,Cs).


meltvars('$VAR'(N), Var, VarNames) :-
   !,
   member('$VAR'(N) = Var, VarNames).
meltvars(FrozenTerm, Term, VarNames) :-
   FrozenTerm =.. [F|FrozenArgs],
   (integer(F) ->
    Term = F
    ;
    meltvars_list(FrozenArgs, Args, VarNames),
    Term =.. [F|Args]
   ).

meltvars_list([], [], _VarNames):-
    !.
meltvars_list([FrozenArg|FrozenArgs], [Arg|Args], VarNames) :-
    meltvars(FrozenArg, Arg, VarNames),
    meltvars_list(FrozenArgs, Args, VarNames).

constraints2list(C, LC) :-
    constraints2list(C, [], LC).

constraints2list((A, B), Rest, LC) :-
    !,
    constraints2list(B, Rest, BRest),
    constraints2list(A, BRest, LC).
constraints2list(C, Rest, Rest1) :-
    (check_constraint(C)
     -> 
     Rest1 = [C|Rest]
     ;
     Rest1 = [0 = 1]
    ).


list2constraints([], {}) :-
    !.
list2constraints(CSList, { CS }) :-
    list2constraints_aux(CSList, CS).

list2constraints_aux([A], A) :-
    !.
list2constraints_aux([A|Bs], (A,BCs)) :-
    list2constraints_aux(Bs, BCs).

residue2constraints([], {}) :-
    !.
residue2constraints(RCS, { CS }) :-
    residue2constraints_aux(RCS, CS).

residue2constraints_aux([[_] - {C}], C) :-
    !.
residue2constraints_aux([[_] - {C}|RCRest], (C,CRest)) :-
    residue2constraints_aux(RCRest, CRest).

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

build_equality_constraints([], []).
build_equality_constraints([Var = Num|Eqs], AllEqConstrs) :-
    build_equality_constraints(Eqs, EqConstrs),
    (nonvar(Num) ->
     Num = rat(Int,1),
     AllEqConstrs = [Var = Int|EqConstrs]
    ;
     AllEqConstrs = EqConstrs
    ).

check_expr('$VAR'(_)).
check_expr(Num) :-
    integer(Num).
check_expr(Num*Var) :-
    integer(Num),
    check_expr(Var).
check_expr(E + F) :-
    check_expr(E),
    check_expr(F).
check_expr(E - F) :-
    check_expr(E),
    check_expr(F).

check_constraint(Expr = Expr1) :-
    check_expr(Expr),
    check_expr(Expr1).
check_constraint(Expr >= Expr1) :-
    check_expr(Expr),
    check_expr(Expr1).
check_constraint(Expr > Expr1) :-
    check_expr(Expr),
    check_expr(Expr1).
check_constraint(Expr =< Expr1) :-
    check_expr(Expr),
    check_expr(Expr1).
check_constraint(Expr < Expr1) :-
    check_expr(Expr),
    check_expr(Expr1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Startup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

common_main :-
    write('\
Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>\n\
this program is free software, covered by the GNU General Public License,\n\
and you are welcome to change it and/or distribute copies of it\n\
under certain conditions.\n\
Type "copying" to see the conditions.\n\
There is ABSOLUTELY NO WARRANTY for this program.\n\
Type "warranty" for details.\n'),
  main_loop.
