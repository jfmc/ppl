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
  % Create a table `FrozeVN' between the names of variables as
  % input and those as used by the PPL in the polyhedra.
  % Also another table `VarNames' between the ppl names of variables
  % and the actual variables.
  freezevars(VN, FrozeVN, 0, Dims, [], VarNames),
  % Use the latter table to create a copy of the Goals
  % but where the actual variables are replaced by their PPL names.
  freezevars(Goals, FrozeGoals, Dims, _, VarNames, _),
  % The initial polyhedron is initialised with
  % `Dims' dimensions, the number of variables in `Goals'.
  % We use the NNC topology so that we can handle strict constraints.
  Topology = nnc,
  ppl_new_Polyhedron_from_dimension(Topology, Dims, Poly),
  % Try to reduce `Goals'.
  solve(Topology, FrozeGoals, [Poly], PolysOut, VarNames),
  % Use the last polyhedron `PolyOut' that has been added to the list
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
  % Conjunction is solved using the output list of non-empty
  % polyhedra from the first component for input to the second.
  solve(T, A, PolysIn, PolysTmp, VarNames),
  solve(T, B, PolysTmp, PolysOut, VarNames).

solve(T, (A; B), PolysIn, PolysOut, VarNames) :-
  % Disjunction is dealt with by making a copy of the polyhedron
  % before starting each branch.
  PolysIn = [Poly|_],
  (
    (ppl_new_Polyhedron_from_Polyhedron(T, Poly, T, Q),
      solve(T, A, [Q|PolysIn], PolysOut, VarNames))
  ;
    (ppl_new_Polyhedron_from_Polyhedron(T, Poly, T, Q),
      solve(T, B, [Q|PolysIn], PolysOut, VarNames))
  ).

solve(_, {}, Polys, Polys, _VarNames) :-
  % If the goal is an empty set of constraints, then this is
  % the same as for `true' and we can return the input list of
  % non-empty polyhedron as output.
  !.

solve(_, { Constraints }, [Poly|Polys], [Poly|Polys], _VarNames) :-
  !,
  % Solve the constraints using the constraint solver.
  constraints2list(Constraints, ConstraintsList),
  % Fails if `Poly' becomes empty.
  (ppl_Polyhedron_add_constraints_and_minimize(Poly, ConstraintsList) ->
    true
  ;
    % If the constraints are unsatisfiable,
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
    ppl_delete_Polyhedron(Poly),
    fail
  ),

  % Add the new binding to the polyhedron.
  (ppl_Polyhedron_add_constraints_and_minimize(Poly, [N = MeltN]) ->
    true
  ;
    % If the new value makes the constraints unsatisfiable,
    % first throw the empty polyhedron away and then fail.
    ppl_delete_Polyhedron(Poly),
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
  (ppl_Polyhedron_add_constraints_and_minimize(PolyCopy, PP_ConstraintsList) ->
    % If satisfiable, try to solve the body.
    % The input list of used polyhedra is augmented with the new copy.
    solve(Topology, Body, [PolyCopy, Poly|Polys], PolysOut, VarNames)
  ;
    % If the parameter passing constraints are unsatisfiable,
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
      A =.. [AFunct|Aargs],
      F =.. [FFunct|Fargs],
      (AFunct == FFunct ->
        % Functors agree so we process the arguments.
        build_pp_constraints(Aargs, Fargs, Equations1),
        append(Equations1, Equations, NewEquations)
      ;
        % Unification fails so we force the constraints to fail.
        NewEquations = [0 = 1]
      )        
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
      assertz(user_clause(Clause, true))
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
do_command(warranty, _VN) :-
  !,
  show_warranty,
  main_loop_yes.
do_command(copying, _VN) :-
  !,
  show_copying,
  main_loop_yes.
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
      Num = rat(Int, 1),
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
  (write_constraint(C, VariableNames) ->
    nl
  ;
    true
  ),
  write_constraints(CS, VariableNames).

%%%%%%%%%%%%%%%%%%%%%%%%%% Utility Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%

% freezevars(?Term, -Term, +InN, ?OutN, ?List)
%
% Pairs each of the variables in Term with the special terms
% '$VAR'(k), where k ranges from InN to OutN-1.
% A frozen version of the term is also returned.
% The original goal and its variables are not bound.

freezevars(X, '$VAR'(NX), InN, OutN, VarNames, VarNamesOut) :-
  var(X),
  !,
  (var_member(('$VAR'(NX) = X), VarNames) ->
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
    Args = [FrozenTerm, _],
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
  (check_constraint(C) -> 
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
list2constraints_aux([A|Bs], (A, BCs)) :-
  list2constraints_aux(Bs, BCs).

residue2constraints([], {}) :-
  !.
residue2constraints(RCS, { CS }) :-
  residue2constraints_aux(RCS, CS).

residue2constraints_aux([[_] - {C}], C) :-
  !.
residue2constraints_aux([[_] - {C}|RCRest], (C, CRest)) :-
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
    Num = rat(Int, 1),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Legalese %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_or_quit :-
  write('---Type <return> to continue, or q <return> to quit---'),
  flush_output(user_output),
  get_code(user_input, C),
  (
    C == 10
  ;
    eat_eol
  ),
  !,
  C \== 113.

show_copying :-
  eat_eol,
  write('\
                    GNU GENERAL PUBLIC LICENSE\n\
                       Version 2, June 1991\n\
\n\
 Copyright (C) 1989, 1991 Free Software Foundation, Inc.\n\
                       59 Temple Place, Suite 330, Boston, MA  02111-1307  USA\n\
 Everyone is permitted to copy and distribute verbatim copies\n\
 of this license document, but changing it is not allowed.\n\
\n\
                            Preamble\n\
\n\
  The licenses for most software are designed to take away your\n\
freedom to share and change it.  By contrast, the GNU General Public\n\
License is intended to guarantee your freedom to share and change free\n\
software--to make sure the software is free for all its users.  This\n\
General Public License applies to most of the Free Software\n\
Foundation''s software and to any other program whose authors commit to\n\
using it.  (Some other Free Software Foundation software is covered by\n\
the GNU Library General Public License instead.)  You can apply it to\n\
your programs, too.\n\
\n\
  When we speak of free software, we are referring to freedom, not\n\
price.  Our General Public Licenses are designed to make sure that you\n\
have the freedom to distribute copies of free software (and charge for\n'),
  next_or_quit,
  write('\
this service if you wish), that you receive source code or can get it\n\
if you want it, that you can change the software or use pieces of it\n\
in new free programs; and that you know you can do these things.\n\
\n\
  To protect your rights, we need to make restrictions that forbid\n\
anyone to deny you these rights or to ask you to surrender the rights.\n\
These restrictions translate to certain responsibilities for you if you\n\
distribute copies of the software, or if you modify it.\n\
\n\
  For example, if you distribute copies of such a program, whether\n\
gratis or for a fee, you must give the recipients all the rights that\n\
you have.  You must make sure that they, too, receive or can get the\n\
source code.  And you must show them these terms so they know their\n\
rights.\n\
\n\
  We protect your rights with two steps: (1) copyright the software, and\n\
(2) offer you this license which gives you legal permission to copy,\n\
distribute and/or modify the software.\n\
\n\
  Also, for each author''s protection and ours, we want to make certain\n\
that everyone understands that there is no warranty for this free\n\
software.  If the software is modified by someone else and passed on, we\n\
want its recipients to know that what they have is not the original, so\n'),
  next_or_quit,
  write('\
that any problems introduced by others will not reflect on the original\n\
authors'' reputations.\n\
\n\
  Finally, any free program is threatened constantly by software\n\
patents.  We wish to avoid the danger that redistributors of a free\n\
program will individually obtain patent licenses, in effect making the\n\
program proprietary.  To prevent this, we have made it clear that any\n\
patent must be licensed for everyone''s free use or not licensed at all.\n\
\n\
  The precise terms and conditions for copying, distribution and\n\
modification follow.\n\
\n\
                    GNU GENERAL PUBLIC LICENSE\n\
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n\
\n\
  0. This License applies to any program or other work which contains\n\
a notice placed by the copyright holder saying it may be distributed\n\
under the terms of this General Public License.  The "Program", below,\n\
refers to any such program or work, and a "work based on the Program"\n\
means either the Program or any derivative work under copyright law:\n\
that is to say, a work containing the Program or a portion of it,\n\
either verbatim or with modifications and/or translated into another\n\
language.  (Hereinafter, translation is included without limitation in\n'),
  next_or_quit,
  write('\
the term "modification".)  Each licensee is addressed as "you".\n\
\n\
Activities other than copying, distribution and modification are not\n\
covered by this License; they are outside its scope.  The act of\n\
running the Program is not restricted, and the output from the Program\n\
is covered only if its contents constitute a work based on the\n\
Program (independent of having been made by running the Program).\n\
Whether that is true depends on what the Program does.\n\
\n\
  1. You may copy and distribute verbatim copies of the Program''s\n\
source code as you receive it, in any medium, provided that you\n\
conspicuously and appropriately publish on each copy an appropriate\n\
copyright notice and disclaimer of warranty; keep intact all the\n\
notices that refer to this License and to the absence of any warranty;\n\
and give any other recipients of the Program a copy of this License\n\
along with the Program.\n\
\n\
You may charge a fee for the physical act of transferring a copy, and\n\
you may at your option offer warranty protection in exchange for a fee.\n\
\n\
  2. You may modify your copy or copies of the Program or any portion\n\
of it, thus forming a work based on the Program, and copy and\n\
distribute such modifications or work under the terms of Section 1\n'),
  next_or_quit,
  write('\
above, provided that you also meet all of these conditions:\n\
\n\
    a) You must cause the modified files to carry prominent notices\n\
    stating that you changed the files and the date of any change.\n\
\n\
    b) You must cause any work that you distribute or publish, that in\n\
    whole or in part contains or is derived from the Program or any\n\
    part thereof, to be licensed as a whole at no charge to all third\n\
    parties under the terms of this License.\n\
\n\
    c) If the modified program normally reads commands interactively\n\
    when run, you must cause it, when started running for such\n\
    interactive use in the most ordinary way, to print or display an\n\
    announcement including an appropriate copyright notice and a\n\
    notice that there is no warranty (or else, saying that you provide\n\
    a warranty) and that users may redistribute the program under\n\
    these conditions, and telling the user how to view a copy of this\n\
    License.  (Exception: if the Program itself is interactive but\n\
    does not normally print such an announcement, your work based on\n\
    the Program is not required to print an announcement.)\n\
\n\
These requirements apply to the modified work as a whole.  If\n\
identifiable sections of that work are not derived from the Program,\n'),
  next_or_quit,
  write('\
and can be reasonably considered independent and separate works in\n\
themselves, then this License, and its terms, do not apply to those\n\
sections when you distribute them as separate works.  But when you\n\
distribute the same sections as part of a whole which is a work based\n\
on the Program, the distribution of the whole must be on the terms of\n\
this License, whose permissions for other licensees extend to the\n\
entire whole, and thus to each and every part regardless of who wrote it.\n\
\n\
Thus, it is not the intent of this section to claim rights or contest\n\
your rights to work written entirely by you; rather, the intent is to\n\
exercise the right to control the distribution of derivative or\n\
collective works based on the Program.\n\
\n\
In addition, mere aggregation of another work not based on the Program\n\
with the Program (or with a work based on the Program) on a volume of\n\
a storage or distribution medium does not bring the other work under\n\
the scope of this License.\n\
\n\
  3. You may copy and distribute the Program (or a work based on it,\n\
under Section 2) in object code or executable form under the terms of\n\
Sections 1 and 2 above provided that you also do one of the following:\n\
\n\
    a) Accompany it with the complete corresponding machine-readable\n'),
  next_or_quit,
  write('\
    source code, which must be distributed under the terms of Sections\n\
    1 and 2 above on a medium customarily used for software interchange; or,\n\
\n\
    b) Accompany it with a written offer, valid for at least three\n\
    years, to give any third party, for a charge no more than your\n\
    cost of physically performing source distribution, a complete\n\
    machine-readable copy of the corresponding source code, to be\n\
    distributed under the terms of Sections 1 and 2 above on a medium\n\
    customarily used for software interchange; or,\n\
\n\
    c) Accompany it with the information you received as to the offer\n\
    to distribute corresponding source code.  (This alternative is\n\
    allowed only for noncommercial distribution and only if you\n\
    received the program in object code or executable form with such\n\
    an offer, in accord with Subsection b above.)\n\
\n\
The source code for a work means the preferred form of the work for\n\
making modifications to it.  For an executable work, complete source\n\
code means all the source code for all modules it contains, plus any\n\
associated interface definition files, plus the scripts used to\n\
control compilation and installation of the executable.  However, as a\n\
special exception, the source code distributed need not include\n\
anything that is normally distributed (in either source or binary\n'),
  next_or_quit,
  write('\
form) with the major components (compiler, kernel, and so on) of the\n\
operating system on which the executable runs, unless that component\n\
itself accompanies the executable.\n\
\n\
If distribution of executable or object code is made by offering\n\
access to copy from a designated place, then offering equivalent\n\
access to copy the source code from the same place counts as\n\
distribution of the source code, even though third parties are not\n\
compelled to copy the source along with the object code.\n\
\n\
  4. You may not copy, modify, sublicense, or distribute the Program\n\
except as expressly provided under this License.  Any attempt\n\
otherwise to copy, modify, sublicense or distribute the Program is\n\
void, and will automatically terminate your rights under this License.\n\
However, parties who have received copies, or rights, from you under\n\
this License will not have their licenses terminated so long as such\n\
parties remain in full compliance.\n\
\n\
  5. You are not required to accept this License, since you have not\n\
signed it.  However, nothing else grants you permission to modify or\n\
distribute the Program or its derivative works.  These actions are\n\
prohibited by law if you do not accept this License.  Therefore, by\n\
modifying or distributing the Program (or any work based on the\n'),
  next_or_quit,
  write('\
Program), you indicate your acceptance of this License to do so, and\n\
all its terms and conditions for copying, distributing or modifying\n\
the Program or works based on it.\n\
\n\
  6. Each time you redistribute the Program (or any work based on the\n\
Program), the recipient automatically receives a license from the\n\
original licensor to copy, distribute or modify the Program subject to\n\
these terms and conditions.  You may not impose any further\n\
restrictions on the recipients'' exercise of the rights granted herein.\n\
You are not responsible for enforcing compliance by third parties to\n\
this License.\n\
\n\
  7. If, as a consequence of a court judgment or allegation of patent\n\
infringement or for any other reason (not limited to patent issues),\n\
conditions are imposed on you (whether by court order, agreement or\n\
otherwise) that contradict the conditions of this License, they do not\n\
excuse you from the conditions of this License.  If you cannot\n\
distribute so as to satisfy simultaneously your obligations under this\n\
License and any other pertinent obligations, then as a consequence you\n\
may not distribute the Program at all.  For example, if a patent\n\
license would not permit royalty-free redistribution of the Program by\n\
all those who receive copies directly or indirectly through you, then\n\
the only way you could satisfy both it and this License would be to\n'),
  next_or_quit,
  write('\
refrain entirely from distribution of the Program.\n\
\n\
If any portion of this section is held invalid or unenforceable under\n\
any particular circumstance, the balance of the section is intended to\n\
apply and the section as a whole is intended to apply in other\n\
circumstances.\n\
\n\
It is not the purpose of this section to induce you to infringe any\n\
patents or other property right claims or to contest validity of any\n\
such claims; this section has the sole purpose of protecting the\n\
integrity of the free software distribution system, which is\n\
implemented by public license practices.  Many people have made\n\
generous contributions to the wide range of software distributed\n\
through that system in reliance on consistent application of that\n\
system; it is up to the author/donor to decide if he or she is willing\n\
to distribute software through any other system and a licensee cannot\n\
impose that choice.\n\
\n\
This section is intended to make thoroughly clear what is believed to\n\
be a consequence of the rest of this License.\n\
\n\
  8. If the distribution and/or use of the Program is restricted in\n\
certain countries either by patents or by copyrighted interfaces, the\n'),
  next_or_quit,
  write('\
original copyright holder who places the Program under this License\n\
may add an explicit geographical distribution limitation excluding\n\
those countries, so that distribution is permitted only in or among\n\
countries not thus excluded.  In such case, this License incorporates\n\
the limitation as if written in the body of this License.\n\
\n\
  9. The Free Software Foundation may publish revised and/or new versions\n\
of the General Public License from time to time.  Such new versions will\n\
be similar in spirit to the present version, but may differ in detail to\n\
address new problems or concerns.\n\
\n\
Each version is given a distinguishing version number.  If the Program\n\
specifies a version number of this License which applies to it and "any\n\
later version", you have the option of following the terms and conditions\n\
either of that version or of any later version published by the Free\n\
Software Foundation.  If the Program does not specify a version number of\n\
this License, you may choose any version ever published by the Free Software\n\
Foundation.\n\
\n\
  10. If you wish to incorporate parts of the Program into other free\n\
programs whose distribution conditions are different, write to the author\n\
to ask for permission.  For software which is copyrighted by the Free\n\
Software Foundation, write to the Free Software Foundation; we sometimes\n'),
  next_or_quit,
  write('\
make exceptions for this.  Our decision will be guided by the two goals\n\
of preserving the free status of all derivatives of our free software and\n\
of promoting the sharing and reuse of software generally.\n'),
    !.
show_copying.

show_warranty :-
  write('\
                            NO WARRANTY\n\
\n\
  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY\n\
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN\n\
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES\n\
PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED\n\
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n\
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS\n\
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE\n\
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,\n\
REPAIR OR CORRECTION.\n\
\n\
  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR\n\
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,\n\
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING\n\
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED\n\
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY\n\
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER\n\
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE\n\
POSSIBILITY OF SUCH DAMAGES.\n').


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
