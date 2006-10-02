m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_ciao_predicate_check.pl
/* Ciao Prolog interface: Ciao Prolog part for checking all predicates.
m4_include(`ppl_interface_generator_copyright')
*/

:- module(_, [main/0], []).
:- use_module(library(debugger)).
:- use_module(library(dynamic)).
:- use_module(library(lists)).
:- use_module(library(prolog_sys)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(ppl_ciao,
[
m4_divert(1)
]).

:- set_prolog_flag(multi_arity_warnings, off).

:- `include'('pl_check.pl').

prolog_system('Ciao').

main:-
   (check_all ->
        write('OK')
   ;
        write('FAILURE')
   ),
   nl.

m4_divert`'dnl
m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
m4_define(`m4_extension', `m4_ifelse($4, 0, , `COMMA
')  $1/$2')dnl
m4_patsubst(ppl_prolog_sys_code, COMMA, `,')`'dnl
m4_undivert(1)`'dnl
