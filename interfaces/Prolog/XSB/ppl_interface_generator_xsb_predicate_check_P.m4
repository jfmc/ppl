dnl This file generates ppl_ciao_predicate_check.pl
/* XSB Prolog interface: XSB Prolog part for checking all predicates.
include(`ppl_interface_generator_copyright')
*/

:- compiler_options([xpp_on]).
:- import xpp_include_dir/1 from parse.
:- assert(xpp_include_dir('.')).

:- import append/3, length/2, member/2 from basics.
:- import
divert(1)
   from ppl_xsb.

:- [ppl_xsb].

#include "pl_check.pl"

prolog_system('XSB').

main :-
    (check_all ->
        write('OK')
    ;
        write('FAILURE')
    ),
    nl.

:- main.
divert`'dnl
include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`m4_extension', `ifelse($4, 0, , `COMMA
')	  $1/$2')dnl
patsubst(ppl_prolog_sys_code, COMMA, `,')`'dnl
undivert(1)`'dnl
