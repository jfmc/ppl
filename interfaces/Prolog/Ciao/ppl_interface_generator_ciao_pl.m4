ifelse(
        This file is to generate ppl_ciao.pl in the
        interfaces/Prolog/Ciao directory.
      )
/* Ciao Prolog interface: Ciao Prolog part.
include(`ppl_interface_generator_copyright')
*/

:- module(ppl_ciao,
[
divert(1)
],
[
        assertions,
        basicmodes,
        regtypes,
        foreign_interface
]).

divert(2)dnl
:- extra_linker_opts('-L.libs').
:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
divert(3)
]).

:- comment(version_maintenance,off).

divert`'dnl
include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`start', 0)
define(`extension', `ifelse(start, 0, , `COMMA
')	  $1/$2`'undefine(`start')')dnl
patsubst(ppl_prolog_sys_code, COMMA, `,')`'dnl
undivert(1)`'dnl
divert`'dnl
define(`check_fail', ifelse(index($1, nofail), -1, failok, nofail))
define(`term', `Term$1')dnl
define(`anyterm', `any_term')dnl
define(`interm', `in(Term$1)')dnl
define(`extension', `ifelse(check_fail(`$3'), nofail,
  `:- true pred $1`'ifelse(`$2', 0, ,(`'term_sequence($2, `interm'))
          :: `'term_sequence($2, `anyterm', ` *')) +  foreign.
',
  `:- true pred $1_2(`'term_sequence($2, `interm')`'ifelse(`$2', 0, `go(Success)', `, go(Success)'))
          :: `'term_sequence($2, `anyterm', ` *')`'ifelse(`$2', 0, ` int', ` * int')
  + (returns(Success), foreign($1)).

$1`'ifelse(`$2', 0, ,`(term_sequence($2, `term'))') :-
   $1_2(`'term_sequence($2, `term')`'ifelse(`$2', 0, `1', `, 1')).

')')dnl
ppl_prolog_sys_code`'dnl
undivert(2)dnl
divert`'dnl
define(`start', 0)dnl
define(`extension',
  `ifelse(check_fail(`$3'), nofail, ,ifelse(start, 0, , `COMMA
')	`'$1_2/incr($2)`'undefine(`start'))')dnl
patsubst(ppl_prolog_sys_code, COMMA, `,')`'dnl
