dnl This file generates ppl_xsb.H.
/* XSB Prolog interface: declarations.  -*- Prolog -*-
include(`ppl_interface_generator_copyright')

*/

:- export`'dnl
divert(1)

:- ldoption('-L../../../src/.libs -lppl -L../../../Watchdog/.libs -lpwl -lgmp -lgmpxx').

divert`'dnl
include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`m4_extension', `ifelse($4, 0, `$1/$2', `COMMA
$1/$2')')dnl
patsubst(ppl_prolog_sys_code, COMMA, `,')`.'dnl

