m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_xsb.H.
/* XSB Prolog interface: declarations.  -*- Prolog -*-
m4_include(`ppl_interface_generator_copyright')

*/

:- export`'dnl
m4_divert(1)

:- ldoption('-L../../../src/.libs -lppl -L../../../Watchdog/.libs -lpwl -lgmp -lgmpxx').

m4_divert`'dnl
m4_include(`ppl_interface_generator_prolog_systems.m4')`'dnl
m4_define(`m4_extension', `m4_ifelse($4, 0, `$1/$2', `COMMA
$1/$2')')`'dnl
m4_patsubst(ppl_prolog_sys_code, COMMA, `,')`.'dnl

