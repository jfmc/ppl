m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_gprolog.pl.
/* GNU Prolog interface: GNU Prolog part.
m4_include(`ppl_interface_generator_copyright')
*/

m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
m4_define(`tm', ``+term'')dnl
m4_define(`start', 0)
m4_define(`m4_extension',
  `:- foreign($1`'m4_ifelse(`$2', 0, ,`(m4_term_sequence($2, `tm'))')).
')dnl
ppl_prolog_sys_code`'dnl
