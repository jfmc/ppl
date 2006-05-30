ifelse(
        This file is to generate ppl_gprolog.pl in the
        interfaces/Prolog/GNU directory.
      )
/* GNU Prolog interface: GNU Prolog part.
include(`ppl_interface_generator_copyright')
*/

include(`ppl_interface_generator_prolog_systems.m4')dnl
define(`tm', ``+term'')dnl
define(`start', 0)
define(`extension',
  `:- foreign($1`'ifelse(`$2', 0, ,`(m4_term_sequence($2, `tm'))')).
')dnl
ppl_prolog_sys_code`'dnl
