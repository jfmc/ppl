m4_divert(-1)
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_prolog_dat.m4')dnl

dnl m4_separator(String)
dnl
dnl String is returned unless if it is empty, then use `,'
m4_define(`m4_separator', `m4_ifelse($1, `', `,', `$1')')

dnl m4_pattern_delimiter(String)
dnl
dnl For the system files we use the macro names
dnl where the patterns have no delimiters
dnl FIXME The `@' is temporary.
dnl This needs to be replaced by the empty string `'
dnl when the dat.m4 and code.m4 files are changed.
m4_define(`m4_pattern_delimiter', `@')

dnl m4_term_sequence(Number of Iterations, String to be Repeated)
dnl
dnl generates a sequence of strings with the option of
dnl identifying each string uniquely and the option of
dnl changing the separator from the default `,'.
m4_define(`m4_term_sequence',
  `m4_ifelse(`$1', 0, ,
     `$2(1)`'m4_ifelse(`$1', 1, ,
       `m4_forloop(`i', 2, `$1', `m4_separator(`$3') $2(i)')')')')

dnl m4_library_names_to_code(Class_Kind,
dnl                            Procedure_Name1, Procedure_Name2, ...)
dnl
dnl Each name from the second argument onwards is replaced
dnl with the code and then the schema patterns in the code
dnl are replaced by the various instances.
m4_define(`m4_library_names_to_code', `dnl
m4_ifelse($#, 0, ,$#, 1, ,$#, 2, m4_get_schematic_code($2, $1),
       `dnl
m4_get_schematic_code($2, $1)`'dnl
m4_library_names_to_code(m4_incr($1), m4_shift(m4_shift($@)))`'dnl
')`'dnl
')

dnl ppl_prolog_sys_code
dnl
dnl For each recognised class in the "classes" list,
dnl takes main predicate input list and sends one line at a time to
dnl a macro that adds extensions for the result of
dnl a macro that sets the class and the schema(s).
m4_define(`ppl_prolog_sys_code', `dnl
m4_library_names_to_code(0, m4_library_predicate_list)`'dnl
m4_all_classes_code`'dnl
')
m4_divert`'dnl
