include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl
divert(-1)

# m4_separator(String)
#
# String is returned unless if it is empty, then use `,'
define(`m4_separator', `ifelse($1, `', `,', `$1')')

# m4_term_sequence(Number of Iterations, String to be Repeated)
#
# generates a sequence of strings with the option of
# identifying each string uniquely and the option of
# changing the separator from the default `,'.
define(`m4_term_sequence',
  `ifelse(`$1', 0, ,
     `$2(1)`'ifelse(`$1', 1, ,
       `m4_forloop(`i', 2, `$1', `m4_separator(`$3') $2(i)')')')')

# m4_extension(String)
#
# the extensions (optionally prefix, postfix and infix text) are added.
# the extension can be controlled by the extra tokens at the end of the main
# text.
# The arity can be reset to n for a specific class by the flag class/n
# the `nofail/' flag indicates that the predicate(s) will always succeed.
define(`m4_extension', `extension($1, $2, $3)')

# ppl_prolog_sys_code
#
# For each recognised class in the "classes" list,
# takes main predicate input list and sends one line at a time to
# a macro that adds extensions for the result of                                # a macro that sets the class and the schema(s).
define(`ppl_prolog_sys_code',
  `m4_procedure_names_to_code(m4_library_predicate_list)dnl
m4_all_classes_code')
divert`'dnl
