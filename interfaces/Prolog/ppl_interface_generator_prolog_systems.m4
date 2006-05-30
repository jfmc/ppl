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

# m4_extend_procedure_names(String)
#
# adds the system specific extensions to the predicates that
# are dependent on the class.
define(`m4_extend_procedure_names',
       `patsubst(`$1', `\(.*\)
',
  `ifelse((index(\1, CLASS) + index(\1, class)) == -2, 1, ,
     `m4_set_schema_strings(m4_add_extension(\1!),
        m4_string_substitution_list)')')')

# ppl_prolog_sys_code
#
# For each recognised class in the "classes" list,
# takes main predicate input list and sends one line at a time to
# a macro that adds extensions for the result of                                # a macro that sets the class and the schema(s).
define(`ppl_prolog_sys_code',
  `dnl
m4_extend_procedure_names(library_predicate_list)dnl
m4_forloop(`ind', 1, m4_num_possible_classes,
    `dnl
define(`class', Class`'ind)dnl
ifelse(index(m4_classes, class), -1, ,
`m4_extend_procedure_names(m4_set_class(m4_filter(class_predicate_list)))')')')dnl
divert`'dnl
