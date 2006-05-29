include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl
divert(-1)

# ppl_prolog_sys_code
#
# For each recognised class in the "classes" list,
# takes main predicate input list and sends one line at a time to
# a macro that adds extensions for the result of
# a macro that sets the class and the schema(s).
define(`ppl_prolog_sys_code',
  `dnl
extend_predicates(library_predicate_list)dnl
forloop(`ind', 1, num_possible_classes,
    `dnl
define(`class', Class`'ind)dnl
ifelse(index(classes, class), -1, ,
`extend_predicates(set_class(filter(class_predicate_list)))')')')dnl
divert`'dnl
