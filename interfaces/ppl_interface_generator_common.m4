divert(-1)

dnl m4_forloop(Iteration Variable, Starting Value, Finish Value,
dnl            Text to be Expanded)
dnl
dnl Code copied from m4 documentation.
define(`m4_forloop',
  `pushdef(`$1', `$2')_m4_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_m4_forloop',
  `$4`'ifelse($1, `$3', ,
    `define(`$1', incr($1))_m4_forloop(`$1', `$2', `$3', `$4')')')

dnl m4_upcase(String), m4_downcase(String)
dnl
dnl Code copied from m4 documentation.
define(`m4_upcase', `translit(`$*', `a-z', `A-Z')')
define(`m4_downcase', `translit(`$*', `A-Z', `a-z')')

dnl m4_capfirstletter(String)
dnl
dnl Capitalises first letter of words that can have letters and underscores.
dnl Thus "xyz_abc" will become "Xyz_abc".
define(`m4_capfirstletter',
  `regexp(`$1', `^\(\w\)\([\w_]*\)',
     m4_upcase(`\1')`\2')')

dnl m4_capfirstletters(String)
dnl
dnl Capitalises first letter of subwords that can have letters only.
dnl Thus "xyz_abc" will become "Xyz_Abc".
define(`m4_capfirstletters',
  `ifelse(index(`$1', `_'), -1,
     `regexp(`$1', `^\(\w\)\(\w*\)',
       `m4_upcase(`\1')`\2'')',
         regexp(`$1', `^\(\w\)\(\w*_\)\(\w\)\(\w*\)',
           `m4_upcase(`\1')`\2'm4_upcase(`\3')`\4''))')

dnl m4_ifndef(Macro, Default Definition)
dnl
dnl If Macro is defined, use that definition, otherwise use the
dnl Default Definition.
define(`m4_ifndef', `ifdef(`$1', $1, $2)')

dnl m4_replace_pattern(Pattern, String)
dnl
dnl Replaces in String occurrences of the capitalised form of Pattern
dnl by the required actual string (determined both by the class
dnl and Pattern). There are additional codes to help provide the
dnl appropriate pattern for the replacmement.
dnl - alt_ means that the alternative string must be used if one exists.
dnl - U means that the alt_actual string must be capitalised at start
dnl   of word and after "_".
define(`m4_replace_pattern',
  `define(`PATTERN', `m4_upcase($1)')dnl
ifelse(index(`$2', PATTERN), `-1', `$2',
  `define(`num_strings',
     m4_ifndef(num_`'m4_class`'_`'$1`'s, m4_ifndef(num_`'$1`'s, 0)))dnl
ifelse(num_strings, 0, ,
  `m4_forloop(`m4_i', 1, num_strings, `dnl
define(`replacement',
  m4_ifndef(m4_class`'_`'$1`'m4_i, m4_ifndef($1`'m4_i, `')))dnl
define(`alt_replacement',
  m4_ifndef(alt_`'m4_class`'_`'$1`'m4_i, replacement))dnl
define(`Replacement',
  m4_capfirstletters(replacement))dnl
define(`Alt_Replacement',
  m4_capfirstletters(alt_replacement))dnl
patsubst(patsubst(patsubst(patsubst(`$2',
           4U`'PATTERN`'4, Replacement),
           4UALT_`'PATTERN`'4, Alt_Replacement),
           4ALT_`'PATTERN`'4, alt_replacement),
           4`'PATTERN`'4, replacement)')')')')

dnl m4_replace_all_patterns(String, Sequence of Strings)
dnl
dnl A (recursive) macro to set the schemas in the string in the first
dnl argument. The sequence of schemas are in arguments 2 to end.
define(`m4_replace_all_patterns', `dnl
ifelse($2, `', ``$1'',
       `m4_replace_all_patterns(m4_replace_pattern($2, $1),
                              shift(shift($@)))')dnl
')

dnl m4_set_class(String)
dnl
dnl replaces dummy string `4CLASS4' by the actual class defined
dnl in m4_class.
define(`m4_set_class',
  `patsubst(`patsubst(`$1',  `4CLASS4', m4_class)',
     4lCLASS4, m4_downcase(m4_class))')

dnl m4_replace_with_code(String)
dnl
dnl Procedure name schemas are replaced by the code schema.
define(`m4_replace_with_code',
  `patsubst(`$1',
     `[ ]*\(ppl_[^ /]+\)/*\([0-9]*\)[ ]*\([a-z]*\)[^\n]*!',
          `m4_extension(\1, \2, \3)')')

dnl m4_extension(Procedure Name)
dnl
dnl Adds "_code" to Procedure Name so that it
dnl matches the code macro definition name.
dnl
dnl This has to be redefined in the Prolog system files
dnl for the specific extensions needed for the code there.
define(`m4_extension', `m4_ifndef($1`'_code, `')')

dnl m4_procedure_names_to_code(Procedure_Schema_List)
dnl
dnl Each name_schema in Procedure_Schema_List, is replaced
dnl with the code and then the schema patterns in the code
dnl are replaced by the various instances.
define(`m4_procedure_names_to_code', `dnl
patsubst(`$1', `\(.*\)
',
         `m4_replace_all_patterns(m4_replace_with_code(\1!),
                                m4_pattern_substitution_list)')dnl
')

dnl m4_filter(Procedure_Schema_List)
dnl
dnl Keeps just those procedures that are needed for the given class.
dnl There are several codes for keeping or eliminating a schema name
dnl and the tests here correspond to these.
define(`m4_filter',
  `patsubst(`$1', `\(.*
\)',
    `ifelse(index(\1, X`'m4_class), -1,
       ifelse(index(\1, m4_class), -1,
         ifelse(index(\1, All), -1,
           ifelse(index(\1, m4_class_group), -1,
             ifelse(index(\1, m4_class_super_group), -1, ,
               \1), \1), \1), \1))')')

dnl m4_one_class_code
dnl
dnl Takes main procedure input list and each procedure is checked
dnl to see if there is a macro with "_code" extension that defines the code.
dnl Then a macro sets the class and other schematic components.
define(`m4_one_class_code', `dnl
m4_ifndef(`m4_pre_extra_class_code', `')dnl
m4_set_class(m4_procedure_names_to_code(m4_filter(m4_procedure_list)))dnl
m4_ifndef(`m4_post_extra_class_code', `')dnl
')

dnl m4_all_classes_loop(counter)
dnl
dnl This iterates through the classes to generate the code.
dnl All the required classes are defined by m4_interface_class`'counter.
dnl The m4_class = m4_interface_class`'counter
dnl                ie the current class interface name;
dnl the m4_class_num = counter;
dnl the m4_cpp_class = the complete cplusplus class name;
dnl the m4_class_kind = the first component of the cplusplus class name;
dnl Once all the class names etc have been defined, the actual
dnl code is generated by the macro m4_one_class_code.
dnl
dnl For instance, given that the 2nd class is BD_Shape<int8_t>
dnl with interface name BD_Shape_int8_t, before calling the macro
dnl m4_one_class_code,
dnl m4_class = BD_Shape_int8_t;
dnl m4_class_num = 2;
dnl m4_cpp_class = BD_Shape<int8_t>;
dnl m4_class_kind = BD_Shape;
define(`m4_all_classes_loop',
  `ifdef(m4_interface_class`'$1,
    `define(`m4_class', m4_interface_class`'$1)dnl
define(`m4_class_num', $1)dnl
define(`m4_cpp_class', m4_cplusplus_class`'$1)dnl
define(`m4_class_kind', m4_class`'$1_component`'1)dnl
m4_one_class_code`'dnl
m4_all_classes_loop(incr($1))')')

dnl m4_get_name_components(Class Num, Component_Num, String)
dnl
dnl The components of the class name in String are separated out
dnl by recursively getting the first part before the "<"
dnl removing the outer angle brackets and calling this macro again
dnl with the remaining string.
dnl Each component is defined as
dnl m4_class<Class Num>_component<Component Num>
dnl The total number of components is defined by the macro
dnl m4_num_class<Class Num>_components
dnl
dnl For example:
dnl if there is the interface list of classes
dnl Polyhedron, Grid, BD_Shape_int32_t,
dnl                     Polyhedra_Powerset_BD_Shape_signed_char
dnl and the cplusplus list of classes
dnl Polyhedron, Grid, BD_Shape<int32_t>,
dnl                     Polyhedra_Powerset<BD_Shape<signed char> >
dnl then the initialization code would define
dnl m4_interface_class1 as Polyhedron
dnl m4_interface_class2 as Grid
dnl m4_interface_class3 as BD_Shape_int32_t
dnl m4_interface_class4 as Polyhedra_Powerset_BD_Shape_signed_char
dnl m4_cplusplus_class1 as Polyhedron
dnl m4_cplusplus_class2 as Grid
dnl m4_cplusplus_class3 as BD_Shape<int32_t>
dnl m4_cplusplus_class4 as Polyhedra_Powerset<BD_Shape<signed char> >
dnl m4_class1_component1 as Polyhedron
dnl m4_class2_component2 as Grid
dnl m4_class3_component1 as BD_Shape
dnl m4_class3_component2 as int32_t
dnl m4_class4_component1 as Polyhedra_Powerset
dnl m4_class4_component2 as BD_Shape
dnl m4_class4_component3 as signed char
dnl m4_class1_num_components as 1
dnl m4_class2_num_components as 1
dnl m4_class3_num_components as 2
dnl m4_class4_num_components as 3
define(`m4_get_name_components',
  `ifelse($3, `', ,
    `ifelse(index($3, <), -1,
      define(m4_class`'$1_component`'$2, $3)dnl
define(m4_class`'$1_num_components, $2),
        `regexp($3, `\([^ <]+\)[<]\(.*\)[ ]*[>]',
`define(m4_class`'$1_component`'$2, \1)dnl
m4_get_name_components($1, incr($2), \2)')')')')

dnl m4_init_interface_classes(Class_List)
dnl
dnl Parses the comma-separated list of class names Class_List
dnl for the names of the classes used to form the names of procedures
dnl in the user interface.
define(`m4_init_interface_classes', `m4_init_interface_classes_aux(1, $@)')

dnl m4_init_interface_classes_aux(counter, Class_List)
define(`m4_init_interface_classes_aux',
  `ifelse($2, `', ,
    `define(m4_interface_class`'$1, $2)dnl
m4_init_interface_classes_aux(incr($1), shift(shift($@)))')')

dnl m4_init_cplusplus_classes(Class_List)
dnl
dnl Parses the comma-separated list of class names Class_List
dnl to be used in the C++ code implementing the interface procedures.
dnl The components of the class name are also separated out
dnl and defined as m4_class<class_num>_component<component_num>
dnl (see comment and example for m4_get_name_components/3).
define(`m4_init_cplusplus_classes', `m4_init_cplusplus_classes_aux(1, $@)')

dnl m4_init_cplusplus_classes_aux(counter, Class_List)
define(`m4_init_cplusplus_classes_aux',
  `ifelse($2, `', ,
     `define(m4_cplusplus_class`'$1, $2)dnl
m4_get_name_components($1, 1, $2)dnl
m4_init_cplusplus_classes_aux(incr($1), shift(shift($@)))')')

dnl m4_initialize_classes
dnl
dnl Provides the class name macro definitions by calling
dnl m4_init_interface_classes and m4_init_cplusplus_classes.
define(`m4_initialize_classes', `dnl
m4_init_interface_classes(m4_interface_class_names)`'dnl
m4_init_cplusplus_classes(m4_cplusplus_class_names)dnl
')

dnl m4_all_classes_code
dnl
dnl This initializes the macros for the classes requested by the user
dnl (which is determined by the configuration)
dnl and then calls the main loop m4_all_classes_loop for generating
dnl all the required classes.
define(`m4_all_classes_code', `dnl
m4_initialize_classes`'dnl
m4_all_classes_loop(1)dnl
')

divert`'dnl
