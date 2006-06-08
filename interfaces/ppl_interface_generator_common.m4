divert(-1)

dnl =====================================================================
dnl ====== The following are application independent helper macros ======
dnl =====================================================================

dnl m4_forloop(Iteration_Var, From_Value, To_Value, Loop_Body)
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
dnl Examples: m4_upcase(`abCd')   ==> ABCD
dnl           m4_downcase(`abCd') ==> abcd
define(`m4_upcase', `translit(`$*', `a-z', `A-Z')')
define(`m4_downcase', `translit(`$*', `A-Z', `a-z')')

dnl m4_capfirstletter(String)
dnl
dnl Capitalises first letter of words that can have letters and underscores.
dnl Example: m4_capfirstletter(`xyz_abc') ==> Xyz_abc
define(`m4_capfirstletter',
  `regexp(`$1', `^\(\w\)\([\w_]*\)',
     m4_upcase(`\1')`\2')')

dnl m4_capfirstletters(String)
dnl
dnl Capitalises first letter of subwords that can have letters only.
dnl Example: m4_capfirstletter(`xyz_abc') ==> Xyz_Abc
define(`m4_capfirstletters',
  `ifelse(index(`$1', `_'), -1,
     `regexp(`$1', `^\(\w\)\(\w*\)',
       `m4_upcase(`\1')`\2'')',
         regexp(`$1', `^\(\w\)\(\w*_\)\(\w\)\(\w*\)',
           `m4_upcase(`\1')`\2'm4_upcase(`\3')`\4''))')

dnl m4_ifndef(Macro, Default Definition)
dnl
dnl If Macro is defined, use that definition;
dnl otherwise use the Default Definition.
define(`m4_ifndef', `ifdef(`$1', $1, $2)')


dnl =====================================================================
dnl ====== The following are application dependent macros: their meaning
dnl ====== is influenced by the overall interface generator architecture.
dnl =====================================================================

dnl The pattern delimiter.
define(`m4_pattern_delimiter', 4)

dnl m4_replace_pattern(Class_Kind, String, Pattern)
dnl
dnl Replaces in String occurrences of the capitalised form of Pattern
dnl by the required actual string (determined both by the Class_Kind
dnl and Pattern). There are additional codes to help provide the
dnl appropriate pattern for the replacmement.
dnl - alt_ means that the alternative string must be used if one exists.
dnl - U means that the alt_actual string must be capitalised at start
dnl   of word and after "_".
define(`m4_replace_pattern', `dnl
define(`PATTERN', m4_upcase($3))dnl
ifelse(index(`$2', PATTERN), `-1', `$2', `dnl
define(`num_strings',
       `m4_ifndef(num_`'$1`'_`'$3`'s,
                  m4_ifndef(num_`'$3`'s, 0))')dnl
ifelse(num_strings, 0, ,
  `m4_forloop(`m4_i', 1, num_strings, `dnl
define(`replacement',
       m4_ifndef($1`'_`'$3`'m4_i, m4_ifndef($3`'m4_i, `')))dnl
define(`alt_replacement',
       m4_ifndef(alt_`'$1`'_`'$3`'m4_i, replacement))dnl
define(`Replacement',
       m4_capfirstletters(replacement))dnl
define(`Alt_Replacement',
       m4_capfirstletters(alt_replacement))dnl
patsubst(patsubst(patsubst(patsubst(`$2',
           m4_pattern_delimiter`'U`'PATTERN`'m4_pattern_delimiter,
             Replacement),
           m4_pattern_delimiter`'UALT_`'PATTERN`'m4_pattern_delimiter,
             Alt_Replacement),
           m4_pattern_delimiter`'ALT_`'PATTERN`'m4_pattern_delimiter,
             alt_replacement),
           m4_pattern_delimiter`'PATTERN`'m4_pattern_delimiter,
             replacement)dnl
undefine(`Alt_Replacement')dnl
undefine(`Replacement')dnl
undefine(`alt_replacement')dnl
undefine(`replacement')dnl
')')dnl
undefine(`num_strings')dnl
')dnl
undefine(`PATTERN')dnl
')

dnl m4_replace_all_patterns(Class_Kind, String, Pattern1, Pattern2, ...)
dnl
dnl A (recursive) macro to replace, inside the second argument String,
dnl all of the patterns listed from the third argument onwards.
define(`m4_replace_all_patterns', `dnl
ifelse($3, `', ``$2'',
       `m4_replace_all_patterns($1,
                                m4_replace_pattern($1, $2, $3),
                                shift(shift(shift($@))))')dnl
')

dnl m4_replace_class_patterns(Class, CPP_Class, String)
dnl
dnl String - the code for all the methods for the current class.
dnl
dnl The macro replaces in String the different patterns for
dnl the class kind, class interface name and class C++ name.
define(`m4_replace_class_patterns', `dnl
patsubst(`patsubst(`$3',
  m4_pattern_delimiter`'CLASS`'m4_pattern_delimiter, $1)',
  m4_pattern_delimiter`'CPP_CLASS`'m4_pattern_delimiter, $2)dnl
')

dnl m4_get_code_schema(Procedure_Name)
dnl
dnl Procedure name schemas are replaced by the code schema.
define(`m4_get_code_schema',
  `patsubst(`$1',
     `[ ]*\(ppl_[^ /]+\)\(/\([0-9]+\)\)?[ ]*\([a-z]*\).*',
     `m4_extension(\1, \3, \4)')')

dnl m4_extension(Procedure Name)
dnl
dnl Adds "_code" to Procedure Name so that it
dnl matches the code macro definition name.
dnl
dnl This has to be redefined in the Prolog system files
dnl for the specific extensions needed for the code there.
define(`m4_extension', `m4_ifndef($1`'_code, `')')


dnl m4_procedure_names_to_code(Class_Kind,
dnl                            Procedure_Name1, Procedure_Name2, ...)
dnl
dnl Each name from the second argument onwards is replaced
dnl with the code and then the schema patterns in the code
dnl are replaced by the various instances.
define(`m4_procedure_names_to_code', `dnl
ifelse($#, 0, , $#, 1, ,
       $#, 2, `m4_procedure_name_to_code($1, $2)',
       `dnl
m4_procedure_name_to_code($1, $2)dnl
m4_procedure_names_to_code($1, shift(shift($@)))dnl
')dnl
')

dnl m4_procedure_name_to_code(Class_Kind, Procedure_Name)
dnl
dnl The procedure specification is replaced with the code.
define(`m4_procedure_name_to_code', `dnl
patsubst(`$2', `\(.*\)',
         `m4_replace_all_patterns($1,
                                  m4_get_code_schema(\1),
                                  m4_pattern_list)')dnl
')

dnl m4_filter_all(Class_Kind, Procedure_Name1, ProcedureName2, ...)
dnl
dnl Keeps just those procedure names that are needed for the given class kind.
dnl There are several codes for keeping or eliminating a schema name
dnl and the tests here correspond to these.
define(`m4_filter_all', `dnl
ifelse($2, `', `', `dnl
ifelse(m4_filter($1, $2), `', `', `$2, ')dnl
m4_filter_all($1, shift(shift($@)))dnl
')dnl
')

dnl m4_filter(Class_Kind, Procedure_Name)
dnl
dnl Keeps just those procedure names that are needed for the given class kind.
dnl There are several codes for keeping or eliminating a schema name
dnl and the tests here correspond to these.
define(`m4_filter', `dnl
ifelse(index($2, X`'$1), -1,
  ifelse(index($2, $1), -1,
    ifelse(index($2, All), -1,
      ifelse(index($2, m4_class_group($1)), -1,
        ifelse(index($2, m4_class_super_group($1)), -1, `',
          $2), $2), $2), $2))')dnl
')

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Default (empty) definitions for pre- and post- code for each class.
define(`m4_pre_extra_class_code', `')dnl
define(`m4_post_extra_class_code', `')dnl

dnl m4_one_class_code(Class, CPP_Class, Class_Kind)
dnl
dnl Takes main procedure input list and each procedure is checked
dnl to see if there is a macro with "_code" extension that defines the code.
dnl Then a macro sets the class and other schematic components.
define(`m4_one_class_code', `dnl
m4_pre_extra_class_code($1, $2, $3)dnl
define(`m4_filtered_proc_list',
       `m4_filter_all($3, m4_procedure_list)')dnl
define(`m4_procedure_code',
       `m4_procedure_names_to_code($3, m4_filtered_proc_list)')dnl
m4_replace_class_patterns($1, $2, m4_procedure_code)dnl
undefine(`m4_procedure_code')dnl
undefine(`m4_filtered_proc_list')dnl
m4_post_extra_class_code($1, $2, $3)dnl
')

dnl m4_all_classes_loop(counter)
dnl
dnl This iterates through the classes to generate the code.
dnl All the required classes are defined by m4_interface_class`'counter.
dnl The m4_class      =  m4_interface_class`'counter
dnl                      ie the current class interface name;
dnl the m4_cpp_class  = the complete cplusplus class name;
dnl the m4_class_kind = the first component of the cplusplus class name;
dnl Once all the class names etc have been defined, the actual
dnl code is generated by the macro m4_one_class_code.
dnl
dnl For instance, given that the 2nd class is BD_Shape<int8_t>
dnl with interface name BD_Shape_int8_t, before calling the macro
dnl m4_one_class_code, we have:
dnl   m4_class      ==> BD_Shape_int8_t;
dnl   m4_cpp_class  ==> BD_Shape<int8_t>;
dnl   m4_class_kind ==> BD_Shape;
define(`m4_all_classes_loop', `dnl
ifdef(m4_interface_class`'$1, `dnl
dnl Provide meaningful names to actual arguments of m4_one_class_code.
define(`m4_class', m4_interface_class$1)dnl
define(`m4_cpp_class', m4_cplusplus_class$1)dnl
define(`m4_class_kind', m4_class$1_component1)dnl
dnl
m4_one_class_code(m4_class, m4_cpp_class, m4_class_kind)dnl
dnl
undefine(`m4_class')dnl
undefine(`m4_cpp_class')dnl
undefine(`m4_class_kind')dnl
m4_all_classes_loop(incr($1))')dnl
')

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
define(`m4_init_interface_classes', `m4_init_interface_classes_aux(1, $1)')

dnl m4_init_interface_classes_aux(counter, Class_List)
dnl
dnl counter    - is the index to the first class in Class_List
dnl Class_List - is the tail part of the input list of interface
dnl              class names.
dnl The macro also defines m4_num_classes to be the number of classes
dnl in the full list (ie counter + number in the current list - 1).
dnl The macro calls itself recursively to process the complete list.
define(`m4_init_interface_classes_aux',
  `ifelse($2, `',  `define(m4_num_classes, decr($1))',
    `regexp($2, `\([^@]+\)@?\(.*\)',
      `define(m4_interface_class`'$1, \1)dnl
m4_init_interface_classes_aux(incr($1), \2)')')')

dnl m4_init_cplusplus_classes(Class_List)
dnl
dnl Parses the comma-separated list of class names Class_List
dnl to be used in the C++ code implementing the interface procedures.
dnl The components of the class name are also separated out
dnl and defined as m4_class<class_num>_component<component_num>
dnl (see comment and example for m4_get_name_components/3).
define(`m4_init_cplusplus_classes', `m4_init_cplusplus_classes_aux(1, $1)')

dnl m4_init_cplusplus_classes_aux(counter, Class_List)
dnl
dnl counter    - is the index to the first class in Class_List
dnl Class_List - is the tail part of the input list of cplusplus
dnl              class names.
dnl
dnl The macro also calls the macro m4_get_name_components/3
dnl to extract the details of the components from the top
dnl cplusplus name in the list.
dnl The macro calls itself recursively to process the complete list.
define(`m4_init_cplusplus_classes_aux',
  `ifelse($2, `', ,
    `regexp($2, `\([^@]+\)@?\(.*\)',
       `define(m4_cplusplus_class`'$1, \1)dnl
m4_get_name_components($1, 1, \1)dnl
m4_init_cplusplus_classes_aux(incr($1), \2)')')')

dnl m4_all_classes_code
dnl
dnl This initializes the macros for the classes requested by the user
dnl (which is determined by the configuration)
dnl and then calls the main loop m4_all_classes_loop for generating
dnl all the required classes.
define(`m4_all_classes_code', `dnl
dnl Provides the class name macro definitions by calling
m4_init_interface_classes(m4_interface_classes_names)dnl
m4_init_cplusplus_classes(m4_cplusplus_classes_names)dnl
m4_all_classes_loop(1)dnl
')

divert`'dnl
