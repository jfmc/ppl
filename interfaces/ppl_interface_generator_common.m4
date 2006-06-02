divert(-1)

# m4_forloop(Iteration Variable, Starting Value, Finish Value,
#         Text to be Expanded)
#
# Code copied from m4 documentation.
define(`m4_forloop',
  `pushdef(`$1', `$2')_m4_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_m4_forloop',
  `$4`'ifelse($1, `$3', ,
    `define(`$1', incr($1))_m4_forloop(`$1', `$2', `$3', `$4')')')

# m4_upcase(String), m4_downcase(String)
#
# Code copied from m4 documentation.
define(`m4_upcase', `translit(`$*', `a-z', `A-Z')')
define(`m4_downcase', `translit(`$*', `A-Z', `a-z')')

# m4_capfirstletter(String)
#
# Capitalises first letter of words that can have letters and underscores.
# Thus "xyz_abc" will become "Xyz_abc".
define(`m4_capfirstletter',
  `regexp(`$1', `^\(\w\)\([\w_]*\)',
     m4_upcase(`\1')`\2')')

# m4_capfirstletters(String)
#
# Capitalises first letter of subwords that can have letters only.
# Thus "xyz_abc" will become "Xyz_Abc".
define(`m4_capfirstletters',
  `ifelse(index(`$1', `_'), -1,
     `regexp(`$1', `^\(\w\)\(\w*\)',
       `m4_upcase(`\1')`\2'')',
         regexp(`$1', `^\(\w\)\(\w*_\)\(\w\)\(\w*\)',
           `m4_upcase(`\1')`\2'm4_upcase(`\3')`\4''))')

# m4_ifndef(Macro, Default Definition)
#
# If Macro is defined, use that definition, otherwise use the
# Default Definition.
define(`m4_ifndef', `ifdef(`$1', $1, $2)')

# m4_set_string(Pattern, String)
#
# replaces in String occurrences of the capitalised form of Pattern
# by the required actual string (determined both by the class
# and Pattern). There are additional codes to help provide the
# appropriate pattern for the replacmement.
# - alt_ means that the alternative string must be used if one exists.
# - U means that the alt_actual string must be capitalised at start
#   of word and after "_".
define(`m4_set_string',
  `define(`ustring', `m4_upcase($1)')dnl
ifelse(index(`$2', ustring), `-1', `$2',
  `define(`num_strings',
     m4_ifndef(num_`'m4_class`'_`'$1`'s, m4_ifndef(num_`'$1`'s, 0)))dnl
ifelse(num_strings, 0, ,
  `m4_forloop(`m4_i', 1, num_strings, `dnl
define(`actual_string',
  m4_ifndef(m4_class`'_`'$1`'m4_i, m4_ifndef($1`'m4_i, `')))dnl
define(`alt_actual_string',
  m4_ifndef(alt_`'m4_class`'_`'$1`'m4_i, actual_string))dnl
define(`Uactual_string',
  m4_capfirstletters(actual_string))dnl
define(`Ualt_actual_string',
  m4_capfirstletters(alt_actual_string))dnl
patsubst(patsubst(patsubst(patsubst(`$2',
           U`'ustring, Uactual_string),
           UALT_`'ustring, Ualt_actual_string),
           ALT_`'ustring, alt_actual_string),
           ustring, actual_string)')')')')

# m4_set_schema_strings(String, Sequence of Strings)
#
# A (recursive) macro to set the schemas in the string in the first
# argument. The sequence of schemas are in arguments 2 to end.
define(`m4_set_schema_strings', `ifelse($2, `', ``$1'',
  `m4_set_schema_strings(m4_set_string($2, $1), shift(shift($@)))')'))

# m4_set_class(String)
#
# replaces dummy string `M4_CLASS' by the actual class defined
# in m4_class.
define(`m4_set_class',
  `patsubst(`patsubst(`$1',  `M4_CLASS', m4_class)',
     M4_lCLASS, m4_downcase(m4_class))')

# m4_replace_with_code(String)
#
# procedure name schemas are replaced by the code schema
define(`m4_replace_with_code',
  `patsubst(`$1',
     `[ ]*\(ppl_[^ /]+\)/*\([0-9]*\)[ ]*\([a-z]*\)[^\n]*!',
          `m4_extension(\1, \2, \3)')')

# m4_extension(Procedure Name)
#
# Adds "_code" to Procedure Name so that it
# matches the code macro definition name.
#
# This has to be redefined in the Prolog system files
# for the specific extensions needed for the code there.
define(`m4_extension', `m4_ifndef($1`'_code, `')')

# m4_procedure_names_to_code(Procedure_Schema_List)
#
# Each name_schema in Procedure_Schema_List, is replaced
# with the code and then the schema patterns in the code
# are replaced by the various instances.
define(`m4_procedure_names_to_code',
  `patsubst(`$1', `\(.*\)
',
       `m4_set_schema_strings(m4_replace_with_code(\1!),
         m4_string_substitution_list)')')

# m4_filter(Procedure_Schema_List)
#
# keeps just those procedures that are needed for the given class.
# There are several codes for keeping or eliminating a schema name
# and the tests here correspond to these.
define(`m4_filter',
  `patsubst(`$1', `\(.*
\)',
    `ifelse(index(\1, X`'m4_short_class_name), -1,
       ifelse(index(\1, m4_short_class_name), -1,
         ifelse(index(\1, All), -1,
           ifelse(index(\1, m4_class_group), -1,
             ifelse(index(\1, m4_class_super_group), -1, ,
               \1), \1), \1), \1))')')

# m4_one_class_code
#
# takes main procedure input list and each procedure is checked
# to see if there is a macro with "_code" extension that defines the code.
# Then a macro sets the class and other schematic components.
define(`m4_one_class_code',
  `m4_ifndef(`m4_pre_extra_class_code', `')dnl
m4_set_class(m4_procedure_names_to_code(m4_filter(m4_procedure_list)))dnl
m4_ifndef(`m4_post_extra_class_code', `')')

# m4_short_class_name(String)
#
# The initial two letters of the class name - use to identify a class.
define(`m4_short_class_name', `substr(m4_class, 0, 4)')

# m4_all_classes_loop(Count)
#
# This iterates through the classes to generate the code.
# All the required classes are defined by m4_interface_class`'count
# and this is defined to be m4_class before proceding with
# generating the code.
define(`m4_all_classes_loop',
  `ifdef(m4_interface_class`'$1,
    `define(`m4_class', m4_interface_class`'$1)dnl
m4_one_class_code`'dnl
m4_all_classes_loop(incr($1))')')

# m4_init_interface_classes(Class_List, Count)
#
# parses the space-separated list of class names Class_List
# for the names of the classes used to form the names of procedures
# in the user interface.
define(`m4_init_interface_classes',
  `ifelse($1, `', ,
    `regexp($1, `\([^ ]+\) ?\(.*\)',
       `define(m4_interface_class`'$2, \1)dnl
m4_init_interface_classes(\2, incr($2))')')')

# m4_init_cplusplus_classes(Class_List, count)
#
# parses the space separated list of class names Class_List
# to be used in the C++ code implementing the interface procedures.
# The class type name (ie the class name without the numeric type)
# and the class numeric type are also defined by macros
# m4_class_type`'count and m4_numeric_type`'count, respectively.
define(`m4_init_cplusplus_classes',
  `ifelse($1, `', ,
    `regexp($1, `\([^ <]+\)\([<]*\)\([^ >]*\)\([>]*\) ?\(.*\)',
       `define(m4_cplusplus_class`'$2, \1\2\3\4)dnl
define(m4_numeric_type`'$2, \3)dnl
define(m4_class_type`'$2, \1)dnl
m4_init_cplusplus_classes(\5, incr($2))')')')

# m4_initialize_classes
#
# provides the class name macro definitions.
# it calls the macros m4_init_interface_classes/2 and
# m4_init_cplusplus_classes/2 with the given class_lists
# and an initial counter = 1.
define(`m4_initialize_classes',
  `m4_init_interface_classes(m4_interface_class_names, 1)`'dnl
m4_init_cplusplus_classes(m4_cplusplus_class_names, 1)')

# m4_all_classes_code
#
# This initializes the macros for the classes requested by the user
# (which is determined by the configuration)
# and then calls the main loop m4_all_classes_loop for generating
# all the required classes.
define(`m4_all_classes_code',
  `m4_initialize_classes`'dnl
m4_all_classes_loop(1)')

divert`'dnl
