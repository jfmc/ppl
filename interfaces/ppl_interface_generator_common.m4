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
define(`m4_pattern_delimiter', `@')

dnl m4_replace_one_pattern_once(Class_Kind, String, Pattern, Replacement)
dnl
dnl The delimted PATTERN is replaced by Replacement in the String.
dnl There are additional codes to help provide the right form of
dnl the replacmement.
dnl - alt_ means that the alternative string must be used if one exists.
dnl - U means that the alt_actual string must be capitalised at start
dnl   of word and after "_".
define(`m4_replace_one_pattern_once', `dnl
patsubst(patsubst(patsubst(patsubst($2,
           m4_pattern_delimiter`'U`'PATTERN`'m4_pattern_delimiter,
             m4_capfirstletters($4)),
           m4_pattern_delimiter`'UALT_`'PATTERN`'m4_pattern_delimiter,
             m4_capfirstletters(m4_ifndef(`m4_$1_$3_$4_alt_replacement', $4))),
           m4_pattern_delimiter`'ALT_`'PATTERN`'m4_pattern_delimiter,
             m4_ifndef(`m4_$1_$3_$4_alt_replacement', $4)),
           m4_pattern_delimiter`'PATTERN`'m4_pattern_delimiter,
             $4)dnl
')

dnl m4_replace_one_pattern_aux(
dnl          Class_Kind, String, Replacement1, Replacement2, ...)
dnl
dnl This iteratively calls m4_replace_one_pattern_once/3 to replace
dnl a delimited form of PATTERN by Replacement`'i.
define(`m4_replace_one_pattern_aux', `dnl
ifelse($#, 0, ,$#, 1, , $#, 2, , $#, 3, ,
$#, 4, m4_replace_one_pattern_once($1, $2, $3, $4),
  `dnl
m4_replace_one_pattern_once($1, $2, $3, $4)dnl
m4_replace_one_pattern_aux($1, $2, $3, shift(shift(shift(shift($@)))))')dnl
')

dnl m4_replace_one_pattern_aux1
dnl
dnl This extra definition is used to expand the default replacement
dnl string m4_<pattern>_replacement.
define(`m4_replace_one_pattern_aux1', m4_replace_one_pattern_aux(`$*'))

dnl m4_replace_one_pattern(Class_Kind, String, Pattern)
dnl
dnl Replaces in String occurrences of the capitalised form of Pattern
dnl by the required actual string (determined both by the Class_Kind
dnl and Pattern).
define(`m4_replace_one_pattern', `dnl
define(`PATTERN', m4_upcase($3))dnl
ifelse(index(`$2', PATTERN), `-1', $2, `dnl
m4_replace_one_pattern_aux1($1, $2, $3,
  m4_ifndef(`m4_$1_$3_replacement', ``m4_$3_replacement''))dnl
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
                                m4_replace_one_pattern($1, $2, $3),
                                shift(shift(shift($@))))')dnl
')

dnl m4_replace_class_patterns(Class_Counter, String)
dnl
dnl String - the code for all the methods for the current class.
dnl
dnl The macro replaces in String the different patterns for
dnl the class interface name and class C++ names.
define(`m4_replace_class_patterns', `dnl
patsubst(`patsubst(`$2',
  m4_pattern_delimiter`'CLASS`'m4_pattern_delimiter, m4_interface_class$1)',
  m4_pattern_delimiter`'CPP_CLASS`'m4_pattern_delimiter, m4_cplusplus_class$1)dnl
')

define(`m4_get_arity', `regexp(`$1', `/\([0-9]*\)', \1)')
define(`m4_get_attribute', `regexp(`$1', `\*\(nofail\)', \1)')

dnl m4_get_code_schema(Procedure_Name)
dnl
dnl Procedure name schemas are replaced by the code schema.
define(`m4_get_code_schema', `dnl
patsubst(`$1',
         `[ ]*\(ppl_[^ /]+\)\(.*\)',
         `m4_extension(\1, m4_get_arity(\2), m4_get_attribute(\2))')')

dnl m4_extension(Procedure_Name, [Arity, Attribute])
dnl
dnl Appends "_code" to Procedure_Name so that it can match the name
dnl of one of the macros defined (if eveer) in file *_code.m4 and get
dnl therefore expanded to the corresponding code schema.
dnl
dnl By default, arguments Arity and Attribute are ignored. When and where
dnl these are needed (e.g., in the Prolog system files), the macro
dnl m4_extension will be referined appropriately.
dnl
dnl Note: the macro `$1_code' has to be called using builtin `indir'
dnl because it is not a legal m4 identifier (it contains `@').
define(`m4_extension', `ifdef(`$1_code', `indir(`$1_code')', `')')


dnl m4_procedure_names_to_code(Class_Counter, Class_Kind,
dnl                            Procedure_Name1, Procedure_Name2, ...)
dnl
dnl Each Procedure_Name`'i is replaced
dnl with code where the patterns have all been replaced.
define(`m4_procedure_names_to_code', `dnl
ifelse($#, 0, , $#, 1, ,$#, 2, ,
       $#, 3, `m4_procedure_name_to_code($1, $2, $3)',
       `dnl
m4_procedure_name_to_code($1, $2, $3)dnl
m4_procedure_names_to_code($1, $2, shift(shift(shift($@))))dnl
')dnl
')

dnl m4_procedure_name_to_code(Class_Counter, Class_Kind, Procedure_Name)
dnl
dnl The procedure specification is replaced with the code.
dnl All the schema patterns in the code are replaced.
define(`m4_procedure_name_to_code', `dnl
patsubst(`$3', `\(.*\)', `dnl
m4_replace_all_patterns($2,
  m4_replace_class_patterns($1, m4_get_code_schema(\1)),
    m4_pattern_list)')dnl
')

dnl m4_proc_keep_or_throw(
dnl     Class_Kind, Procedure_Info, +_or_-, Group1, Group2, ...)
dnl
dnl This compares the Class_Kind with the groups in the
dnl third and subsequent arguments that are also in the groups
dnl in the procedure info.
dnl For each group, it checks if Class_Kind is in that group;
dnl if it is, it checks if +group or -group
dnl (depending if +_or_- is + or - is in the Procedure_Info.
dnl Once it finds such a group, it stops iterating through the groups
dnl and expands to 1. If no such group is found, it expands to 0.
define(`m4_proc_keep_or_throw', `dnl
ifelse($#, 0, 0, $#, 1, 0, $#, 2, 0, $#, 3, 0,
  $#, 4, `m4_proc_keep_or_throw_aux($1, $2, $3, $4)',
    `ifelse(m4_proc_keep_or_throw_aux($1, $2, $3, $4), 1, 1,
      m4_proc_keep_or_throw($1, $2, $3, shift(shift(shift(shift($@))))))')dnl
')

dnl m4_proc_keep_or_throw_aux(
dnl     Class_Kind, Procedure_Info, +_or_-, Group)
dnl
dnl This checks if or Class_Kind is in Group;
dnl if it is, it checks if +Group or -Group
dnl (depending if +_or_-Group is in the Procedure_Info;
dnl if it is, then it expands to 1, otherwise, expands to 0.
define(`m4_proc_keep_or_throw_aux', `dnl
ifelse(m4_check_if_class_in_group($1, m4_$4_group), 1,
  `ifelse(index($2, $3$4), -1, 0, 1)', 0)dnl
')

dnl m4_check_if_class_in_group(Class_Kind, Class_Kind1, Class_Kind2, ...)
dnl
dnl This expands to 1 only if Class_Kind matches a Class_Kind`'i
dnl in the rest of the list; otherwise, it expands to 0.
define(`m4_check_if_class_in_group', `dnl
ifelse($#, 0, 0, $#, 1, 0,
$#, 2, `ifelse($1, $2, 1, 0)',
$1, $2, 1, `dnl
m4_check_if_class_in_group($1, shift(shift($@)))dnl
')dnl
')

dnl m4_filter(Class_Kind, Procedure_Name)
dnl
dnl Keeps just those procedure names that are needed for the given class kind.
dnl It first checks if there is a group including the Class_name
dnl in Procedure_Name, preceded by a -.
dnl if so, it expands to the empty string.
dnl If this is not the case, it checks if there is a group
dnl including the Class_name in Procedure_Name, preceded by a +.
dnl if so, it expands to the given Procedure_Name.
define(`m4_filter', `dnl
define(`m4_proc_info_string',
       `patsubst(`$2', `[ ]*ppl_[^ ]+ \(.*\)', \1)')dnl
ifelse(m4_proc_keep_or_throw($1, m4_proc_info_string, -, m4_group_names), 1, 0,
  m4_proc_keep_or_throw($1, m4_proc_info_string, +, m4_group_names))dnl
undefine(m4_proc_info_string)dnl
')

dnl m4_filter_all(Class_Kind, Procedure_Name1, ProcedureName2, ...)
dnl
dnl Keeps just those procedure names that are needed for the given class kind.
dnl The classes to be kept or filtered away are determined by extra info
dnl included with each Procedure_Name
define(`m4_filter_all', `dnl
ifelse($#, 0, ,$# , 1, ,
$#, 2,`ifelse(m4_filter($1, $2), 1, `$2')',
`ifelse(m4_filter($1, $2), 1, `$2,
')dnl
m4_filter_all($1, shift(shift($@)))dnl
')dnl
')

dnl m4_pre_extra_class_code(Class_Counter, Class_Kind)
dnl m4_post_extra_class_code(Class_Counter, Class_Kind)
dnl Default (empty) definitions for pre- and post- code for each class.
define(`m4_pre_extra_class_code', `')dnl
define(`m4_post_extra_class_code', `')dnl

dnl m4_one_class_code(Class_Counter, Class_Kind)
dnl
dnl Takes main procedure input list and each procedure is checked
dnl to see if there is a macro with "_code" extension that defines the code.
dnl Then a macro sets the class and other schematic components.
# define(`m4_one_class_code', `dnl
# m4_pre_extra_class_code($1, $2)dnl
# define(`m4_filtered_proc_list',
#        `m4_filter_all($2, m4_procedure_list)')dnl
# define(`m4_procedure_code',
#        `m4_procedure_names_to_code($2, m4_filtered_proc_list)')dnl
# m4_replace_class_patterns($1, m4_procedure_code)dnl
# undefine(`m4_procedure_code')dnl
# undefine(`m4_filtered_proc_list')dnl
# m4_post_extra_class_code($1, $2)dnl
# ')
define(`m4_one_class_code', `dnl
m4_pre_extra_class_code($1, $2)dnl
define(`m4_filtered_proc_list',
       `m4_filter_all($2, m4_procedure_list)')dnl
m4_procedure_names_to_code($1, $2, m4_filtered_proc_list)dnl
undefine(`m4_filtered_proc_list')dnl
m4_post_extra_class_code($1, $2)dnl
'))

dnl m4_all_classes_aux(counter)
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
define(`m4_all_classes_aux', `dnl
ifdef(m4_interface_class`'$1, `dnl
m4_one_class_code($1, m4_class$1_component1)dnl
m4_all_classes_aux(incr($1))')dnl
')

dnl m4_all_classes_code
dnl
dnl This calls initilization  macros that generate macro definitions
dnl for the class names for classes requested by the user
dnl (which are determined by the configuration).
dnl Then calls the main loop m4_all_classes_aux for generating
dnl code for all the required classes.
define(`m4_all_classes_code', `dnl
m4_init_interface_classes(m4_interface_classes_names)dnl
m4_init_cplusplus_classes(m4_cplusplus_classes_names)dnl
m4_all_classes_aux(1)dnl
')

divert`'dnl
