divert(-1)

# forloop(Iteration Variable, Starting Value, Finish Value,
#         Text to be Expanded)
#
# Code copied from m4 documentation.
define(`forloop',
       `pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop',
       `$4`'ifelse($1, `$3', ,
          `define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

# upcase(String), downcase(String)
#
# Code copied from m4 documentation.
define(`upcase', `translit(`$*', `a-z', `A-Z')')
define(`downcase', `translit(`$*', `A-Z', `a-z')')

# capfirstletter(String)
#
# Capitalises first letter of words that can have letters and underscores.
# Thus "xyz_abc" will become "Xyz_abc".
define(`capfirstletter',
      `regexp(`$1', `^\(\w\)\([\w_]*\)',
             upcase(`\1')`\2')')

# capfirstletters(String)
#
# Capitalises first letter of subwords that can have letters only.
# Thus "xyz_abc" will become "Xyz_Abc".
define(`capfirstletters',
     `ifelse(index(`$1', `_'), -1,
      `regexp(`$1', `^\(\w\)\(\w*\)',
             `upcase(`\1')`\2'')',
      regexp(`$1', `^\(\w\)\(\w*_\)\(\w\)\(\w*\)',
             `upcase(`\1')`\2'upcase(`\3')`\4''))')

# ifndef(String, String)
#
# If the macro is defined, use that definition, otherwise use $2
# as the definition.
define(`ifndef', `ifdef(`$1', $1, $2)')

# set_string(String, String)
#
# replaces capitalised form of dummy string `$1' in `$2'
# by the required actual string.
# alt_ means that the alternative string must be used if one exists.
# U means that the alt_actual string must be capitalised at start
# of word and after "_".
define(`set_string',
  `define(`ustring', `upcase($1)')dnl
ifelse(index(`$2', ustring), `-1', `$2',
     `define(`num_strings',
        ifndef(num_`'class`'_`'$1`'s, ifndef(num_`'$1`'s, 0)))dnl
ifelse(num_strings, 0, ,
         `forloop(`js', 1, num_strings, `dnl
define(`actual_string', ifndef(class`'_`'$1`'js, ifndef($1`'js, `')))dnl
define(`alt_actual_string', ifndef(alt_`'class`'_`'$1`'js, actual_string))dnl
define(`Uactual_string', capfirstletters(actual_string))dnl
define(`Ualt_actual_string', capfirstletters(alt_actual_string))dnl
patsubst(patsubst(patsubst(patsubst(`$2',
           U`'ustring, Uactual_string),
           UALT_`'ustring, Ualt_actual_string),
           ALT_`'ustring, alt_actual_string),
           ustring, actual_string)')')')')

# set_schema_strings(String, sequence_of strings)
#
# A (recursive) macro to set the schemas in the string in the first
# argument. The schemas are listed in arguments 2 to end.
define(`set_schema_strings', `ifelse($2, `', ``$1'',
  `set_schema_strings(set_string($2, $1), shift(shift($@)))')'))

# set_class(String)
#
# replaces dummy string `CLASS' by the actual class.
define(`set_class',
`patsubst(`patsubst(`$1',  `CLASS', class)', cLASS, downcase(class))')

# add_extension(String)
#
# the extensions (optionally prefix, postfix and infix text) are added.
# the extension can be controlled by the extra tokens at the end of the main
# text.
# The arity can be reset to n for a specific class by the flag class/n
# the `nofail/' flag indicates that the predicate(s) will always succeed.
define(`add_extension',
  `patsubst(`$1',
     `[ ]*\(ppl_[^ /]+\)/\([0-9]+\)[ ]*\([a-z]*\)[^\n]*!',
          `extension(\1, \2, \3)')')

# get_code(String)
#
# If the code exists get the code, otherwise return the empty string.
define(`get_code', `ifdef($1`'_code, $1`'_code, `')')

# replace_with_code(String)
#
# predicate schemas are replaced by the code schema
define(`replace_with_code',
  `patsubst(`$1',
     `[ ]*\(ppl_[^ /]+\)/\([0-9]+\)[ ]*\([a-z]*\)[^\n]*!',
          `get_code(\1)')')

# separator(String)
#
# String is returned unless if it is empty, then use `,'
define(`separator', `ifelse($1, `', `,', `$1')')

# term_sequence(Number of Iterations, String to be Repeated)
#
# generates a sequence of strings with the option of
# identifying each string uniquely and the option of
# changing the separator from the default `,'.
define(`term_sequence',
       `ifelse(`$1', 0, ,
         `$2(1)`'ifelse(`$1', 1, ,
          `forloop(`i', 2, `$1', `separator(`$3') $2(i)')')')')

# extend_special(String)
#
# adds the system specific extensions to the predicates that
# are dependent on the class.
define(`extend_predicates',
       `patsubst(`$1', `\(.*\)
',
  `ifelse((index(\1, CLASS) + index(\1, class)) == -2, 1, ,
        `set_schema_strings(add_extension(\1!),
            string_substitution_list)')')')

# extend_special(String)
#
# replaces with the code for the predicates that
# are dependent on the class.
define(`predicates_to_code',
       `patsubst(`$1', `\(.*\)
',
  `ifelse((index(\1, CLASS) + index(\1, class)) == -2, 1, ,
        `set_schema_strings(replace_with_code(\1!),
           string_substitution_list)')')')

define(`short_name', `substr(class, 0, 2)')

# filter(String)
#
# keeps just those predicates that are needed for the given class..
define(`filter',
  `patsubst(`$1', `\(.*
\)',
    `ifelse(index(\1, X`'short_name), -1,
       ifelse(index(\1, short_name), -1,
         ifelse(index(\1, Al), -1,
           ifelse(index(\1, class_group), -1,
             ifelse(index(\1, class_super_group), -1, , \1), \1), \1), \1))')')

divert`'dnl
