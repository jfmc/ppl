dnl Classes to be implemented and C++ versions of these classes.
include(ppl_interface_instantiations.m4)

dnl =====================================================================
dnl ===== The first set of macros here initialise the class names   =====
dnl ===== using "@" separated lists and defined                     =====
dnl ===== by macros in ppl_interface_instantiations.m4.             =====
dnl =====================================================================

dnl m4_init_interface_classes(Class_List)
dnl
dnl Parses the comma-separated list of class names Class_List
dnl for the names of the classes used to form the names of procedures
dnl in the user interface.
define(`m4_init_interface_classes', `m4_init_interface_classes_aux(1, $1)')

dnl m4_subst_comma(String1, String2,...)
dnl
dnl String1, String2,... is the "@"-separated list of C++ class names
dnl provided by the configuration.
dnl As the product classes have their arguments ","-separated,
dnl the "," separates the list into macro arguments.
dnl This macro iterates through these macro arguments, replacing the
dnl "," by "COMMA" so that the C++ names are handled as intended.
define(`m4_subst_comma',
`ifelse($#, 0, , $#, 1, $1,
  `$1`'COMMA`'m4_subst_comma(shift($@))')')

dnl m4_init_cplusplus_classes(Class_List)
dnl
dnl Parses the "@"-separated list of class names Class_List
dnl to be used in the C++ code implementing the interface procedures.
dnl Note that first the "," is replaced using the macro m4_subst_comma.
define(`m4_init_cplusplus_classes',
 `m4_init_cplusplus_classes_aux(1, m4_subst_comma($@))')


dnl m4_init_interface_classes_aux(Class_Counter, Class_List)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class_List    - is a tail part of the input list of interface
dnl                 class names.
dnl The macro also defines m4_num_classes to be the number of classes
dnl in the full list (ie counter + number in the current list - 1).
dnl The macro calls m4_init_interface_names to define the next
dnl interface names and then to to call this macro to recursively
dnl process the rest of the list.
define(`m4_init_interface_classes_aux', `dnl
ifelse($2, `',  `define(m4_num_classes, decr($1))',
  regexp(`$2', `\([^@]+\)@?\(.*\)',
    `m4_init_interface_names($1, \1, \2)'))`'dnl
')

dnl m4_init_interface_names(Class_Counter, Class, Class_List)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class         - is the interface class name, as input;
dnl Class_List    - is a tail part of the input list of interface
dnl                 class names.
dnl The macro has three cases.
dnl If the class name does not containg "Polyhedron",
dnl then m4_interface_class`'Class_Counter => Class;
dnl If the class name is simply "Polyhedron",
dnl then m4_interface_class`'Class_Counter => Class;
dnl Otherwise
dnl m4_interface_class`'Class_Counter => C_Class and
dnl m4_interface_class`'Class_Counter+1 => NNC_Class.
dnl In all cases, the m4_init_interface_classes_aux is called again
dnl to process the rest of the list with a new value for the class counter.
define(`m4_init_interface_names', `dnl
ifelse(
  index($2, Polyhedron), -1,
    `define(m4_interface_class`'$1, $2)`'dnl
m4_init_interface_classes_aux(incr($1), $3)',
  `$2', `Polyhedron',
    `define(m4_interface_class`'$1, $2)`'dnl
m4_init_interface_classes_aux(incr($1), $3)',
    `define(m4_interface_class`'$1, m4_prefix_polyhedron($2, C))`'dnl
define(m4_interface_class`'incr($1), m4_prefix_polyhedron($2, NNC))`'dnl
m4_init_interface_classes_aux(incr(incr($1)), $3)')`'dnl
')

dnl m4_prefix_polyhedron(Class, String)
dnl
dnl Every occurrence of Polyhedron in the name is replaced by
dnl String_Polyhedron.
define(`m4_prefix_polyhedron', `dnl
patsubst($1, Polyhedron, $2_Polyhedron)`'dnl
')

dnl m4_init_cplusplus_classes(Class_List)
dnl
dnl Parses the "@"-separated list of class names Class_List
dnl to be used in the C++ code implementing the interface procedures.
dnl Note that first the "," is replaced using the macro m4_subst_comma.
define(`m4_init_cplusplus_classes',
  `m4_init_cplusplus_classes_aux(1, m4_subst_comma($@))')

dnl m4_init_cplusplus_classes_aux(Class_Counter, Class_List)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class_List    - is a tail part of the input list of cplusplus
dnl                 class names.
dnl The macro calls m4_init_cplusplus_names to define the next
dnl cplusplus names and then to to call this macro to recursively
dnl process the rest of the list.
dnl The COMMA pattern is revised to @COMMA@ as soon as a class name
dnl has been separated from the @-separated list of classes and
dnl any unnecessary spaces removed.
define(`m4_init_cplusplus_classes_aux', `dnl
ifelse($2, `',  `',
  regexp(`$2', `\([^@]+\)@?\(.*\)',
    `m4_init_cplusplus_names(`$1',
      patsubst(\1, COMMA, @COMMA@), `\2')'))`'dnl
')

dnl m4_prefix_polyhedron(Class, String)
dnl
dnl Every occurrence of Polyhedron in the name is replaced by
dnl String_Polyhedron.
define(`m4_prefix_polyhedron', `dnl
patsubst($1, Polyhedron, $2_Polyhedron)`'dnl
')

dnl m4_init_cplusplus_names(Class_Counter, Class, Class_List)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class         - is the cplusplus class name, as input;
dnl Class_List    - is a tail part of the input list of cplusplus
dnl                 class names.
dnl The macro has three cases.
dnl If the class name does not contain "Polyhedron",
dnl then m4_init_cplusplus_names_aux(Class_Counter, Class) is called;
dnl If the class name is simply "Polyhedron",
dnl then m4_init_cplusplus_names_aux(Class_Counter, Class) is called;
dnl Otherwise
dnl then m4_init_cplusplus_names_aux(Class_Counter, ClassC)
dnl and m4_init_cplusplus_names_aux(Class_Counter, ClassNNC) are called
dnl where ClassC and ClassNNC are defined by m4_prefix_polyhedron(Class, C)
dnl and m4_prefix_polyhedron(Class, NNC), respectively;
dnl In all cases, the m4_init_cplusplus_classes_aux is called again
dnl to process the rest of the list with a new value for the class counter.
define(`m4_init_cplusplus_names', `dnl
ifelse(
  index($2, Polyhedron), -1, `m4_init_cplusplus_names_aux($1, $2)`'dnl
define(m4_class_topology`'$1, `')`'dnl
m4_init_cplusplus_classes_aux(incr($1), $3)',
  $2, `Polyhedron', `m4_init_cplusplus_names_aux($1, $2)`'dnl
define(m4_class_topology`'$1, `')`'dnl
m4_init_cplusplus_classes_aux(incr($1), $3)',
  `m4_init_cplusplus_names_aux($1, m4_prefix_polyhedron($2, C))`'dnl
define(m4_class_topology`'$1, C_)`'dnl
m4_init_cplusplus_names_aux(incr($1), m4_prefix_polyhedron($2, NNC))`'dnl
define(m4_class_topology`'incr($1), NNC_)`'dnl
m4_init_cplusplus_classes_aux(incr(incr($1)), $3)')`'dnl
')

dnl m4_init_cplusplus_names_aux(Class_Counter, Class)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class         - is the cplusplus class name, as input;
dnl This macro just defines three macros,
dnl defining the cplusplus_class_name`'Class_Counter,
dnl class_kind`'Class_Counter and class_body`'Class_Counter,
dnl where the kind is the part preceding the first "<"
dnl and the body is the rest of the full cplusplus name.
define(`m4_init_cplusplus_names_aux', `dnl
define(m4_cplusplus_class`'$1, `$2')`'dnl
m4_get_class_kind(`$1', `$2')`'dnl
m4_get_class_body(`$1', `$2')`'dnl
')

dnl m4_get_class_kind(Class_Counter, String)
dnl m4_get_class_body(Class_Counter, String)
dnl
dnl String = a cplusplus class name.
dnl
dnl The head (class_kind) and the body of
dnl the C++ class name in String are separated out.
dnl m4_class_kind`'Class_Counter = the first part before the "<"
dnl m4_class_body`'Class_Counter = the rest
dnl
dnl For example:
dnl If String = Polyhedron and Class_Counter = 1
dnl m4_class_kind1 => `Polyhedron'
dnl m4_class_body1 => `'
dnl If String = Polyhedra_Powerset<BD_Shape<signed char> >
dnl               and Class_Counter = 2
dnl m4_class_kind2 => `Polyhedra_Powerset'
dnl m4_class_body2 => `<BD_Shape<signed char> >'
define(`m4_get_class_kind',
  `define(m4_class_kind`'$1,
    `ifelse(`$2', `', ,
      `ifelse(index(`$2', <), -1, `$2',
        `regexp(`$2', `\([^ <]+\)[.]*', `\1')')')')')

define(`m4_get_class_body',
  `define(m4_class_body`'$1,
    `ifelse(`$2', `', ,
      `ifelse(index(`$2', <), -1, `',
        `regexp(`$2', `[^ <]+[<]\(.*\w>?\)[ ]*[>]', `\1')')')')')


dnl m4_group_names expands to all the group names.
dnl
dnl Each group_name in the expansion should
dnl have a corresponding definition for
dnl "m4_`'group_name`'_group
dnl which must be defined as a (comma separated) list of class kinds.
dnl
dnl If more groups are wanted, then these must be added to this list.
dnl and the list of class_kinds they include be defined.
dnl If a group_name occurs in the extra text preceded by a - after
dnl a procedure name, then no code for any classes in that group.
dnl Conversely, if a group_name preceded by a + occurs in the extra text after
dnl a procedure name, then no code for any classes in that group.
dnl
dnl More formally, if the extra text for a procedure includes
dnl +g1 +g2 ''' +gm and -h1 -h2 ... -hn
dnl where g1,g2, ..., gm and h1, h2, ..., hn are group names,
dnl then the actual class kinds that code will be generated for
dnl will be determined using set difference "\":
dnl (m4_g1_group union m4_g2_group union ... union m4_gm_group)\
dnl (m4_h1_group union m4_h2_group union ... union m4_hn_group)
dnl
dnl That is: in case of conflict between the + and - groups,
dnl the - takes precedence over the +;
dnl all class kinds for group_names preceded by - are filtered away.
dnl For instance, with:
dnl "+shape -bd_shape"
dnl following a procedure name, only code (for that procedure)
dnl for the Polyhedron and Octagonal_Shape class will be generated.
define(`m4_group_names', `dnl
all,
simple_pps,
simple,
shape,
wr_shape,
polyhedron,
grid,
bd_shape,
octagonal_shape,
polyhedra_powerset')

define(`m4_all_group',
  `Polyhedron, Grid, BD_Shape, Octagonal_Shape, Polyhedra_Powerset')
define(`m4_simple_pps_group',
  `Polyhedron, Grid, BD_Shape, Octagonal_Shape, Polyhedra_Powerset')
define(`m4_simple_group', `Polyhedron, Grid, BD_Shape, Octagonal_Shape')
define(`m4_shape_group', `Polyhedron, BD_Shape, Octagonal_Shape')
define(`m4_wr_shape_group', `BD_Shape, Octagonal_Shape')
define(`m4_polyhedron_group', Polyhedron)
define(`m4_grid_group', Grid)
define(`m4_bd_shape_group', BD_Shape)
define(`m4_octagonal_shape_group', Octagonal_Shape)
define(`m4_polyhedra_powerset_group', Polyhedra_Powerset)

dnl m4_pattern_list
dnl
dnl Returns a list of patterns (in lowercase) used for the generation
dnl of procedure names and code schemas.
define(`m4_pattern_list', `dnl
class,
cpp_class,
friend,
intopology,
topology,
disjunct,
represent,
dimension,
generator,
point,
constrainer,
state,
simplify,
abovebelow,
maxmin,
embedproject,
affimage,
comparison,
binop,
binminop,
widenexp,
box,
describe')

dnl The interface class name.
define(`m4_class_replacement', m4_interface_class`'$1)

dnl The cplusplus class name.
define(`m4_cpp_class_replacement', m4_cplusplus_class`'$1)

dnl The friend class name.
dnl First the default - every class is a friend of itself.
define(`m4_friend_replacement', m4_interface_class`'$1)
define(`m4_friend_alt_replacement', m4_cplusplus_class`'$1)

dnl Several classes for friend replacement use the next two macros:
dnl m4_same_class_string/4 and m4_same_class_string_aux/4

dnl m4_same_class_string(String, Name_Type, Class_Name_Type, Topology)
dnl
dnl String = class kind or cplusplus_class name (= the class body)
dnl depending on Class_Name_Type.
dnl Name_Type  = "interface" or "cplusplus".
dnl Class_Name_Type = "class_kind" or "cplusplus_class".
dnl This and the "_aux"  macros are needed to define the friend replacements.
dnl The macro expands to a list of either the full interface name or cplusplus
dnl name, depending on whether $2 = "interface" or "cplusplus"
define(`m4_same_class_string', `dnl
dnl Find all interface class names for $1 in the class list.
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_same_class_string_aux(
  $1, m4_$4`'m4_ind, m4_$2_class`'m4_ind, $3)`'dnl
')`'dnl
')

define(`m4_same_class_string_aux', `dnl
dnl comma is a separator so the first element has no comma.
ifelse($1, $4`'$2,
  `ifelse(m4_replace_list_start, 0,
     `undefine(`m4_replace_list_start')$4`'$3',
    `, '$4`'$3)')`'dnl
')

dnl For BD_Shape class kind, any generated class with kind BD_Shape
dnl is a friend.
dnl Also if Polyhedron is a generated class it is a friend
dnl
define(`m4_BD_Shape_friend_replacement', `dnl
dnl
dnl Initialise a flag to ensure the comma in the list is a separator only.
define(`m4_replace_list_start', 0)`'dnl
m4_same_class_string(
  BD_Shape, interface, m4_class_topology$1, class_kind)`'dnl
m4_same_class_string(
  Polyhedron, interface, m4_class_topology$1, class_kind)`'dnl
')

dnl Defines the alternative friend name for cplusplus code.
define(`m4_BD_Shape_friend_alt_replacement', `dnl
define(`m4_replace_list_start', 0)`'dnl
m4_same_class_string(
  BD_Shape, cplusplus, m4_class_topology$1, class_kind)`'dnl
m4_same_class_string(
  Polyhedron, cplusplus, m4_class_topology$1, class_kind)`'dnl
')

dnl For Octagon class kind, any generated class with kind BD_Shape
dnl is a friend.
dnl Also if Polyhedron is a generated class it is a friend
dnl
define(`m4_Octagon_friend_replacement', `dnl
define(`m4_replace_list_start', 0)`'dnl
m4_same_class_string(
  Octagon, interface, m4_class_topology$1, class_kind)`'dnl
m4_same_class_string(
  Polyhedron, interface, m4_class_topology$1, class_kind)`'dnl
')

define(`m4_Octagon_friend_alt_replacement', `dnl
define(`m4_replace_list_start', 0)`'dnl
m4_same_class_string(
  Octagon, cplusplus, m4_class_topology$1, class_kind)`'dnl
m4_same_class_string(
  Polyhedron, cplusplus, m4_class_topology$1, class_kind)`'dnl
')

dnl For Polyhedra_Powerset class kind, if the body is C_Polyhedron
dnl or NNC_Polyhedron,
dnl and Polyhedron is generated, then C_Polyhedron
dnl (if the body is C_Polyhedron) or
dnl NNC_Polyhedron (if the body is NNCC_Polyhedron)
dnl is a friend.
dnl
define(`m4_Polyhedra_Powerset_friend_replacement', `dnl
dnl
m4_interface_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, interface, m4_class_topology$1, cplusplus_class)`'dnl
')
dnl
define(`m4_Polyhedra_Powerset_friend_alt_replacement', `dnl
m4_cplusplus_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, cplusplus, m4_class_topology$1, cplusplus_class)`'dnl
')

dnl The topology of the domain element. The default is the empty string.
define(`m4_topology_replacement', `')
define(`m4_Polyhedron_topology_replacement', `C_, NNC_')

dnl The topology used to copy from another element of the domain
define(`m4_intopology_replacement', `')
define(`m4_Polyhedron_intopology_replacement', `C_, NNC_')

dnl The widening and extrapolation operators.
define(`m4_widenexp_replacement', `')
define(`m4_Polyhedron_widenexp_replacement', `BHRZ03, H79')
define(`m4_Grid_widenexp_replacement', `congruence, generator')
define(`m4_BD_Shape_widenexp_replacement', `BHMZ05, H79')
define(`m4_Octagonal_Shape_widenexp_replacement', `CH78')

dnl The shape classes have bounding boxes while the grid classes also
dnl have covering boxes.
define(`m4_box_replacement', `bounding_box')
define(`m4_Grid_box_replacement', `m4_box_replacement, covering_box')
define(`m4_Grid_box_alt_replacement', `shrink_bounding_box, get_covering_box')
define(`m4_Grid_box_bounding_box_alt_replacement', `shrink_bounding_box')
define(`m4_Grid_box_covering_box_alt_replacement', `get_covering_box')

dnl  Space or affine dimensions
define(`m4_dimension_replacement', `space_dimension, affine_dimension')
define(`m4_Polyhedra_Powerset_dimension_replacement',`space_dimension')

dnl The different kinds of objects use to generate a class.
define(`m4_generator_replacement', `generator')
define(`m4_Grid_generator_replacement', `grid_generator')

dnl  The different kinds of points.
define(`m4_point_replacement', `point')
define(`m4_Grid_point_replacement', `grid_point')

dnl  The constrainer objects used to describe a class.
define(`m4_constrainer_replacement', `constraint')
define(`m4_Grid_constrainer_replacement', `congruence')

dnl The different kinds of objects that are elements of a Polyhedra_Powerset.

dnl The class body is a cplusplus name but we also need the matching
dnl interface name.
dnl The interface name is found using two auxilliary macros:
dnl m4_get_interface_class_name/2 and
dnl m4_get_interface_class_name_aux/4

dnl m4_get_interface_class_name(Cpp_Class_Name, Topology)
dnl
dnl expands to the interface name for the cplusplus class.
dnl If the class is C_Polyhderon or NNC_Polyhedron
dnl then the topology has to be added to the configuration name.
define(`m4_get_interface_class_name', `dnl
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_get_interface_class_name_aux(
  $1, m4_cplusplus_class`'m4_ind, $2, m4_interface_class`'m4_ind)')`'dnl
')

define(`m4_get_interface_class_name_aux', `dnl
ifelse($1, $3`'$2, $3`'$4)`'dnl
')

define(`m4_disjunct_replacement', `dnl
m4_get_interface_class_name(m4_class_body`'$1, m4_class_topology`'$1)`'dnl
')
define(`m4_disjunct_alt_replacement', m4_class_body`'$1)')

dnl  The different kinds of objects use to represent a class.
define(`m4_represent_replacement', `constraint')
define(`m4_Polyhedron_represent_replacement',
         `constraint, generator')
define(`m4_Grid_represent_replacement',
         `constraint, grid_generator, congruence')

dnl  The different kinds of objects use to describe a class.
define(`m4_describe_replacement', `constraint')
define(`m4_Polyhedron_describe_replacement',
         `constraint, generator')
define(`m4_Grid_describe_replacement',
         `congruence, grid_generator')

dnl  The "is" predicates
define(`m4_state_replacement', `empty, universe, bounded')
define(`m4_Polyhedron_state_replacement',
         `m4_state_replacement, topologically_closed')
define(`m4_Grid_state_replacement',
        `m4_state_replacement, topologically_closed, discrete')
define(`m4_Polyhedra_Powerset_state_replacement',`')

dnl  The "simplify" predicates
define(`m4_simplify_replacement', `topological_closure_assign')
define(`m4_Polyhedra_Powerset_simplify_replacement', `pairwise_reduce')

dnl  Above or below
define(`m4_abovebelow_replacement', `above, below')

dnl  Maximize or Minimize
define(`m4_maxmin_replacement', `maximize, minimize')

dnl  Embed or project
define(`m4_embedproject_replacement', `and_embed, and_project')

dnl  Affine_image or affine_preimage
define(`m4_affimage_replacement', `affine_image, affine_preimage')

dnl  One object can be contained, strictly contained or disjoint in the other.
define(`m4_comparison_replacement',
         `contains, strictly_contains, is_disjoint_from')
define(`m4_Polyhedra_Powerset_comparison_replacement',
         `geometrically_covers, geometrically_equals')

dnl  The different kinds of binary operators.
define(`m4_binop_replacement',
         `intersection_assign, upper_bound_assign, difference_assign,
          concatenate_assign, time_elapse_assign')
define(`m4_Polyhedron_binop_replacement',
         `m4_binop_replacement, poly_hull_assign, poly_difference_assign')
define(`m4_Grid_binop_replacement',
         `m4_binop_replacement, join_assign')
define(`m4_BD_Shape_binop_replacement',
         `m4_binop_replacement, bds_hull_assign')
define(`m4_Octagonal_Shape_binop_replacement',
         `m4_binop_replacement, oct_hull_assign')
define(`m4_Polyhedra_Powerset_binop_replacement',
          `intersection_assign, poly_difference_assign, concatenate_assign,
           time_elapse_assign')

dnl  The different kinds of "and_minimize" binary operators.
define(`m4_binminop_replacement', `intersection_assign_and_minimize')
define(`m4_Polyhedron_binminop_replacement',
         `m4_binminop_replacement, poly_hull_assign_and_minimize')
define(`m4_Grid_binminop_replacement',
         `m4_binminop_replacement, join_assign_and_minimize')
