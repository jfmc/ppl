m4_divert(-1)

m4_define(`dnl', `m4_dnl')
dnl Classes to be implemented and C++ versions of these classes.
m4_include(ppl_interface_instantiations.m4)

dnl =====================================================================
dnl ===== The first set of macros here initialise the class names   =====
dnl ===== using "@" separated lists and defined                     =====
dnl ===== by macros in ppl_interface_instantiations.m4.             =====
dnl =====================================================================

dnl ---------------------------------------------------------------------
dnl =====  any macros needed for both interfaces and cplusplus      =====
dnl =====  names go here                           .                =====
dnl ---------------------------------------------------------------------

dnl m4_prefix_polyhedron(Class, String)
dnl
dnl Every occurrence of Polyhedron in the name is replaced by
dnl String_Polyhedron.
m4_define(`m4_prefix_polyhedron', `dnl
m4_patsubst($1, Polyhedron, $2_Polyhedron)`'dnl
')

dnl ---------------------------------------------------------------------
dnl =====  m4_init_interface_names is defined.                      =====
dnl ---------------------------------------------------------------------

dnl m4_init_interface_classes(Class_List)
dnl
dnl Parses the @-separated list of class names Class_List
dnl for the names of the classes used to form the names of procedures
dnl in the user interface.
m4_define(`m4_init_interface_classes', `m4_init_interface_classes_aux(1, $1)')

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
m4_define(`m4_init_interface_classes_aux', `dnl
m4_ifelse($2, `',  `m4_define(m4_num_classes, m4_decr($1))',
  m4_regexp(`$2', `\([^@]+\)@?\(.*\)',
    `m4_init_interface_names($1, \1, \2)'))`'dnl
')

dnl m4_init_interface_names(Class_Counter, Class, Class_List)
dnl
dnl Class_Counter - is the index to the first class in Class_List;
dnl Class         - is the interface class name, as input;
dnl Class_List    - is a tail part of the input list of interface
dnl                 class names.
dnl The macro has three cases.
dnl If the class name does not contain "Polyhedron",
dnl then m4_interface_class`'Class_Counter => Class;
dnl If the class name is simply "Polyhedron",
dnl then m4_interface_class`'Class_Counter => Class;
dnl Otherwise
dnl m4_interface_class`'Class_Counter => C_Class and
dnl m4_interface_class`'Class_Counter+1 => NNC_Class.
dnl In all cases, the m4_init_interface_classes_aux is called again
dnl to process the rest of the list with a new value for the class counter.
m4_define(`m4_init_interface_names', `dnl
m4_ifelse(
  m4_index($2, Polyhedron), -1,
    `m4_define(m4_interface_class`'$1, $2)`'dnl
m4_init_interface_classes_aux(m4_incr($1), $3)',
  `$2', `Polyhedron',
    `m4_define(m4_interface_class`'$1, $2)`'dnl
m4_init_interface_classes_aux(m4_incr($1), $3)',
    `m4_define(m4_interface_class`'$1, m4_prefix_polyhedron($2, C))`'dnl
m4_define(m4_interface_class`'m4_incr($1), m4_prefix_polyhedron($2, NNC))`'dnl
m4_init_interface_classes_aux(m4_incr(m4_incr($1)), $3)')`'dnl
')

dnl ---------------------------------------------------------------------
dnl =====  m4_init_cplusplus_names is defined.                      =====
dnl ---------------------------------------------------------------------

dnl m4_subst_comma(String1, String2,...)
dnl
dnl String1, String2,... is the "@"-separated list of C++ class names
dnl provided by the configuration.
dnl As the product classes have their arguments ","-separated,
dnl the "," separates the list into macro arguments.
dnl This macro iterates through these macro arguments, replacing the
dnl "," by "COMMA" so that the C++ names are handled as intended.
m4_define(`m4_subst_comma',
`m4_ifelse($#, 0, , $#, 1, $1,
  `$1`'COMMA`'m4_subst_comma(m4_shift($@))')')

dnl m4_init_cplusplus_classes(Class_List)
dnl
dnl Parses the "@"-separated list of class names Class_List
dnl to be used in the C++ code implementing the interface procedures.
dnl Note that first the "," is replaced using the macro m4_subst_comma.
m4_define(`m4_init_cplusplus_classes',
  `m4_init_cplusplus_classes_aux(1, m4_subst_comma($@))')

dnl m4_init_cplusplus_classes_aux(Class_Counter, Class_List)
dnl
dnl Class_Counter - is the index to the next class in Class_List;
dnl Class_List    - is a tail part of the input list of cplusplus
dnl                 class names.
dnl The macro calls m4_init_cplusplus_names to define the next
dnl cplusplus names and then calls itself to recursively
dnl process the rest of the list.
dnl The COMMA pattern is revised to @COMMA@ as soon as a class name
dnl has been separated from the @-separated list of classes.
m4_define(`m4_init_cplusplus_classes_aux', `dnl
m4_ifelse($2, `',  `',
  m4_regexp(`$2', `\([^@]+\)@?\(.*\)',
    `m4_init_cplusplus_names(`$1',
      m4_patsubst(\1, COMMA, @COMMA@), `\2')'))`'dnl
')

dnl m4_init_cplusplus_names(Class_Counter, Class, Class_List)
dnl
dnl Class_Counter - is the index to the next class in Class_List;
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
dnl and m4_init_cplusplus_names_aux(Class_Counter+1, ClassNNC) are called
dnl where ClassC and ClassNNC are defined by m4_prefix_polyhedron(Class, C)
dnl and m4_prefix_polyhedron(Class, NNC), respectively;
dnl In all cases, the m4_init_cplusplus_classes_aux is called again
dnl to process the rest of the list with a new value for the class counter.
m4_define(`m4_init_cplusplus_names', `dnl
m4_ifelse(
  m4_index($2, Polyhedron), -1, `m4_init_cplusplus_names_aux($1, $2)`'dnl
m4_init_cplusplus_classes_aux(m4_incr($1), $3)',
  $2, `Polyhedron', `m4_init_cplusplus_names_aux($1, $2)`'dnl
m4_init_cplusplus_classes_aux(m4_incr($1), $3)',
  `m4_init_cplusplus_names_aux($1, m4_prefix_polyhedron($2, C))`'dnl
m4_init_cplusplus_names_aux(m4_incr($1), m4_prefix_polyhedron($2, NNC))`'dnl
m4_init_cplusplus_classes_aux(m4_incr(m4_incr($1)), $3)')`'dnl
')

dnl m4_init_cplusplus_names_aux(Class_Counter, Class)
dnl
dnl Class_Counter - is the index to Class;
dnl Class         - is the cplusplus class name, as input;
dnl This macro defines three class dependent macros:
dnl - m4_cplusplus_class`'Class_Counter,
dnl - m4_class_kind`'Class_Counter and
dnl - m4_class_body`'Class_Counter,
dnl where the kind is the part preceding the first "<"
dnl and the body is the rest of the full cplusplus name.
dnl
dnl FIXME: This only works for product classes whose components
dnl        are _not_ themselves products.
dnl If the class kind is a Direct_Product or Open_Product,
dnl it also calls the macro m4_parse_body_class(`$1')
dnl to define subclasses
dnl m4_cplusplus_class`'Class_Counter_1 and
dnl m4_cplusplus_class`'Class_Counter_2
dnl for the two components.
dnl
m4_define(`m4_init_cplusplus_names_aux', `dnl
m4_define(m4_cplusplus_class`'$1, `$2')`'dnl
m4_get_class_kind(`$1', `$2')`'dnl
m4_get_class_body(`$1', `$2')`'dnl
m4_ifelse(m4_class_kind`'$1, Direct_Product,
  m4_parse_body_class(`$1'),
       m4_class_kind`'$1, Open_Product,
  m4_parse_body_class(`$1'))`'dnl
')

dnl m4_parse_body_class(Class_Counter)
dnl This only should be called when the class kind is a product
dnl and the components are separated by "@COMMA@".
dnl The components are defined as:
dnl m4_cplusplus_class`'Class_Counter_1 and
dnl m4_cplusplus_class`'Class_Counter_2
m4_define(`m4_parse_body_class', `dnl
m4_define(m4_cplusplus_class`'$1`'_1,
    `m4_regexp(m4_class_body$1, `\([^@]*\).*', `\1')')`'dnl
m4_define(m4_cplusplus_class`'$1`'_2,
    `m4_regexp(m4_class_body$1, `\([^@]*\)@COMMA@\(.*\)', `\2')')`'dnl
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
dnl If String = Pointset_Powerset<BD_Shape<signed char> >
dnl               and Class_Counter = 2
dnl m4_class_kind2 => `Pointset_Powerset'
dnl m4_class_body2 => `BD_Shape<signed char>'
m4_define(`m4_get_class_kind',
  `m4_define(m4_class_kind`'$1,
    `m4_ifelse(`$2', `', ,
      `m4_ifelse(m4_index(`$2', <), -1, `$2',
        `m4_regexp(`$2', `\([^ <]+\)[.]*', `\1')')')')')

m4_define(`m4_get_class_body',
  `m4_define(m4_class_body`'$1,
    `m4_ifelse(`$2', `', ,
      `m4_ifelse(m4_index(`$2', <), -1, `',
        `m4_regexp(`$2', `[^ <]+[<]\(.*\w>?\)[ ]*[>]', `\1')')')')')


dnl m4_get_class_topology(Class)
dnl
dnl expands to the empty string unless the class is
dnl C_Polyhedron or NNC_Polyhedron, in which case it expands to
dnl "C_" or "NNC_" respectively.
m4_define(`m4_get_class_topology', `dnl
m4_ifelse(m4_index($1, C_), 0, C_,
m4_index($1, NNC_), 0, NNC_)`'dnl
')

dnl m4_get_class_counter(Cpp_Class_Name, Topology)
dnl
dnl expands to the class counter for the cplusplus class.
dnl If the class is C_Polyhderon or NNC_Polyhedron
dnl then the topology may have to be added to the configuration name.
m4_define(`m4_get_class_counter', `dnl
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_ifelse(
  $1, m4_get_class_topology($1)`'m4_echo_unquoted(m4_cplusplus_class`'m4_ind),
    m4_ind,
  $1, m4_echo_unquoted(m4_cplusplus_class`'m4_ind),
    m4_ind)`'dnl
')`'dnl
')

dnl =====================================================================
dnl ===== The next set of macros define the groups used             =====
dnl ===== to specify which classes the schematic procedures apply   =====
dnl ===== see ppl_interface_generators_c_dat.m4       .             =====
dnl ===== and ppl_interface_generators_prolog_dat.m4.      .        =====
dnl =====================================================================

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
m4_define(`m4_group_names', `dnl
all,
simple_pps,
simple,
shape,
wr_shape,
polyhedron,
grid,
bd_shape,
octagonal_shape,
pointset_powerset')

m4_define(`m4_all_group',
  `Polyhedron, Grid, BD_Shape, Octagonal_Shape, Pointset_Powerset')
m4_define(`m4_simple_pps_group',
  `Polyhedron, Grid, BD_Shape, Octagonal_Shape, Pointset_Powerset')
m4_define(`m4_simple_group', `Polyhedron, Grid, BD_Shape, Octagonal_Shape')
m4_define(`m4_shape_group', `Polyhedron, BD_Shape, Octagonal_Shape')
m4_define(`m4_wr_shape_group', `BD_Shape, Octagonal_Shape')
m4_define(`m4_polyhedron_group', Polyhedron)
m4_define(`m4_grid_group', Grid)
m4_define(`m4_bd_shape_group', BD_Shape)
m4_define(`m4_octagonal_shape_group', Octagonal_Shape)
m4_define(`m4_pointset_powerset_group', Pointset_Powerset)

dnl =====================================================================
dnl ===== The next set of macros define the replacements            =====
dnl ===== for the patterns used                                     =====
dnl =====================================================================

dnl m4_pattern_list
dnl
dnl Returns a list of patterns (in lowercase) used for the generation
dnl of procedure names and code schemas.
m4_define(`m4_pattern_list', `dnl
class,
cpp_class,
friend,
intopology,
topology,
disjunct,
build_represent,
get_represent,
relation_represent,
add_represent,
dimension,
generator,
point,
constrainer,
has_property,
simplify,
abovebelow,
maxmin,
embedproject,
affimage,
comparison,
binop,
binminop,
widenexpn,
widen,
extrapolation,
narrow,
limitedbounded,
box')

dnl The interface class name.
m4_define(`m4_class_replacement', m4_interface_class`'$1)

dnl The cplusplus class name.
m4_define(`m4_cpp_class_replacement', m4_cplusplus_class`'$1)

dnl The friend class name.
dnl First the default - every class is a friend of itself.
m4_define(`m4_friend_replacement', m4_interface_class`'$1)
m4_define(`m4_friend_alt_replacement', m4_cplusplus_class`'$1)

dnl To allow for other classes to be friends,
dnl we cannot just take a predefined list of friends as some
dnl may not be instantiated and available.
dnl
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
m4_define(`m4_same_class_string', `dnl
dnl Find all interface class names for $1 in the class list.
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_same_class_string_aux(
  $1, m4_$4`'m4_ind, m4_$2_class`'m4_ind, $3)`'dnl
')`'dnl
')

m4_define(`m4_same_class_string_aux', `dnl
dnl comma is a separator so the first element has no comma.
m4_ifelse($1, $2,
  `m4_ifelse(m4_replace_list_start, 0,
     `m4_undefine(`m4_replace_list_start')$4`'$3',
    `, '$4`'$3)')`'dnl
')

dnl This is commented for now as this is not in the C++ interface.
dnl
dnl For BD_Shape class kind, any generated class with kind BD_Shape
dnl is a friend.
dnl Also if C_Polyhedron is a generated class it is a friend
dnl
dnl m4_define(`m4_BD_Shape_friend_replacement', `dnl
dnl
dnl Initialise a flag to ensure the comma in the list is a separator only.
dnl m4_define(`m4_replace_list_start', 0)`'dnl
dnl m4_same_class_string(
dnl   BD_Shape, interface,,
dnl   class_kind)`'dnl
dnl m4_same_class_string(
dnl   Polyhedron, interface,,
dnl   class_kind)`'dnl
dnl ')

dnl This is commented for now as this is not in the C++ interface.
dnl
dnl Defines the alternative friend name for cplusplus code.
dnl m4_define(`m4_BD_Shape_friend_alt_replacement', `dnl
dnl m4_define(`m4_replace_list_start', 0)`'dnl
dnl m4_same_class_string(
dnl   BD_Shape, cplusplus,
dnl   class_kind)`'dnl
dnl m4_same_class_string(
dnl   Polyhedron, cplusplus,,
dnl   class_kind)
dnl ')

dnl This is commented for now as this is not in the C++ interface.
dnl
dnl For Octagon class kind, any generated class with kind Octagon
dnl is a friend.
dnl Also if Polyhedron is a generated class it is a friend
dnl
dnl m4_define(`m4_Octagonal_Shape_friend_replacement', `dnl
dnl m4_define(`m4_replace_list_start', 0)`'dnl
dnl m4_same_class_string(
dnl   Octagonal_Shape, interface,,
dnl   class_kind)`'dnl
dnl m4_same_class_string(
dnl   Polyhedron, interface,,
dnl   class_kind)`'dnl
dnl ')

dnl This is commented for now as this is not in the C++ interface.
dnl
dnl m4_define(`m4_Octagonal_Shape_friend_alt_replacement', `dnl
dnl m4_define(`m4_replace_list_start', 0)`'dnl
dnl m4_same_class_string(
dnl   Octagonal_Shape, cplusplus,,
dnl   class_kind)`'dnl
dnl m4_same_class_string(
dnl   Polyhedron, cplusplus, C_,
dnl   class_kind)`'dnl
dnl ')

dnl For Pointset_Powerset class kind, if the body is C_Polyhedron
dnl or NNC_Polyhedron,
dnl and Polyhedron is generated, then C_Polyhedron
dnl (if the body is C_Polyhedron) or
dnl NNC_Polyhedron (if the body is NNC_Polyhedron)
dnl is a friend.
dnl
m4_define(`m4_Pointset_Powerset_friend_replacement', `dnl
dnl
m4_interface_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, interface, m4_get_class_topology($1), cplusplus_class)`'dnl
')
dnl
m4_define(`m4_Pointset_Powerset_friend_alt_replacement', `dnl
m4_cplusplus_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, cplusplus, m4_get_class_topology($1), cplusplus_class)`'dnl
')

dnl The topology of the domain element. The default is the empty string.
m4_define(`m4_topology_replacement', `')
m4_define(`m4_Polyhedron_topology_replacement', `C_, NNC_')

dnl The topology used to copy from another element of the domain
m4_define(`m4_intopology_replacement', `')
m4_define(`m4_Polyhedron_intopology_replacement', `C_, NNC_')

dnl The widening operators.
m4_define(`m4_widen_replacement', `')
m4_define(`m4_Polyhedron_widen_replacement', `BHRZ03, H79')
m4_define(`m4_Grid_widen_replacement', `congruence, generator')
m4_define(`m4_BD_Shape_widen_replacement', `BHMZ05, H79')
m4_define(`m4_Octagonal_Shape_widen_replacement', `BHMZ05')
m4_define(`m4_Pointset_Powerset_widen_replacement', `BHZ03')
dnl The alt_replacement defines the certificates for the widenings
m4_define(`m4_Polyhedron_widen_alt_replacement', `BHRZ03, H79')
m4_define(`m4_Grid_widen_alt_replacement', `Grid, Grid')

dnl The extrapolation operators.
m4_define(`m4_extrapolation_replacement', `')
m4_define(`m4_BD_Shape_extrapolation_replacement',
  `CC76')
m4_define(`m4_Octagonal_Shape_extrapolation_replacement',
   `CC76')

dnl The limited/bounded extrapolation operators.
m4_define(`m4_widenexpn_replacement', `m4_widen_replacement')
m4_define(`m4_Polyhedron_widenexpn_replacement',
  `m4_Polyhedron_widen_replacement')
m4_define(`m4_Grid_widenexpn_replacement', `m4_Grid_widen_replacement')
m4_define(`m4_BD_Shape_widenexpn_replacement',
  `m4_BD_Shape_widen_replacement,
   m4_BD_Shape_extrapolation_replacement')
m4_define(`m4_Octagonal_Shape_widenexpn_replacement',
   `m4_Octagonal_Shape_widen_replacement,
    m4_Octagonal_Shape_extrapolation_replacement')
m4_define(`m4_Pointset_Powerset_widenexpn_replacement',
  `m4_Pointset_Powerset_widen_replacement')

dnl The narrowing operators.
m4_define(`m4_narrow_replacement', `CC76')

dnl Limited or bounded
m4_define(`m4_limitedbounded_replacement', `limited')
m4_define(`m4_Polyhedron_limitedbounded_replacement', `limited, bounded')

dnl The shape classes have bounding boxes while the grid classes also
dnl have covering boxes.
# m4_define(`m4_box_replacement', `bounding_box')
# m4_define(`m4_Grid_box_replacement', `m4_box_replacement, covering_box')
# m4_define(`m4_Grid_box_alt_replacement', `shrink_bounding_box, get_covering_box')
# m4_define(`m4_Grid_box_bounding_box_alt_replacement', `shrink_bounding_box')
# m4_define(`m4_Grid_box_covering_box_alt_replacement', `get_covering_box')
m4_define(`m4_box_replacement', `bounding_box')
m4_define(`m4_Grid_box_replacement', `m4_box_replacement, covering_box')

dnl  Space or affine dimensions
m4_define(`m4_dimension_replacement', `space_dimension, affine_dimension')
m4_define(`m4_Pointset_Powerset_dimension_replacement',`space_dimension')

dnl The different kinds of objects use to generate a class.
m4_define(`m4_generator_replacement', `generator')
m4_define(`m4_Grid_generator_replacement', `grid_generator')

dnl  The different kinds of points.
m4_define(`m4_point_replacement', `point')
m4_define(`m4_Grid_point_replacement', `grid_point')

dnl  The constrainer objects used to describe a class.
m4_define(`m4_constrainer_replacement', `constraint')
m4_define(`m4_Grid_constrainer_replacement', `congruence')

dnl The different kinds of objects that are elements of a Pointset_Powerset.

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
m4_define(`m4_get_interface_class_name', `dnl
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_ifelse($1,
  m4_get_class_topology($1)`'m4_echo_unquoted(m4_cplusplus_class`'m4_ind),
  m4_get_class_topology($1)`'m4_echo_unquoted(m4_interface_class`'m4_ind))`'dnl
')`'dnl
')

m4_define(`m4_disjunct_replacement', `dnl
m4_get_interface_class_name(m4_class_body`'$1)`'dnl
')
m4_define(`m4_disjunct_alt_replacement', m4_class_body`'$1)

dnl  The different kinds of objects that can build a class.
m4_define(`m4_build_represent_replacement', `constraint, generator')
m4_define(`m4_Polyhedron_build_represent_replacement',
         `constraint, generator')
m4_define(`m4_Grid_build_represent_replacement',
         `constraint, grid_generator, congruence')

dnl  The different kinds of alternative objects that can build
dnl  the same class.
dnl  At the moment, this is just used for the test data generator.
m4_define(`m4_build_represent_alt_replacement', `generator, constraint')
m4_define(`m4_Polyhedron_build_represent_alt_replacement',
         `generator, constraint')
m4_define(`m4_Grid_build_represent_alt_replacement',
         `constraint, congruence, grid_generator')

dnl  The different kinds of objects that can have a relation with a class.
m4_define(`m4_relation_represent_replacement', `constraint')
m4_define(`m4_Polyhedron_relation_represent_replacement',
         `constraint, generator')
m4_define(`m4_Grid_relation_represent_replacement',
         `constraint, grid_generator, congruence')

dnl  The different kinds of objects that can be added to a class.
m4_define(`m4_add_represent_replacement', `constraint')
m4_define(`m4_Polyhedron_add_represent_replacement',
         `constraint, generator')
m4_define(`m4_Grid_add_represent_replacement',
         `constraint, grid_generator, congruence')

dnl  The different kinds of objects that can be obtained from a
dnl  class description.
m4_define(`m4_get_represent_replacement', `constraint')
m4_define(`m4_Polyhedron_get_represent_replacement',
         `constraint, generator, congruence')
m4_define(`m4_Grid_get_represent_replacement',
         `congruence, grid_generator')

dnl  The unary "has_property" predicates
m4_define(`m4_has_property_replacement', `is_empty, is_universe, is_bounded, contains_integer_point, is_topologically_closed')
m4_define(`m4_Grid_has_property_replacement',
        `m4_has_property_replacement, is_discrete')
m4_define(`m4_Pointset_Powerset_has_property_replacement',`')

dnl  The "simplify" predicates
m4_define(`m4_simplify_replacement', `topological_closure_assign')
m4_define(`m4_Pointset_Powerset_simplify_replacement', `pairwise_reduce')

dnl  Above or below
m4_define(`m4_abovebelow_replacement', `above, below')

dnl  Maximize or Minimize
m4_define(`m4_maxmin_replacement', `maximize, minimize')

dnl  Embed or project
m4_define(`m4_embedproject_replacement', `and_embed, and_project')

dnl  Affine_image or affine_preimage
m4_define(`m4_affimage_replacement', `affine_image, affine_preimage')

dnl  One object can be contained, strictly contained or disjoint in the other.
m4_define(`m4_comparison_replacement',
         `contains, strictly_contains, is_disjoint_from')
m4_define(`m4_Pointset_Powerset_comparison_replacement',
         `geometrically_covers, geometrically_equals')

dnl  The different kinds of binary operators.
m4_define(`m4_binop_replacement',
         `intersection_assign, upper_bound_assign, difference_assign,
          concatenate_assign, time_elapse_assign')
m4_define(`m4_Polyhedron_binop_replacement',
         `m4_binop_replacement, poly_hull_assign, poly_difference_assign')
m4_define(`m4_Grid_binop_replacement',
         `m4_binop_replacement, join_assign')
m4_define(`m4_BD_Shape_binop_replacement',
         `m4_binop_replacement, bds_hull_assign')
m4_define(`m4_Octagonal_Shape_binop_replacement',
         `m4_binop_replacement, oct_hull_assign')
m4_define(`m4_Pointset_Powerset_binop_replacement',
          `intersection_assign, poly_difference_assign, concatenate_assign,
           time_elapse_assign')

dnl  The different kinds of "and_minimize" binary operators.
m4_define(`m4_binminop_replacement', `intersection_assign_and_minimize')
m4_define(`m4_Polyhedron_binminop_replacement',
         `m4_binminop_replacement, poly_hull_assign_and_minimize')
m4_define(`m4_Grid_binminop_replacement',
         `m4_binminop_replacement, join_assign_and_minimize')

m4_divert`'