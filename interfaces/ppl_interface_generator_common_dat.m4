m4_define(`dnl', `m4_dnl')`'dnl
m4_divert(-1)
dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

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
m4_define(`m4_init_interface_names', `dnl
m4_define(m4_interface_class`'$1, $2)`'dnl
m4_init_interface_classes_aux(m4_incr($1), $3)`'dnl
')

dnl FIXME: Old code for allowing for having Pointset_Powerset_Polyhedron
dnl        classes. Now we require the topology C or NNC to be given
dnl        in the C++ name in the configuration.
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
dnl m4_define(`m4_init_interface_names', `dnl
dnl m4_ifelse(
dnl   m4_index($2, Polyhedron), -1,
dnl     `m4_define(m4_interface_class`'$1, $2)`'dnl
dnl m4_init_interface_classes_aux(m4_incr($1), $3)',
dnl   `$2', `Polyhedron',
dnl     `m4_define(m4_interface_class`'$1, $2)`'dnl
dnl m4_init_interface_classes_aux(m4_incr($1), $3)',
dnl     `m4_define(m4_interface_class`'$1, m4_prefix_polyhedron($2, C))`'dnl
dnl m4_define(m4_interface_class`'m4_incr($1), m4_prefix_polyhedron($2, NNC))`'dnl
dnl m4_init_interface_classes_aux(m4_incr(m4_incr($1)), $3)')`'dnl
dnl ')

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
m4_define(`m4_init_cplusplus_names', `dnl
m4_init_cplusplus_names_aux($1, $2)`'dnl
m4_init_cplusplus_classes_aux(m4_incr($1), $3)`'dnl
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
dnl FIXME: This is out-of-date and only worked for product classes
dnl        whose components were _not_ themselves products.
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
dnl
dnl FIXME: This code is untested.
m4_ifelse(m4_class_kind`'$1, Direct_Product,
              m4_parse_body_class(`$1'),
          m4_class_kind`'$1, Open_Product,
              m4_parse_body_class(`$1'))`'dnl
dnl
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


dnl m4_get_disjunct_topology(Class_Counter)
dnl
dnl expands to the empty string unless the disjunct is
dnl C_Polyhedron or NNC_Polyhedron, in which case it expands to
dnl "C_" or "NNC_" respectively.
m4_define(`m4_get_disjunct_topology', `dnl
m4_ifelse(m4_index(m4_class_body`'$1, C_), 0, C_,
m4_index(m4_class_body`'$1, NNC_), 0, NNC_)`'dnl
')

dnl m4_remove_topology(Class_Name)
dnl
dnl expands to the class name unless it is
dnl C_Polyhedron or NNC_Polyhedron, in which case it expands to
dnl "Polyhedron".
m4_define(`m4_remove_topology', `dnl
m4_ifelse(m4_index($1, C_), 0, Polyhedron,
            m4_index($1, NNC_), 0, Polyhedron, $1)`'dnl
')

dnl m4_remove_topology(Class_Name)
dnl
dnl expands to the class name unless it is
dnl C_Polyhedron or NNC_Polyhedron, in which case it expands to
dnl "Polyhedron".
m4_define(`m4_remove_topologies', `dnl
m4_ifelse($1, 0, ,
            $1, `m4_remove_topologies(m4_remove_topology($1), m4_shift($@))')
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
dnl ===== The next set of macros define the groups used to          =====
dnl ===== specify to which classes the schematic procedures apply;  =====
dnl ===== see ppl_interface_generators_common_procedure_list.m4     =====
dnl ===== and <interface>/ppl_interface_generators_<interface>_dat.m4 ===
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
dnl a procedure name, then no code for any classes in that group is generated.
dnl Conversely, if a group_name preceded by a + occurs in the extra text after
dnl a procedure name, then code for those classes in that group that are
dnl not covered by a - is generated.
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

dnl ---------------------------------------------------------------------
dnl First the list of patterns. Note that the order is important.
dnl ---------------------------------------------------------------------

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
cpp_disjunct,
disjunct,
classtopology,
build_represent,
get_represent,
relation_represent,
add_represent,
partition,
superclass,
recycle,
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
ub_exact,
disjunct_extrapolation,
disjunct_widen,
widenexpn,
widen,
extrapolation,
narrow,
limitedbounded,
box')

dnl ---------------------------------------------------------------------
dnl Define the replacements for these patterns
dnl ---------------------------------------------------------------------

dnl ---------------------------------------------------------------------
dnl pattern == class
dnl The class being generated
dnl ---------------------------------------------------------------------

dnl The interface class name.
m4_define(`m4_class_replacement', m4_interface_class`'$1)

dnl The cplusplus class name.
m4_define(`m4_cpp_class_replacement', m4_cplusplus_class`'$1)

dnl ---------------------------------------------------------------------
dnl pattern == friend
dnl A class can be built from any other class named as a "friend".
dnl A friend must be one of the classes named in the instantiations
dnl ---------------------------------------------------------------------

dnl Some of the class specific friend replacements use the next two macros:
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

dnl The friend class name.
dnl There is the interface name as default friend,
dnl the interface name but where "Polyhedron" does not include
dnl the topology (alt_friend), and the C++ name (cppx_friend).
dnl
dnl To allow for other classes to be friends,
dnl we cannot just take a predefined list of friends as some
dnl may not be instantiated and available.
m4_define(`m4_friend_replacement', `m4_all_friends(interface)')

m4_define(`m4_friend_alt_replacement', `dnl
m4_all_friends(interface, no_topology)`'dnl
')

m4_define(`m4_friend_cppx_replacement',`m4_all_friends(cplusplus)')

m4_define(`m4_all_friends', `dnl
m4_patsubst(m4_all_friends_aux($1, $2), `@COMMA@', `, ')`'dnl
')
m4_define(`m4_all_friends_aux', `dnl
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_ifelse(m4_echo_unquoted(m4_class_kind`'m4_ind), Pointset_Powerset, ,
m4_echo_unquoted(m4_class_kind`'m4_ind), Domain_Product, , `dnl
m4_define(`m4_friend_class', m4_`'$1`'_class`'m4_ind)`'dnl
m4_ifelse(m4_friend_class, Polyhedron,
          m4_ifelse($2, `',
          m4_one_friend(C_`'m4_friend_class@COMMA@NNC_`'m4_friend_class),
          m4_one_friend(m4_friend_class@COMMA@m4_friend_class)),
          m4_one_friend(m4_friend_class))`'dnl
m4_undefine(`m4_friend_class')')')`'dnl
')

m4_define(`m4_one_friend', `dnl
m4_ifelse(m4_ind, 1, `$1', @COMMA@`'$1)`'dnl
')

dnl For Pointset_Powerset class kind, if the body is C_Polyhedron
dnl or NNC_Polyhedron, and Polyhedron is generated, then C_Polyhedron
dnl (if the body is C_Polyhedron) or NNC_Polyhedron
dnl (if the body is NNC_Polyhedron) is a friend.
dnl
m4_define(`m4_Pointset_Powerset_friend_replacement', `dnl
dnl
m4_interface_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, interface, m4_get_class_topology($1), cplusplus_class)`'dnl
')
dnl
m4_define(`m4_Pointset_Powerset_friend_cppx_replacement', `dnl
m4_cplusplus_class$1`'dnl
m4_same_class_string(
  m4_class_body$1, cplusplus, m4_get_class_topology($1), cplusplus_class)`'dnl
')

dnl ---------------------------------------------------------------------
dnl pattern == topology or intopology
dnl This is C_ or NNC_ if the class is Polyhedron and `' otherwise
dnl ---------------------------------------------------------------------

dnl The topology of the domain element. The default is the empty string.
m4_define(`m4_topology_replacement', `')
m4_define(`m4_Polyhedron_topology_replacement', `C_, NNC_')

dnl The topology used to copy from another element of the domain
m4_define(`m4_intopology_replacement', `')
m4_define(`m4_Polyhedron_intopology_replacement', `C_, NNC_')

dnl ---------------------------------------------------------------------
dnl pattern == widen
dnl ---------------------------------------------------------------------
dnl The widening operators.
m4_define(`m4_widen_replacement', `')
m4_define(`m4_Polyhedron_widen_replacement', `BHRZ03, H79')
m4_define(`m4_Grid_widen_replacement', `congruence, generator')
m4_define(`m4_BD_Shape_widen_replacement', `BHMZ05, H79')
m4_define(`m4_Octagonal_Shape_widen_replacement', `BHMZ05')
m4_define(`m4_Pointset_Powerset_widen_replacement', `BHZ03')
dnl The alt_replacement defines the certificates for the widenings
m4_define(`m4_Polyhedron_widen_alt_replacement', `BHRZ03, H79')
m4_define(`m4_BD_Shape_widen_alt_replacement', `H79, H79')
m4_define(`m4_Octagonal_Shape_widen_alt_replacement', `H79')
m4_define(`m4_Grid_widen_alt_replacement', `Grid, Grid')

dnl ---------------------------------------------------------------------
dnl pattern == extrapolation
dnl ---------------------------------------------------------------------
m4_define(`m4_extrapolation_replacement', `NONE')
m4_define(`m4_BD_Shape_extrapolation_replacement',
  `CC76')
m4_define(`m4_Octagonal_Shape_extrapolation_replacement',
   `CC76')

dnl ---------------------------------------------------------------------
dnl pattern == widenexp
dnl The limited/bounded extrapolation operators.
dnl ---------------------------------------------------------------------
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

dnl ---------------------------------------------------------------------
dnl pattern == narrow
dnl ---------------------------------------------------------------------
m4_define(`m4_narrow_replacement', `CC76')

dnl ---------------------------------------------------------------------
dnl pattern == limitedbounded
dnl limited/bounded are qualifiers for widening and extrapolation
dnl operations.
dnl ---------------------------------------------------------------------
m4_define(`m4_limitedbounded_replacement', `limited')
m4_define(`m4_Polyhedron_limitedbounded_replacement', `limited, bounded')


dnl ---------------------------------------------------------------------
dnl pattern == box
dnl The shape classes have bounding boxes while the grid classes also
dnl have covering boxes.
dnl ---------------------------------------------------------------------
m4_define(`m4_box_replacement', `bounding_box')
m4_define(`m4_Grid_box_replacement', `m4_box_replacement, covering_box')

dnl ---------------------------------------------------------------------
dnl pattern == dimension
dnl ---------------------------------------------------------------------
m4_define(`m4_dimension_replacement', `space_dimension, affine_dimension')
m4_define(`m4_Pointset_Powerset_dimension_replacement',`space_dimension')

dnl ---------------------------------------------------------------------
dnl pattern == generator
dnl The different kinds of objects use to generate a class.
dnl ---------------------------------------------------------------------
m4_define(`m4_generator_replacement', `generator')
m4_define(`m4_Grid_generator_replacement', `grid_generator')

dnl  The different kinds of points.
m4_define(`m4_point_replacement', `point')
m4_define(`m4_Grid_point_replacement', `grid_point')

dnl  The constrainer objects used to describe a class.
m4_define(`m4_constrainer_replacement', `constraint')
m4_define(`m4_Grid_constrainer_replacement', `congruence')

dnl ---------------------------------------------------------------------
dnl pattern == cpp_disjunct or disjunct
dnl The different kinds of objects that are elements of a Pointset_Powerset.
dnl ---------------------------------------------------------------------

dnl If the class is C_Polyhderon or NNC_Polyhedron the topology is removed
dnl but the class topology replacement is then also defined.
m4_define(`m4_cpp_disjunct_replacement', `dnl
m4_remove_topology(m4_class_body`'$1)`'dnl
m4_define(`m4_classtopology_replacement',
  `m4_get_class_topology(m4_class_body`'$1)')`'dnl
')

dnl We need to get the interface name from the cpp name.
dnl The interface name is found using auxilliary macro:
dnl m4_get_interface_class_name(Cpp_Class_Name, Topology)
dnl which expands to the interface name for the cplusplus class.
m4_define(`m4_get_interface_class_name', `dnl
m4_forloop(m4_ind, 1, m4_num_classes, `dnl
m4_ifelse($1,
  m4_get_class_topology($1)`'m4_echo_unquoted(m4_cplusplus_class`'m4_ind),
  m4_get_class_topology($1)`'m4_echo_unquoted(m4_interface_class`'m4_ind))`'dnl
')`'dnl
')

dnl If the class is C_Polyhderon or NNC_Polyhedron the topology is removed
dnl but the class topology replacement is then also defined.
m4_define(`m4_disjunct_replacement', `dnl
m4_remove_topology(m4_get_interface_class_name(m4_class_body`'$1))`'dnl
m4_define(`m4_classtopology_replacement',
  `m4_get_class_topology(m4_class_body`'$1)')`'dnl
')

dnl ---------------------------------------------------------------------
dnl pattern == disjunct_widen
dnl ---------------------------------------------------------------------

m4_define(`m4_disjunct_kind',
  `m4_define(`m4_disj', m4_class_body`'$1)`'dnl
    m4_ifelse(m4_disj, `', ,
      `m4_ifelse(m4_index(m4_disj, <), -1, m4_disj,
        `m4_regexp(m4_disj, `\([^ <]+\)[.]*', `\1')')')`'dnl
m4_undefine(`m4_disj')`'dnl
')

m4_define(`m4_disjunct_widen_replacement',
  `m4_echo_unquoted(m4_`'m4_remove_topology(m4_disjunct_kind($1))`'_widen_replacement)')

m4_define(`m4_disjunct_widen_alt_replacement',
  `m4_echo_unquoted(m4_`'m4_remove_topology(m4_disjunct_kind($1))`'_widen_alt_replacement)')

dnl ---------------------------------------------------------------------
dnl pattern == disjunct_extrapolation
dnl ---------------------------------------------------------------------

m4_define(`m4_disjunct_extrapolation_replacement',
  `m4_echo_unquoted(m4_`'m4_remove_topology(m4_disjunct_kind($1))`'_extrapolation_replacement)')

m4_define(`m4_disjunct_extrapolation_alt_replacement',
  `m4_echo_unquoted(m4_`'m4_remove_topology(m4_disjunct_kind($1))`'_extrapolation_alt_replacement)')


dnl ---------------------------------------------------------------------
dnl pattern == build_represent
dnl  The different kinds of objects that can build a class.
dnl ---------------------------------------------------------------------

m4_define(`m4_build_represent_replacement',
         `constraint, congruence, generator')
m4_define(`m4_Grid_build_represent_replacement',
         `constraint, grid_generator, congruence')
m4_define(`m4_Pointset_Powerset_build_represent_replacement',
         `constraint, congruence')

dnl  The different kinds of alternative objects that can build
dnl  the same class.
dnl  At the moment, this is just used for the test data generator.
m4_define(`m4_build_represent_alt_replacement',
          `generator, congruence, constraint')
m4_define(`m4_Grid_build_represent_alt_replacement',
         `constraint, congruence, grid_generator')
m4_define(`m4_Pointset_Powerset_build_represent_alt_replacement',
         `constraint, congruence')

dnl ---------------------------------------------------------------------
dnl pattern == relation_represent
dnl  The different kinds of objects that can have a relation with a class.
dnl ---------------------------------------------------------------------

m4_define(`m4_relation_represent_replacement', `constraint, generator')
m4_define(`m4_Polyhedron_relation_represent_replacement',
         `m4_relation_represent_replacement, congruence')
m4_define(`m4_Grid_relation_represent_replacement',
         `m4_relation_represent_replacement, congruence, grid_generator')
m4_define(`m4_Pointset_Powerset_relation_represent_replacement',
         `m4_relation_represent_replacement, congruence')

dnl  The type of these relations with a class.
m4_define(`m4_relation_represent_alt_replacement', `con, gen')
m4_define(`m4_Polyhedron_relation_represent_alt_replacement',
         `con, gen, con')
m4_define(`m4_Grid_relation_represent_alt_replacement',
         `con, gen, con, gen')
m4_define(`m4_Pointset_Powerset_relation_represent_alt_replacement',
         `con, gen, con')

dnl  The different kinds of objects that can be added to a class.
m4_define(`m4_add_represent_replacement', `constraint, congruence')
m4_define(`m4_Polyhedron_add_represent_replacement',
         `m4_add_represent_replacement, generator')
m4_define(`m4_Grid_add_represent_replacement',
         `m4_add_represent_replacement, grid_generator')

dnl  The different kinds of objects that can be obtained from a
dnl  class description.
m4_define(`m4_get_represent_replacement', `constraint')
m4_define(`m4_Polyhedron_get_represent_replacement',
         `constraint, generator, congruence')
m4_define(`m4_Grid_get_represent_replacement',
         `congruence, grid_generator')

dnl  The recycling argument which is only needed for the Polyhedron
dnl  or Grid class.
m4_define(`m4_recycle_replacement', `')
m4_define(`m4_Polyhedron_recycle_replacement',
         `@COMMA@ Recycle_Input()')
m4_define(`m4_Grid_recycle_replacement',
         `@COMMA@ Recycle_Input()')

dnl  The "superclass" is the most general class for the disjunct kind.
dnl  For grids it is Grid and for all the other
dnl  simple classes, it is NNC_Polyhedron.
m4_define(`m4_superclass_replacement', `NONE')
m4_define(`m4_Pointset_Powerset_superclass_replacement',
          `m4_ifelse(
          m4_echo_unquoted(m4_disjunct_kind($1)),
          `Grid', `Grid', `NNC_Polyhedron')')

dnl  The "partition" which is currently only available for the Polyhedron
dnl  and Grid Pointset_Powerset classes.
m4_define(`m4_partition_replacement', `NONE')
m4_define(`m4_Pointset_Powerset_partition_replacement',
          `m4_ifelse(
          m4_echo_unquoted(m4_remove_topology(m4_disjunct_kind($1))),
          `Polyhedron', `linear_partition',
          m4_echo_unquoted(m4_remove_topology(m4_disjunct_kind($1))),
          `Grid', `approximate_partition')')

dnl  The unary "has_property" predicates
m4_define(`m4_has_property_replacement', `is_empty, is_universe, is_bounded, contains_integer_point, is_topologically_closed')
m4_define(`m4_Polyhedron_has_property_replacement',
          `m4_has_property_replacement, is_discrete')
m4_define(`m4_Grid_has_property_replacement',
          `m4_has_property_replacement, is_discrete')

dnl  The "simplify" predicates
m4_define(`m4_simplify_replacement', `topological_closure_assign')
m4_define(`m4_Pointset_Powerset_simplify_replacement',
          `m4_simplify_replacement, pairwise_reduce')

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
         `m4_comparison_replacement,
          geometrically_covers, geometrically_equals')

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
          `m4_ifelse(
          m4_echo_unquoted(m4_remove_topology(m4_disjunct_kind($1))), Polyhedron,
          `intersection_assign, poly_difference_assign, concatenate_assign,
           time_elapse_assign',
          `m4_body_class_kind($1)', Grid,
          `intersection_assign, poly_difference_assign, concatenate_assign,
           time_elapse_assign',
          `intersection_assign, concatenate_assign,
           time_elapse_assign')')

dnl  The different kinds of "and_minimize" binary operators.
m4_define(`m4_binminop_replacement', `intersection_assign_and_minimize')
m4_define(`m4_Polyhedron_binminop_replacement',
         `m4_binminop_replacement, poly_hull_assign_and_minimize')
m4_define(`m4_Grid_binminop_replacement',
         `m4_binminop_replacement, join_assign_and_minimize')

dnl  The different kinds of "upper_bound_if_exact" binary operators.
m4_define(`m4_ub_exact_replacement', `upper_bound_assign_if_exact')
m4_define(`m4_Polyhedron_ub_exact_replacement',
         `m4_ub_exact_replacement, poly_hull_assign_if_exact')
m4_define(`m4_Grid_ub_exact_replacement',
         `m4_ub_exact_replacement, join_assign_if_exact')
