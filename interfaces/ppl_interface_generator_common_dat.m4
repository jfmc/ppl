dnl Classes to be implemented and C++ versions of these classes.
include(ppl_interface_instantiations.m4)

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
all, shape, wr_shape, polyhedron, grid, bd_shape, octagonal_shape')

define(`m4_all_group', `Polyhedron, Grid, BD_Shape, Octagonal_Shape')
define(`m4_shape_group', `Polyhedron, BD_Shape, Octagonal_Shape')
define(`m4_wr_shape_group', `BD_Shape, Octagonal_Shape')
define(`m4_polyhedron_group', Polyhedron)
define(`m4_grid_group', Grid)
define(`m4_bd_shape_group', BD_Shape)
define(`m4_octagonal_shape_group', Octagonal_Shape)

dnl m4_pattern_list
dnl
dnl Returns a list of patterns (in lowercase) used for the generation
dnl of procedure names and code schemas.
define(`m4_pattern_list', `dnl
intopology,
topology,
represent,
dimension,
generator,
point,
constrainer,
state,
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

dnl The topology of the domain element. The default is the empty string.
define(`m4_topology_replacement', `')
define(`m4_Polyhedron_topology_replacement', ``C_, NNC_'')

dnl The topology used to copy from another element of the domain
define(`m4_intopology_replacement', `')
define(`m4_Polyhedron_intopology_replacement', ``C_, NNC_'')

dnl The widening and extrapolation operators.
define(`m4_widenexp_replacement', `')
define(`m4_Polyhedron_widenexp_replacement', ``BHRZ03, H79'')
define(`m4_Grid_widenexp_replacement', ``congruence, generator'')
define(`m4_BD_Shape_widenexp_replacement', ``BHMZ05, H79'')
define(`m4_Octagonal_Shape_widenexp_replacement', ``CH78'')

dnl The shape classes have bounding boxes while the grid classes also
dnl have covering boxes.
define(`m4_box_replacement', `bounding_box')
define(`m4_Grid_box_replacement', ```m4_box_replacement', covering_box'')
define(`m4_Grid_box_bounding_box_alt_replacement', ``shrink_bounding_box'')
define(`m4_Grid_box_covering_box_alt_replacement', ``get_covering_box'')

dnl  Space or affine dimensions
define(`m4_dimension_replacement', `space_dimension, affine_dimension')

dnl The different kinds of objects use to generate a class.
define(`m4_generator_replacement', `generator')
define(`m4_Grid_generator_replacement', ``grid_generator'')

dnl  The different kinds of points.
define(`m4_point_replacement', `point')
define(`m4_Grid_point_replacement', ``grid_point'')

dnl  The constrainer objects used to describe a class.
define(`m4_constrainer_replacement', `constraint')
define(`m4_Grid_constrainer_replacement', ``congruence'')

dnl  The different kinds of objects use to represent a class.
define(`m4_represent_replacement', `constraint')
define(`m4_Polyhedron_represent_replacement',
         ```m4_represent_replacement', generator'')
define(`m4_Grid_represent_replacement',
         ```m4_represent_replacement', grid_generator, congruence'')

dnl  The different kinds of objects use to describe a class.
define(`m4_describe_replacement', `constraint')
define(`m4_Polyhedron_describe_replacement',
         ```m4_describe_replacement', generator'')
define(`m4_Grid_describe_replacement',
         ``congruence, grid_generator'')

dnl  The "is" predicates
define(`m4_state_replacement', `empty, universe, bounded')
define(`m4_Polyhedron_state_replacement',
         ```m4_state_replacement', topologically_closed'')
define(`m4_Grid_state_replacement',
        ```m4_state_replacement', topologically_closed, discrete'')

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

dnl  The different kinds of binary operators.
define(`m4_binop_replacement',
         `intersection_assign, upper_bound_assign, difference_assign,
          concatenate_assign, time_elapse_assign')
define(`m4_Polyhedron_binop_replacement',
         ```m4_binop_replacement', poly_hull_assign, poly_difference_assign'')
define(`m4_Grid_binop_replacement',
         ```m4_binop_replacement', join_assign'')
define(`m4_BD_Shape_binop_replacement',
         ```m4_binop_replacement', bds_hull_assign'')
define(`m4_Octagonal_Shape_binop_replacement',
         ```m4_binop_replacement', oct_hull_assign'')

dnl  The different kinds of "and_minimize" binary operators.
define(`m4_binminop_replacement', `intersection_assign_and_minimize')
define(`m4_Polyhedron_binminop_replacement',
         ```m4_binminop_replacement', poly_hull_assign_and_minimize'')
define(`m4_Grid_binminops_replacement',
         ```m4_binminop_replacement', join_assign_and_minimize'')
