divert(-1)

# Classes to be implemented. This is the only line the user needs to edit.
#
define(`m4_classes', ``Polyhedron'')

# Number of possible classes. This needs to be updated as more
# classes are supported in the PPL.
#
define(`m4_num_possible_classes', 5)

# All possible classes. This needs to be updated as more
# classes are supported in the PPL.
#
define(`Class1', `Polyhedron')
define(`Class2', `LP_Problem')
define(`Class3', `Grid')
define(`Class4', `BD_Shape')
define(`Class5', `Octagon')

# class_group
#
# There are three class groups:
# LPM for LP_Problem class,
# GRD for all the grid-like classes,
# SHP for classes denoting subsets of a vector space that have some shape.
define(`m4_class_group', `ifelse(class, LP_Problem, LPM, class, Grid, GRD, SHP)')

# class_super_group
#
# There are two class super_groups:
# PM for LP_Problem class,
# PD for classes defining some Domain based on sets of Points
# (ie grid and shape classes)
define(`m4_class_super_group', `ifelse(m4_class_group, LPM, LPM, PTD)')

define(`m4_string_substitution_list',
`intopology_,
topology_,
represent,
dim,
gnrt,
point,
rstrct,
state,
abovebelow,
maxmin,
embedproject,
affim,
comparison,
binop,
bpmin,
widenexp,
box,
describe'))

# num_class_widenexps
# class_widenexp
#
# The widening and extrapolation operators.
define(`num_Polyhedron_widenexps', 2)
define(`Polyhedron_widenexp1', `BHRZ03')
define(`Polyhedron_widenexp2', `H79')
define(`num_Grid_widenexps', 2)
define(`Grid_widenexp1', `congruence')
define(`Grid_widenexp2', `generator')
#define(`Grid_widenexp3', `BDHMZ06')
define(`Grid_widenexp3', `')
define(`num_BD_Shape_widenexps',3)
define(`BD_Shape_widenexp1',`CC76')
define(`BD_Shape_widenexp2',`BHMZ05')
define(`BD_Shape_widenexp3',`H79')
define(`num_Octagon_widenexps',1)
define(`Octagon_widenexp1',CH78)

# num_class_topology_s
# class_topology_
#
# Some classes can have a topology. The "_" only needed when topology exists.
define(`num_topology_s', 1)
define(`topology_1', `')
define(`num_Polyhedron_topology_s', 2)
define(`Polyhedron_topology_1', `C_')
define(`Polyhedron_topology_2', `NNC_')
define(`num_intopology_s', 1)
define(`intopology_1', `')
define(`num_Polyhedron_intopology_s', 2)
define(`Polyhedron_intopology_1', `C_')
define(`Polyhedron_intopology_2', `NNC_')

# num_class_boxs
# class_box
#
# The shape classes have bounding boxes while the grid classes also
# have covering boxes.
define(`num_boxs', 1)
define(`box1', `bounding_box')
define(`num_Grid_boxs', 2)
define(`Grid_box2', `covering_box')
define(`alt_Grid_box1', `shrink_bounding_box')
define(`alt_Grid_box2', `get_covering_box')

# num_class_dims
# class_dim
#
#  space or affine dimensions
define(`num_dims', 2)
define(`dim1', `space_dimension')
define(`dim2', `affine_dimension')
define(`num_LP_Problem_dims', 1)

# num_class_gnrts
# class_gnrt
#
#  The different kinds of objects use to generate a class.
define(`num_gnrts', 1)
define(`gnrt1', `generator')
define(`alt_Grid_gnrt1', `grid_generator')

# num_class_points
# class_point
#
#  The different kinds of objects use to generate a class.
define(`num_points', 1)
define(`point1', `point')
define(`alt_Grid_point1', `grid_point')

# num_class_rstrcts
# class_rstrct
#
#  The constraint objects used to define a class.
define(`num_rstrcts', 1)
define(`rstrct1', `constraint')
define(`Grid_rstrct1', `congruence')

# num_class_represents
# class_represent?
#
#  The different kinds of objects use to construct a class.
define(`num_represents', 2)
define(`represent1', `constraint')
define(`represent2', `generator')
define(`num_Grid_represents', 3)
define(`Grid_represent2', `generator')
define(`alt_Grid_represent2', `grid_generator')
define(`Grid_represent3', `congruence')
define(`num_BD_Shape_represents', 1)
define(`num_Octagon_represents', 1)


# num_class_describes
# class_describe
#
#  The different kinds of objects use to describe a class.
define(`num_describes', 2)
define(`describe1', `constraint')
define(`describe2', `generator')
define(`num_Grid_describe', 2)
define(`Grid_describe1', `congruence')
define(`Grid_describe2', `generator')
define(`alt_Grid_describe2', `grid_generator')
define(`num_BD_Shape_describes', 1)
define(`num_Octagon_describes', 1)

# num_class_states
# class_State
#
#  the "is" predicates
define(`num_states', 4)
define(`state1', `empty')
define(`state2', `universe')
define(`state3', `bounded')
define(`state4', `topologically_closed')
define(`num_Grid_states', 5)
define(`Grid_state4', `topologically_closed')
define(`Grid_state5', `discrete')

# num_class_bounds
# class_bounds
#
#  above or below
define(`num_abovebelows', 2)
define(`abovebelow1', `above')
define(`abovebelow2', `below')

# num_class_maxmins
# class_maxmin
#
#  Maximize or Minimize
define(`num_maxmins', 2)
define(`maxmin1', `maximize')
define(`maxmin2', `minimize')

# num_class_embedprojects
# class_embedproject
#
#  Embed or project
define(`num_embedprojects', 2)
define(`embedproject1', `and_embed')
define(`embedproject2', `and_project')

# num_class_affIms
# class_affIm
#
#  affine_image or affine_preimage
define(`num_affims', 2)
define(`affim1', `affine_image')
define(`affim2', `affine_preimage')

# num_class_comparisons
# class_comparison
#
#  One object can be contained, strictly contained or disjoint in the other.
define(`num_comparisons', 3)
define(`comparison1', `contains')
define(`comparison2', `strictly_contains')
define(`comparison3', `is_disjoint_from')

# num_class_binops
# class_binop
#
#  The different kinds of binary operators.
define(`num_binops', 4)
define(`binop1', `intersection_assign')
define(`binop2', `join_assign')
define(`binop3', `difference_assign')
define(`binop4', `time_elapse_assign')
define(`Polyhedron_binop2', `poly_hull_assign')
define(`Polyhedron_binop3', `poly_difference_assign')
define(`num_BD_Shape_binops', 3)
define(`num_Octagon_binops', 3)

# num_class_bpmins
# class_bpmin
#
#  The different kinds of "and_minimize" binary operators.
define(`num_bpmins', 2)
define(`bpmin1', `binop1`'_and_minimize')
define(`Polyhedron_bpmin2',  `Polyhedron_binop2`'_and_minimize')
define(`bpmin2',  `binop2`'_and_minimize')

# Class predicate list using patterns wherever possible.
# Which classes the schema applies to is determined by the following codes:
# If code is PTD = the point-domain classes ie grid and polyhedra classes;
#            All = all classes
#            PS = the polyhedra-shape classes;
#            Po = the polyhedron class;
#            Gr = the Grid class;
#            Cc, where Cc is 1st two letters of class name Cccc
#               = the Cccc class.
# There is no limit on the number of codes.
# An "X" in front of the two letter short class name,
# means it is not included.
# Where "CLASS" is replaced by the class name, then that class only
# is applicable for that schema.
#
# Note that the code for the schema "<name>_code" must be defined
# in the ppl_prolog_icc.m4 file. The <name> must be exactly as written here.
#
define(`class_predicate_list',
`ppl_new_TOPOLOGY_CLASS_from_space_dimension/3 PTD
ppl_new_TOPOLOGY_CLASS_from_INTOPOLOGY_CLASS/2 All
ppl_new_TOPOLOGY_CLASS_from_UALT_REPRESENT_System/2 PTD
ppl_new_TOPOLOGY_CLASS_recycle_UALT_REPRESENT_System/2 PTD
ppl_new_TOPOLOGY_CLASS_from_BOX/2 PTD
ppl_delete_CLASS/1 All
ppl_assign_TOPOLOGY_CLASS_from_TOPOLOGY_CLASS/1 PTD
ppl_CLASS_DIM/2 All
ppl_CLASS_DESCRIBEs/2 PTD
ppl_CLASS_minimized_DESCRIBEs/2 PTD
ppl_CLASS_relation_with_UALT_DESCRIBE/3 PTD
ppl_CLASS_shrink_BOX/3 SHP
ppl_Grid_get_BOX/2
ppl_CLASS_is_STATE/1 PTD
ppl_CLASS_topological_closure_assign/1 PTD
ppl_CLASS_bounds_from_ABOVEBELOW/2 PTD
ppl_CLASS_MAXMIN/5 PTD
ppl_CLASS_MAXMIN_with_point/6 PTD
ppl_CLASS_COMPARISON_CLASS/2 PTD
ppl_CLASS_equals_CLASS/2 PTD
ppl_CLASS_OK/1 All
ppl_CLASS_add_REPRESENT/2 PTD
ppl_CLASS_add_REPRESENT_and_minimize/2 PTD
ppl_CLASS_add_REPRESENTs/2 PTD
ppl_CLASS_add_REPRESENTs_and_minimize/2 PTD
ppl_CLASS_add_recycled_REPRESENT/2 PTD
ppl_CLASS_add_recycled_REPRESENT_and_minimize/2 PTD
ppl_CLASS_add_recycled_REPRESENTs/2 PTD
ppl_CLASS_add_recycled_REPRESENTs_and_minimize/2 PTD
ppl_CLASS_BINOP/2 PTD
ppl_CLASS_BPMIN/2 PTD
ppl_CLASS_AFFIM/4 PTD
ppl_CLASS_bounded_AFFIM/5 Po
ppl_CLASS_generalized_AFFIM/5 SHP
ppl_CLASS_generalized_AFFIM_lhs_rhs/4 SHP
ppl_Grid_generalized_AFFIM/5
ppl_Grid_generalized_AFFIM_lhs_rhs/4
ppl_CLASS_WIDENEXP_widening_assign_with_tokens/4 PTD
ppl_CLASS_WIDENEXP_widening_assign/2 PTD
ppl_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens/5 PTD
ppl_CLASS_limited_WIDENEXP_extrapolation_assign/3 PTD
ppl_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens/5 Po
ppl_CLASS_bounded_WIDENEXP_extrapolation_assign/3 Po
ppl_BD_Shape_CC76_narrowing_assign/2
ppl_CLASS_add_space_dimensions_and_EMBEDPROJECT/2 PTD
ppl_CLASS_concatenate_assign/2 PTD
ppl_CLASS_remove_space_dimensions/2 PTD
ppl_CLASS_remove_higher_space_dimensions/2 PTD
ppl_CLASS_expand_space_dimension/3 PTD
ppl_CLASS_fold_space_dimensions/3 PTD
ppl_CLASS_map_space_dimensions/2 PTD
ppl_new_LP_Problem_trivial/1
ppl_new_LP_Problem/4
ppl_LP_Problem_constraints/2
ppl_LP_Problem_objective_function/2
ppl_LP_Problem_optimization_mode/2
ppl_LP_Problem_clear/1
ppl_LP_Problem_add_constraint/2
ppl_LP_Problem_add_constraints/2
ppl_LP_Problem_set_objective_function/2
ppl_LP_Problem_set_optimization_mode/2
ppl_LP_Problem_is_satisfiable/1
ppl_LP_Problem_solve/2
ppl_LP_Problem_feasible_point/2
ppl_LP_Problem_optimizing_point/2
ppl_LP_Problem_optimal_value/3
ppl_LP_Problem_evaluate_objective_function/4'
)
divert`'dnl

