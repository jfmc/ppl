m4_divert(-1)

dnl This m4 file contains the program code for generating Prolog_interface.dox

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

m4_define(`m4_comparison_alt_replacement',
         `is included in or equal to,  is included in but not equal to,
         is disjoint from')
m4_define(`m4_Pointset_Powerset_comparison_alt_replacement',
         `m4_comparison_replacement,
          geometrically covers, geometrically equals')

m4_define(`m4_has_property_alt_replacement', `is empty, is the universe,
            is bounded, contains an integer point, is topologically closed,
            is discrete')
m4_define(`m4_product_has_property_alt_replacement', `dnl
  is empty, is the universe, is bounded, is topologically closed, is discrete')

m4_define(`m4_Polyhedron_topology_replacement', `C_')
m4_define(`m4_Polyhedron_topology_alt_replacement', `C ')

m4_define(`m4_dimension_alt_replacement',
  `dimension of the vector space enclosing,
  affine dimension of`'dnl
')

m4_define(`m4_simplify_alt_replacement', `topological closure')
m4_define(`m4_Pointset_Powerset_simplify_alt_replacement', `dnl
topological closure, pairwise reduction, omega reduction')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_build_doc', `dnl
<P><CODE>ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(+Dimension_Type, +Universe_or_Empty, -Handle)</CODE><BR>
   <EM>Builds a @ALT_TOPOLOGY@@LCLASS@ \f$\cP\f$
   with \c Dimension_Type dimensions; it is empty
   or the universe depending on whether \c Atom
   is \c empty or \c universe@COMMA@ respectively.
   \c Handle is unified with the handle for \f$\cP\f$.
   Thus the query
\code
   ?- ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(3@COMMA@ universe@COMMA@ X).
\endcode
   creates the @ALT_TOPOLOGY@@LCLASS@ defining the
   3-dimensional vector space
   \f$\Rset^3\f$ with \c X bound to a valid handle for accessing it.</EM>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_build_doc',
`dnl
<P><CODE>ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(+@UBUILD_REPRESENT@, -Handle)</CODE><BR>
  <EM>Builds a new @ALT_TOPOLOGY@@LCLASS@ \p P from
  the system of constraints \c @UBUILD_REPRESENT@.
  \c Handle is unified with the handle for \p P.</EM>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_convert_doc', `dnl
m4_pushdef(`m4_friend_replacement', `C_Polyhedron, NNC_Polyhedron')
m4_pushdef(`m4_friend_alt_replacement', `a C polyhedron, an NNC polyhedron')
<P><CODE>ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@(+Handle_1, -Handle_2)</CODE><BR>
  <EM>Builds a new @ALT_TOPOLOGY@@LCLASS@ \p P_1 from
  \c @ALT_FRIEND@ referenced by handle \c Handle_1.
  \c Handle_2 is unified with the handle for \p P_1.</EM>
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_with_complexity_convert_doc', `dnl
m4_pushdef(`m4_friend_replacement', `C_Polyhedron, NNC_Polyhedron')
m4_pushdef(`m4_friend_alt_replacement', `a C polyhedron, an NNC polyhedron')
<P><CODE>ppl_new_@ALT_TOPOLOGY@@LCLASS@_from_@FRIEND@_with_complexity(+Handle, +Complexity, -Handle)</CODE><BR>
  <EM>Builds a new @ALT_TOPOLOGY@@LCLASS@ \p P_1 from
  \c @ALT_FRIEND@ referenced by handle \c Handle_1.
  \c Handle_2 is unified with the handle for \p P_1.</EM>
')

m4_define(`ppl_delete_@CLASS@_destruct_doc',
`dnl
<P><CODE>ppl_delete_@CLASS@(+Handle)</CODE><BR>
  <EM>Invalidates the handle  referenced by \c Handle:
  this makes sure the corresponding resources will eventually be released.</EM>
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_@HAS_PROPERTY@(+Handle)</CODE><BR>
   <EM>Succeeds if and only if the @LCLASS@ referenced by
   \c Handle @ALT_HAS_PROPERTY@.</EM>
')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_bounds_from_@ABOVEBELOW@(+Handle, +Lin_Expr)</CODE><BR>
   <EM>Succeeds if and only if <CODE>Lin_Expr</CODE> is bounded from @ABOVEBELOW@
   in the @LCLASS@ referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_@COMPARISON@_@CLASS@(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Succeeds if and only if the @LCLASS@ referenced by
   <CODE>Handle_2</CODE> @ALT_COMPARISON@
   the @LCLASS@ referenced by <CODE>Handle_1</CODE>.</EM>
')

m4_define(`ppl_@CLASS@_equals_@CLASS@_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_equals_@CLASS@(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Succeeds if and only if the @LCLASS@ referenced by
   <CODE>Handle_1</CODE> is
   equal to the @LCLASS@ referenced by <CODE>Handle_2</CODE>.</EM>
')

m4_define(`ppl_@CLASS@_constrains_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_constrains(+Handle, +PPL_Var)</CODE><BR>
  <EM>Succeeds if and only if the @LCLASS@ referenced by
   \c Handle constrains the dimension \c PPL_Var.</EM>
')

m4_define(`ppl_@CLASS@_OK_testpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_OK(+Handle)</CODE><BR>
   <EM>Succeeds only if the @LCLASS@ referenced by
   \c Handle is well formed@COMMA@ i.e.@COMMA@ if it
   satisfies all its implementation invariants.
   Useful for debugging purposes.</EM>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_constpoly_doc',
`dnl
m4_pushdef(`m4_maxmin_replacement', `max, min')
m4_pushdef(`m4_maxmin_alt_replacement', `above, below')
m4_pushdef(`m4_maxmin_cppx_replacement', `supremum, infinum')
<P><CODE>ppl_@CLASS@_@MAXMIN@imize(+Handle, +Lin_Expr, ?Coeff_1, ?Coeff_2, ?Boolean)</CODE><BR>
  <EM>Succeeds if and only if @LCLASS@ \p P referenced by \c Handle
  is not empty and \c Lin_Expr is bounded from @ALT_MAXMIN@ in \p P.

  \c Coeff_1 is unified with the numerator of the @CPPX_MAXMIN@ value
  and \c Coeff_2 with the denominator of the @CPPX_MAXMIN@ value.
  If the @CPPX_MAXMIN@ is also the @MAXMIN@imum@COMMA@ \c Boolean is unified
  with the atom \c true and@COMMA@ otherwise@COMMA@ unified with
  the atom \c false.</EM>
')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_constpoly_doc',
`dnl
m4_pushdef(`m4_maxmin_replacement', `max, min')
m4_pushdef(`m4_maxmin_alt_replacement', `above, below')
m4_pushdef(`m4_maxmin_cppx_replacement', `supremum, infinum')
<P><CODE>ppl_@CLASS@_@MAXMIN@imize_with_point(+Handle, +Lin_Expr, ?Coeff_1, ?Coeff_2, ?Boolean, ?Point)</CODE><BR>
  <EM>Succeeds if and only if @LCLASS@ \p P referenced by \c Handle
  is not empty and \c Lin_Expr is bounded from @ALT_MAXMIN@ in \p P.

  \c Coeff_1 is unified with the numerator of the @CPPX_MAXMIN@ value
  and \c Coeff_2 with the denominator of the @CPPX_MAXMIN@ value
  and \c Point with a point or closure point where \c Lin_Expr reaches
  this value.
  If the @CPPX_MAXMIN@ is also the @MAXMIN@imum@COMMA@ \c Boolean is unified
  with the atom \c true and@COMMA@ otherwise@COMMA@ unified with the
  atom \c false.</EM>
')

m4_define(`ppl_@CLASS@_@DIMENSION@_constpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_@DIMENSION@(+Handle, ?Dimension_Type)</CODE><BR>
   <EM>Unifies \c Dimension_Type with the @ALT_DIMENSION@
   of the @LCLASS@ referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_constpoly_doc',
`dnl
<P><CODE>ppl_@CLASS@_relation_with_@RELATION_REPRESENT@(+Handle, +@URELATION_REPRESENT@, ?Relation_List)</CODE><BR>
  <EM>Unifies \c Relation_List with the list of relations the
  @LCLASS@ referenced by \c Handle has with \c @URELATION_REPRESENT@.
  The possible relations are listed in the
  grammar rules above.</EM>
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_constpoly_doc',
`dnl
m4_pushdef(`m4_Polyhedron_get_represent_alt_replacement',
         `satisfied by, for, satisfied by')
<P><CODE>ppl_@CLASS@_get_@GET_REPRESENT@s(+Handle, ?@UGET_REPRESENT@_System)</CODE><BR>
   <EM>Unifies \c @UGET_REPRESENT@_System with
   the @GET_REPRESENT@s (in the form of a list) in the @GET_REPRESENT@ system
   @ALT_GET_REPRESENT@ the @LCLASS@ referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_constpoly_doc',
`dnl
m4_pushdef(`m4_Polyhedron_get_represent_alt_replacement',
         `satisfied by, for, satisfied by')
<P><CODE>ppl_@CLASS@_get_minimized_@GET_REPRESENT@s(+Handle, ?@UGET_REPRESENT@_System)</CODE><BR>
   <EM>Unifies \c @UGET_REPRESENT@_System with
   the @GET_REPRESENT@s (in the form of a list)
   in the minimized @GET_REPRESENT@ system
   @ALT_GET_REPRESENT@ the @LCLASS@ referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_ascii_dump_dump_doc',
 `dnl
<P><CODE>ppl_@CLASS@_ascii_dump(+Handle)</CODE><BR>
  <EM>Dumps an ascii representation of the PPL internal state for
  @LCLASS@ referenced by \c Handle on the standard output.
  Useful for debugging.</EM>
')

m4_define(`ppl_@CLASS@_@MEMBYTES@_constpoly_doc',
 `dnl
m4_pushdef(`m4_membytes_alt_replacement', `total, external')
<P><CODE>ppl_@CLASS@_@MEMBYTES@(+Handle, ?Number)</CODE><BR>
<EM>Unifies \c Number with the size of the @ALT_MEMBYTES@ memory in
bytes occupied by the @LCLASS@ referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_addto_doc',
`dnl
m4_pushdef(`m4_add_represent_alt_replacement',
           `an equality or a non-strict inequality,
            an equality, a line@COMMA@ ray or point')`'dnl
<P><CODE>ppl_@CLASS@_add_@ADD_REPRESENT@(+Handle, +@UADD_REPRESENT@)</CODE><BR>
   <EM>Updates the @LCLASS@ referenced by \c Handle to
   one obtained by adding
   \c @UADD_REPRESENT@ to its @ADD_REPRESENT@ system.
   For a C polyhedron@COMMA@ \c @UADD_REPRESENT@ must be
   @ALT_ADD_REPRESENT@.</EM>
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_addto_doc',
`dnl
m4_popdef(`m4_add_represent_alt_replacement')`'dnl
m4_pushdef(`m4_add_represent_alt_replacement',
           `equalities and non-strict inequalities,
            equalities, lines@COMMA@ rays and points')`'dnl
<P><CODE>ppl_@CLASS@_add_@ADD_REPRESENT@s(
                  +Handle,
                  +@UADD_REPRESENT@_System)</CODE><BR>
   <EM>Updates the @LCLASS@ referenced by \c Handle to
   one obtained by adding to its @ADD_REPRESENT@ system the @ADD_REPRESENT@s in
   \c @UADD_REPRESENT@_System.
   For a C polyhedron@COMMA@ \c @UADD_REPRESENT@s must be a list of
   @ALT_ADD_REPRESENT@.</EM>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@_addto_doc',
`dnl
<P><CODE>ppl_@CLASS@_refine_with_@REFINE_REPRESENT@(
                  +Handle,
                  +@UREFINE_REPRESENT@)</CODE><BR>
   <EM>Updates the @LCLASS@ referenced by \c Handle to
   one obtained by refining its @REFINE_REPRESENT@ system with
   \c @UREFINE_REPRESENT@.</EM>
')

m4_define(`ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s_addto_doc',
`dnl
<P><CODE>ppl_@CLASS@_refine_with_@REFINE_REPRESENT@s(
                  +Handle,
                  +@UREFINE_REPRESENT@_System)</CODE><BR>
   <EM>Updates the @LCLASS@ referenced by \c Handle to
   one obtained by refining its @REFINE_REPRESENT@ system with
   the @REFINE_REPRESENT@s in \c @UREFINE_REPRESENT@_System.</EM>
')


m4_define(`ppl_@CLASS@_@BINOP@_binop_doc',
`dnl
m4_pushdef(`m4_binop_replacement',
         `intersection_assign, upper_bound_assign, difference_assign,
          time_elapse_assign')
m4_pushdef(`m4_Polyhedron_binop_replacement',
         `m4_binop_replacement, poly_hull, poly_difference')
m4_pushdef(`m4_binop_alt_replacement',
         `intersection, upper bound, difference,
          time elapse')
m4_pushdef(`m4_Polyhedron_binop_alt_replacement',
         `m4_binop_alt_replacement, poly-hull, poly-difference')
<P><CODE>ppl_@CLASS@_@BINOP@(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P referenced by \c Handle_1
   the @ALT_BINOP@ of \p P and the @LCLASS@ referenced by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_@UB_EXACT@_binop_doc',
`dnl
<P><CODE>ppl_@CLASS@_@UB_EXACT@(+Handle_1, +Handle_2)</CODE><BR>
    <EM>Succeeds if the least upper bound of the
    @LCLASS@ \p P_1 referenced by \c Handle_1
    with the @LCLASS@ referenced by \c Handle_2 is exact;
    in which case the least upper bound is assigned
    to \p P_1;
    fails otherwise.</EM>
')

m4_define(`ppl_@CLASS@_linear_@PARTITION@_binop_doc',
 `dnl
<P><CODE>ppl_@CLASS@_linear_@PARTITION@(+Handle_1, +Handle_2, -Handle_3,
                                        -Handle_4)</CODE><BR>
  <EM>\c Handle_1 and \c Handle_2 are handles for elements \p P_1 and \p P_2
  in the @CLASS@ domain. The predicate unifies handle
  \c Handle_3 to a reference to the intersection of
  \p P_1 and \p P_2 and \c Handle_4 to a reference to
  a pointset powerset of nnc polyhedra \p PS;
  where \p PS is the linear partition of \p P_1 with respect to \p P_2.
  This predicate is only provided if the
  class \c Pointset_Powerset_NNC_Polyhedron
  has been enabled when configuring the library.</EM>
')

m4_define(`ppl_@CLASS@_approximate_@PARTITION@_binop_doc',
 `dnl
<P><CODE>ppl_@CLASS@_approximate_@PARTITION@(+Handle_1, +Handle_2, ?Boolean,
                                             -Handle_3, -Handle_4)</CODE><BR>
  <EM>\c Handle_1 and \c Handle_2 are handles for elements \p P_1 and \p P_2
  in the @CLASS@ domain. The predicate unifies handle
  \c Handle_3 to a reference to the intersection of
  \p P_1 and \p P_2 and \c Handle_4 to a reference to
  a pointset powerset of grids \p P_4 where:
  - if there is a finite linear partition of \p P_1 wrt \p P_2
   \c Boolean is unified with the atom \c true and
   \p P_4 is the linear partition.
  - otherwise \c Boolean is unified with the atom \c false
    and \p P_4 is set to the singleton set that contains \p P_2.</EM>
')

m4_define(`ppl_@CLASS@_simplify_using_context_assign_binop_doc',
`dnl
<P><CODE>ppl_@CLASS@_simplify_using_context_assign(+Handle_1, +Handle_2, ?Boolean)</CODE><BR>
    <EM>Succeeds if and only if the intersection of @LCLASS@ \p P_1
    referenced by \c Handle_1 and the @LCLASS@ \p P_2 referenced by \c Handle_2
    is non-empty.
    Assigns to \p P_1 its meet-preserving simplification with
    respect to \p P_2.</EM>
')

m4_define(`ppl_@CLASS@_widening_assign_with_tokens_widen_doc',
`dnl
<P><CODE>ppl_@CLASS@_widening_assign_with_tokens(+Handle_1, +Handle_2, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
<EM>Same as the predicate</EM>
<CODE>ppl_@CLASS@_H79_widening_assign_with_tokens/4</CODE>
')

m4_define(`ppl_@CLASS@_widening_assign_widen_doc',
`dnl
<P><CODE>ppl_@CLASS@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
<EM>Same as the predicate</EM>
<CODE>ppl_@CLASS@_H79_widening_assign</CODE>/2
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_widen_doc',
`dnl
<P><CODE>ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens(+Handle_1, +Handle_2, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P_1 referenced by \c Handle_1
   the @WIDEN@-widening of \p P_1 with the @LCLASS@
   referenced by \c Handle_2.
   The widening with tokens delay
   technique is applied with \c C_unsigned_1 tokens;
   \c C_unsigned_2 is unified with the number of tokens
   remaining at the end of the operation.</EM>
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_widen_doc',
`dnl
<P><CODE>ppl_@CLASS@_@WIDEN@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P_1 referenced by \c Handle_1
   the @WIDEN@-widening of \p P_1 with the @LCLASS@
   referenced by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_widen_doc',
`dnl
m4_pushdef(`m4_Polyhedron_limitedbounded_alt_replacement', `,
   @COMMA@ further intersected with the smallest box containing \p P_1.')
<P><CODE>ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens(+Handle_1, +Handle_2, +Constraint_System, +C_unsigned_1, ?C_unsigned_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P_1 referenced by \c Handle_1
   the @WIDENEXPN@-widening of \p P_1 with the @LCLASS@
   referenced by \c Handle_2 intersected with the constraints in
   \c Constraint_System that are
   satisfied by all the points of \p P_1@ALT_LIMITEDBOUNDED@.
   The widening with tokens delay
   technique is applied with \c C_unsigned_1 tokens;
   \c C_unsigned_2 is unified with the number of tokens
   remaining at the end of the operation.</EM>
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_widen_doc',
`dnl
m4_pushdef(`m4_Polyhedron_limitedbounded_alt_replacement', `,
   @COMMA@ further intersected with the smallest box containing \p P_1.')
<P><CODE>ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(+Handle_1, +Handle_2, +Constraint_System)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P_1 referenced by \c Handle_1
   the @WIDENEXPN@-widening of \p P_1 with the @LCLASS@
   referenced by \c Handle_2 intersected with the constraints in
   \c Constraint_System that are
   satisfied by all the points of \p P_1@ALT_LIMITEDBOUNDED@.</EM>
')

 m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_widen_doc',
 `dnl
<P><CODE>ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P_1 referenced by \c Handle_1
   the @EXTRAPOLATION@-narrowing of \p P_1 and the @LCLASS@
   referenced by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_swap_build_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_swap(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Swaps the @LCLASS@ referenced by \c Handle_1
   with the @LCLASS@ referenced by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_@SIMPLIFY@(+Handle)</CODE><BR>
  <EM>Assigns to the @LCLASS@ referenced by \c Handle
  its @ALT_SIMPLIFY@.</EM>
')

m4_define(`m4_affimage_alt_replacement',
  `image, preimage')
m4_define(`m4_affimage_cppx_replacement',
  `assigning the affine, substituting the affine')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_@AFFIMAGE@(+Handle, +PPL_Var, +Lin_Expr, +Coeff)</CODE><BR>
   <EM>Transforms the polyhedron referenced by \c Handle
   @CPPX_AFFIMAGE@ expression for \c Lin_Expr/\c Coeff \r to \c PPL_Var.</EM>
')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_bounded_@AFFIMAGE@(+Handle, +PPL_Var, +Lin_Expr_1, +Lin_Expr_2, +Coeff)</CODE><BR>
  <EM>Assigns to @LCLASS@ \p P referenced by \c Handle
  the generalized @ALT_AFFIMAGE@ with respect to the
  generalized affine transfer relation
  \c Lin_Expr_1/Coeff
    \f$\leq\f$ \c PPL_Var
      \f$\leq\f$ \c Lin_Expr_2/Coeff.</EM>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_generalized_@AFFIMAGE@(+Handle, +PPL_Var, +Relation_Symbol, +Lin_Expr, +Coeff)</CODE><BR>
  <EM>Assigns to @LCLASS@ \p P referenced by \c Handle
  the generalized @ALT_AFFIMAGE@ with respect to the
  generalized affine transfer relation
   \c PPL_Var \f$\bowtie\f$ \c Lin_Expr/\c Coeff@COMMA@
   where \f$\bowtie\f$ is the symbol
   represented by \c Relation_Symbol.</EM>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs(+Handle, +Lin_Expr_1, +Relation_Symbol, +Lin_Expr_2)</CODE><BR>
  <EM>Assigns to @LCLASS@ \p P referenced by \c Handle
  the generalized @ALT_AFFIMAGE@ with respect to the
  generalized affine transfer relation
   \c Lin_Expr_1 \f$\bowtie\f$ \c Lin_Expr_2@COMMA@
   where \f$\bowtie\f$ is the symbol
   represented by \c Relation_Symbol.</EM>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence_trans_doc',
 `dnl
<P><CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_with_congruence(+Handle, +PPL_Var, +Relation_Symbol, +Lin_Expr, +Coeff_1, +Coeff_2)</CODE><BR>
  <EM>Assigns to @LCLASS@ \p P referenced by \c Handle
  the generalized @ALT_AFFIMAGE@ with respect to the
  generalized affine transfer relation
   \c Lin_Expr_1 \p \equiv_f \c Lin_Expr_2/\c Coeff_1@COMMA@
   where \c Coeff_2 is bound to the modulus \p f.</EM>
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_with_congruence(+Handle, +Lin_Expr_1, +Relation_Symbol, +Lin_Expr_2, +Coeff)</CODE><BR>
  <EM>Assigns to @LCLASS@ \p P referenced by \c Handle
  the generalized @ALT_AFFIMAGE@ with respect to the
  generalized affine transfer relation
   \c Lin_Expr_1  \p \equiv_f \c Lin_Expr_2.
   where \c Coeff is bound to the modulus \p f.</EM>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_unconstrain_space_dimension(+Handle, +PPL_Var)</CODE><BR>
  <EM>Modifies the @LCLASS@ \p P referenced by \c Handle by unconstraining
  the space dimension \c PPL_Var.</EM>
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_trans_doc',
`dnl
<P><CODE>ppl_@CLASS@_unconstrain_space_dimensions(+Handle, +List_of_PPL_Var)</CODE><BR>
  <EM>Modifies the @LCLASS@ \p P referenced by \c Handle by unconstraining
  the space dimensions that are specified in \c List_of_PPL_Var.
  The presence of duplicates in \c List_of_PPL_Var is a waste
  but an innocuous one.</EM>
')
m4_define(`ppl_@CLASS@_@BINOP@_varspace_doc',
`dnl
m4_popdef(`m4_binop_replacement')
m4_popdef(`m4_Polyhedron_binop_replacement')
m4_popdef(`m4_binop_alt_replacement')
m4_popdef(`m4_Polyhedron_binop_alt_replacement')
m4_pushdef(`m4_binop_replacement', `concatenate_assign')
m4_pushdef(`m4_Polyhedron_binop_replacement', `concatenate_assign')
m4_pushdef(`m4_binop_alt_replacement', `concatenation')
m4_pushdef(`m4_Polyhedron_binop_alt_replacement', `concatenation')
 <P><CODE>ppl_@CLASS@_@BINOP@(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Assigns to the @LCLASS@ \p P referenced by \c Handle_1
   the @ALT_BINOP@ of \p P and the @LCLASS@ referenced by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@(+Handle, +Dimension_Type)</CODE><BR>
  <EM>Adds \c Dimension_Type new dimensions to the space enclosing
  the @LCLASS@ \p P referenced by \c Handle
  and @EMBEDPROJECT@s \p P in this space.</EM>
')

m4_define(`ppl_@CLASS@_remove_space_dimensions_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_remove_space_dimensions(+Handle, +List_of_PPL_Vars)</CODE><BR>
  <EM>Removes from the vector space enclosing
  the @LCLASS@ \p P referenced by \c Handle the space dimensions that
  are specified in \c List_of_PPL_Var.  The presence
  of duplicates in  \c List_of_PPL_Var is a waste but an innocuous one.</EM>
')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_remove_higher_space_dimensions(+Handle, +Dimension_Type)</CODE><BR>
  <EM>Removes the higher dimensions from the vector space enclosing
  the @LCLASS@ \p P referenced by \c Handle
  so that@COMMA@ upon successful return@COMMA@ the new space dimension is
  \c Dimension_Type.</EM>
')

m4_define(`ppl_@CLASS@_expand_space_dimension_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_expand_space_dimension(+Handle, +PPL_Var, +Dimension_Type)</CODE><BR>
  <EM>\extref{expand_space_dimension@COMMA@ Expands} the \c  PPL_Var-th
  dimension of the vector space enclosing
  the @LCLASS@ referenced by \c Handle to
  \c Dimension_Type new space dimensions.</EM>
')

m4_define(`ppl_@CLASS@_fold_space_dimensions_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_fold_space_dimensions(+Handle, +List_of_PPL_Vars, +PPL_Var)</CODE><BR>
  <EM>Modifies the @LCLASS@ referenced by \c Handle
  by \extref{fold_space_dimensions@COMMA@ folding} the
  space dimensions contained in \c List_of_PPL_Vars
  into dimension \c PPL_Var.
  The presence of duplicates in \c List_of_PPL_Vars is a waste
  but an innocuous one.</EM>
')

m4_define(`ppl_@CLASS@_map_space_dimensions_varspace_doc',
`dnl
<P><CODE>ppl_@CLASS@_map_space_dimensions(+Handle, +P_Func)</CODE><BR>
  <EM>Remaps the dimensions of the vector space according to a
  \extref{Mapping_the_Dimensions_of_the_Vector_Space@COMMA@ partial function}.
  This function is specified by means of the \c P_Func@COMMA@
  which has \p n entries.
  The result is undefined if \c P_Func does not encode a partial
  function.</EM>
')

m4_define(`ppl_new_@CLASS@_iterator_from_iterator_pps_iter_doc',
`dnl
<P><CODE>ppl_new_@CLASS@_iterator_from_iterator_pps_iter_doc(+Iterator_1, -Iterator_2)</CODE><BR>
  <EM>Builds a new iterator \p it from the iterator referenced by
  \c Iterator_1. <CODE>Iterator_2</CODE> is unified with the handle
  for \p it.</EM>
')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_pps_change_doc',
`dnl
m4_pushdef(`m4_Pointset_Powerset_simplify_replacement',
  `pairwise_reduce, omega_reduce')
m4_pushdef(`m4_Pointset_Powerset_simplify_alt_replacement',
  `pairwise reduction, omega reduction')
<P><CODE>ppl_@CLASS@_@SIMPLIFY@(+Handle)</CODE><BR>
   <EM>Assigns the result of @ALT_SIMPLIFY@ on
   the pointset powerset referenced by \c Handle.</EM>
')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_pps_iter_doc',
 `dnl
<P><CODE>ppl_@CLASS@_iterator_equals_iterator(+Iterator_1, +Iterator_2)</CODE><BR>
   <EM>Succeeds if and only if the iterator referenced by
   \c Iterator_1 is equal to the iterator referenced by
   \c Iterator_2.</EM>
')

 m4_define(`ppl_@CLASS@_@BEGINEND@_iterator_pps_iter_doc',
 `dnl
m4_pushdef(`m4_Pointset_Powerset_beginend_alt_replacement',
  `beginning, end')
<P><CODE>ppl_@CLASS@_@BEGINEND@_iterator(+Handle, -Iterator)</CODE><BR>
  <EM>Unifies \c Iterator with a handle to an iterator "pointing"
  to the @ALT_BEGINEND@ of
  the sequence of disjuncts of the powerset referred to by \c Handle.</EM>
')

m4_define(`ppl_delete_@CLASS@_iterator_pps_iter_doc',
 `dnl
<P><CODE>ppl_delete_@CLASS@_iterator(+Iterator)</CODE><BR>
  <EM>Invalidates the handle referenced by <CODE>Iterator</CODE>:
  this makes sure the corresponding resources
  will eventually be released.</EM>
')

m4_define(`ppl_@CLASS@_@INCDEC@_iterator_pps_iter_doc',
 `dnl
m4_pushdef(`m4_Pointset_Powerset_incdec_alt_replacement',
  `next, previous')
<P><CODE>ppl_@CLASS@_iterator_@INCDEC@(+Iterator)</CODE><BR>
  <EM>@UINCDEC@s the iterator referenced by \c Iterator
  so that it "points" to the @ALT_INCDEC@ disjunct.</EM>
')

m4_define(`ppl_@CLASS@_get_disjunct_pps_iter_doc',
 `dnl
<P><CODE>ppl_@CLASS@_iterator_get_disjunct(+Iterator, -Handle)</CODE><BR>
  <EM>Unifies with \c Handle a reference to the disjunct referred
  to by \c Iterator_1.</EM>
')

m4_define(`ppl_@CLASS@_drop_disjunct_pps_change_doc',
 `dnl
<P><CODE>ppl_@CLASS@_drop_disjunct(+Handle, +Iterator)</CODE><BR>
  <EM>If \p it is the iterator referred to by \c Iterator@COMMA@
  drops from the pointset powerset referenced by \c Handle
  the disjunct pointed to by \p it and
  assigns to \p it an iterator to the next disjunct.</EM>
')

m4_define(`ppl_@CLASS@_drop_disjuncts_pps_change_doc',
 `dnl
<P><CODE>ppl_@CLASS@_drop_disjuncts(+Handle, +Iterator_1, +Iterator_2)</CODE><BR>
  <EM>If \p it_1 and \p it_2 are the iterators referred to by \c Iterator_1
  and \c Iterator_2@COMMA@ respectively@COMMA@
  drops from the pointset powerset referenced by \c Handle
   all the disjuncts from \p it_1 to \p it_2 (excluded).</EM>
')

m4_define(`ppl_@CLASS@_add_disjunct_pps_change_doc',
 `dnl
<P><CODE>ppl_@CLASS@_add_disjunct(+Handle_1, +Handle_2)</CODE><BR>
  <EM>Adds to the pointset powerset referenced by \c Handle_1 a disjunct
  referred to by \c Handle_2.</EM>
')

m4_define(`ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign_pps_change_doc',
 `dnl
<P><CODE>ppl_@CLASS@_BHZ03_@ALT_DISJUNCT_WIDEN@_@DISJUNCT_WIDEN@_widening_assign(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Assigns to the pointset powerset \p P_1 referenced by
   \c Handle_1 the \extref{pps_certificate_widening@COMMA@ BHZ03-widening}
    between \p P_1 and the pointset powerset referenced by
   \c Handle_2@COMMA@ using the @DISJUNCT_WIDEN@-widening
    certified by the convergence certificate for @ALT_DISJUNCT_WIDEN@.</EM>
')

m4_define(`ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign_pps_change_doc',
 `dnl
<P><CODE>ppl_@CLASS@_BGP99_@DISJUNCT_WIDEN@_extrapolation_assign(+Handle_1, +Handle_2, C_unsigned)</CODE><BR>
    <EM>Assigns to the pointset powerset \p P_1 referenced by
   \c Handle_1 the result of applying the
    \extref{pps_bgp99_extrapolation@COMMA@ BGP99 extrapolation operator}
    between \p P_1 and the pointset powerset referenced by
   \c Handle_2@COMMA@ using the @DISJUNCT_WIDEN@-widening
    and the cardinality threshold \c C_unsigned.</EM>
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_pps_change_doc',
`dnl
m4_pushdef(`m4_Pointset_Powerset_comparison_replacement',
         `geometrically_covers, geometrically_equals')
m4_pushdef(`m4_Pointset_Powerset_comparison_alt_replacement',
         `geometrically covers, geometrically equals')
<P><CODE>ppl_@CLASS@_@COMPARISON@_@CLASS@(+Handle_1, +Handle_2)</CODE><BR>
   <EM>Succeeds if and only if the pointset powerset referenced by
   \c Handle_2 @ALT_COMPARISON@
   the pointset powerset referenced by \c Handle_1.</EM>
')
