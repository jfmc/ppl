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
m4_divert(-1)

m4_define(`m4_add_init_class_code', `dnl
let @LTOPOLOGY@@LCLASS@01
        = ppl_new_@TOPOLOGY@@CLASS@_from_constraints constraints01;;
let @LTOPOLOGY@@LCLASS@02
        = ppl_new_@TOPOLOGY@@CLASS@_from_constraints constraints02;;
print_newline();;
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`
print_string "testing ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension" ;;
print_newline();;
print_string ("space_dimension: ");;
for i = 6 downto 0 do
  (let @LTOPOLOGY@@LCLASS@
        = ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension i Empty
        in let dimension =  ppl_@TOPOLOGY@@CLASS@_space_dimension(@LTOPOLOGY@@LCLASS@)
         in printf "%d, " dimension)
done;;
print_newline();;

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`
print_string "testing ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s" ;;
print_newline();;
let _@LTOPOLOGY@@LCLASS@
  = ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(@BUILD_REPRESENT@s1);;
')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`
let result =  ppl_@TOPOLOGY@@CLASS@_bounds_from_above @LTOPOLOGY@@LCLASS@01 e2;;
')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`
print_string "testing ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@" ;;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_add_@ADD_REPRESENT@ @LTOPOLOGY@@LCLASS@01 @ADD_REPRESENT@1;;
')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`
print_string "testing ppl_@CLASS@_@COMPARISON@_@CLASS@" ;;
print_newline();;
let b = ppl_@TOPOLOGY@@CLASS@_@COMPARISON@_@TOPOLOGY@@CLASS@
  @LTOPOLOGY@@LCLASS@01 @LTOPOLOGY@@LCLASS@01;;
')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`
print_string "testing ppl_@CLASS@_@BINOP@" ;;
print_newline();;
let copy
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
ppl_@TOPOLOGY@@CLASS@_@BINOP@
  copy @LTOPOLOGY@@LCLASS@02;;
')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`
print_string "testing ppl_@CLASS@_get_@GET_REPRESENT@s";;
print_newline();;
let @GET_REPRESENT@s = ppl_@TOPOLOGY@@CLASS@_get_@GET_REPRESENT@s @LTOPOLOGY@@LCLASS@01 in
List.iter print_@GET_REPRESENT@ @GET_REPRESENT@s;;
print_newline();;
')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`
print_string "testing ppl_@CLASS@_bounded_@AFFIMAGE@";;
print_newline();;
let copy_@AFFIMAGE@
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
ppl_@TOPOLOGY@@CLASS@_bounded_@AFFIMAGE@ copy_@AFFIMAGE@ 1
  ((Z.of_int 2) */ var2) var2 (Z.from_int 10);;
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`
print_string "testing ppl_@CLASS@_generalized_@AFFIMAGE@";;
print_newline();;
let copy_@AFFIMAGE@
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@
  copy_@AFFIMAGE@ 1 Equal_RS var1 (Z.from_int 10);;
')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`
print_string "testing ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs";;
print_newline();;
let copy_@AFFIMAGE@
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
ppl_@TOPOLOGY@@CLASS@_generalized_@AFFIMAGE@_lhs_rhs
  copy_@AFFIMAGE@ ((Z.of_int 1) */ var0) Equal_RS (linear_expression_of_int 7);;
')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`
let copy_@AFFIMAGE@
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
print_string "testing ppl_@CLASS@_@AFFIMAGE@";;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_@AFFIMAGE@ copy_@AFFIMAGE@ 1 ((Z.of_int 2) */ var2) (Z.from_int 2);;
')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens',
`
print_string
  "testing ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens";;
print_newline();;
let a = ppl_@TOPOLOGY@@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens
  @LTOPOLOGY@@LCLASS@01 @LTOPOLOGY@@LCLASS@01 constraints1 10;;
')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`
print_string "testing ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens";;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_@WIDEN@_widening_assign @LTOPOLOGY@@LCLASS@01 ;;
')

m4_define(`ppl_@CLASS@_OK_code',
`
print_string "testing ppl_@CLASS@_OK";;
print_newline();;
let b = ppl_@TOPOLOGY@@CLASS@_OK @LTOPOLOGY@@LCLASS@01;;
')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`
print_string "testing ppl_@CLASS@_@MAXMIN@_with_point";;
print_newline();;
print_string "Testing minimization";;
let (is_bounded, num, den, is_supremum, gen)
  = ppl_@TOPOLOGY@@CLASS@_minimize @LTOPOLOGY@@LCLASS@01 e3;;
print_newline();;
print_string "Value: ";;
print_int(Z.to_int num);;
print_string "/";;
print_int(Z.to_int den);;
print_string (", is_bounded: ");;
print_string (string_of_bool is_bounded);;
print_string (", is_supremum: ");;
print_string (string_of_bool is_supremum);;
print_string (", generator: ");;
print_generator(gen);;
print_newline();;
')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`
print_string "testing ppl_@CLASS@_remove_space_dimensions";;
print_newline();;
let copy
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
let dimensions_to_remove = [2;0];;
ppl_@TOPOLOGY@@CLASS@_remove_space_dimensions copy dimensions_to_remove;;
')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`
print_string "testing ppl_@CLASS@_fold_space_dimensions";;
print_newline();;
let copy
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
let dimensions_to_fold = [1];;
ppl_@TOPOLOGY@@CLASS@_fold_space_dimensions copy dimensions_to_fold 0;;
')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`
print_string "testing ppl_@CLASS@_map_space_dimensions";;
print_newline();;
let copy
  = ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(@LTOPOLOGY@@LCLASS@01);;
let dimensions_to_map = [(0,1);(1,2);(2,0);];;
let i = ppl_@TOPOLOGY@@CLASS@_space_dimension copy;;
print_string "Space dimension is: ";
print_int i;;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_map_space_dimensions copy dimensions_to_map;;
')

m4_define(`ppl_@CLASS@_constrains_code',
`
print_string "testing ppl_@CLASS@_constrains";;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_constrains @LTOPOLOGY@@LCLASS@01 1;;
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimension_code',
`
print_string "testing ppl_@CLASS@_unconstrain_space_dimension";;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimension @LTOPOLOGY@@LCLASS@01 1;;
')

m4_define(`ppl_@CLASS@_unconstrain_space_dimensions_code',
`
let dimensions_to_unconstrain = [1];;
ppl_@TOPOLOGY@@CLASS@_unconstrain_space_dimensions
  @LTOPOLOGY@@LCLASS@01 dimensions_to_unconstrain;;
')

m4_define(`ppl_@CLASS@_swap_code',
`
print_string "testing ppl_@CLASS@_swap";;
print_newline();;
ppl_@TOPOLOGY@@CLASS@_swap @LTOPOLOGY@@LCLASS@01 @LTOPOLOGY@@LCLASS@02;;
')
