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

m4_divert(-1)dnl

m4_define(`m4_class_build_cpp_object1_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    private native void build_cpp_object(@UBUILD_REPRESENT@_System cs);

')

m4_define(`m4_class_build_cpp_object2_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    private native void build_cpp_object(long num_dimensions,
                                         Degenerate_Element kind);

')

m4_define(`m4_class_build_cpp_object3_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    private native void build_cpp_object(@FRIEND@ y);

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public @TOPOLOGY@@CLASS@(long num_dimensions,
			Degenerate_Element kind) {
	build_cpp_object(num_dimensions, kind);
    }

    private @TOPOLOGY@@CLASS@() {
    }

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@FRIEND@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public @TOPOLOGY@@CLASS@(@FRIEND@ y) {
        build_cpp_object(y);
    }

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public @TOPOLOGY@@CLASS@(@UBUILD_REPRESENT@_System cs) {
	build_cpp_object(cs);
    }

')

m4_define(`ppl_delete_@CLASS@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    protected native void finalize();

')

m4_define(`ppl_free_@CLASS@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public native void free();

')

m4_define(`ppl_@TOPOLOGY@@CLASS@_@UB_EXACT@_code',
`dnl
%<--%<--%<-- @TOPOLOGY@@CLASS@.java
    public native boolean @UB_EXACT@(@TOPOLOGY@@CLASS@ y);

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void swap(@CLASS@ y);

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native long @DIMENSION@();

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @UGET_REPRESENT@_System @GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @UGET_REPRESENT@_System minimized_@GET_REPRESENT@s();

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native Poly_@UALT_RELATION_REPRESENT@_Relation relation_with(@URELATION_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @HAS_PROPERTY@();

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @SIMPLIFY@();

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean bounds_from_@ABOVEBELOW@(Linear_Expression expr);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> maximum);

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @MAXMIN@(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> maximum,
				   Generator point);

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @COMPARISON@(@CLASS@ y);

')

m4_define(`ppl_@CLASS@_equals_@CLASS@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean equals(@CLASS@ p);

    public boolean equals(Object y) {
   if (this == y)
     return true;
   if (y == null || y.getClass() != this.getClass())
     return false;
   return this.equals((@CLASS@) y);
  }
')

m4_define(`ppl_@CLASS@_hashcode_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native int hashCode();

')


m4_define(`ppl_@CLASS@_OK_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean OK();

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean add_@ADD_REPRESENT@_and_minimize(@UADD_REPRESENT@ c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_@ADD_REPRESENT@s(@UADD_REPRESENT@_System c);

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean add_@ADD_REPRESENT@s_and_minimize(@UADD_REPRESENT@_System c);

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @BINOP@(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native boolean @BINOP@_and_minimize(@CLASS@ p);

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void @AFFIMAGE@(Variable var, Linear_Expression expr,
				    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void bounded_@AFFIMAGE@(Variable var,
					    Linear_Expression lb_expr,
					    Linear_Expression ub_expr,
					    Coefficient denominator);

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator);

')


m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs);

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void
    @EXTRAPOLATION@_narrowing_assign(@TOPOLOGY@@CLASS@ arg);

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Variable var,
				Relation_Symbol relsym,
				Linear_Expression expr,
				Coefficient denominator,
				Coefficient modulus);

')


m4_define(`ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void generalized_@AFFIMAGE@(Linear_Expression lhs,
				Relation_Symbol relsym,
				Linear_Expression rhs,
				Coefficient modulus);

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
%<--%<--%<-- @CLASS@.java
public native void @WIDEN@_widening_assign(@CLASS@ y,
                                By_Reference<Integer> tp);

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native
	void @LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(@CLASS@ y,
						 @UCONSTRAINER@_System cs,
						 By_Reference<Integer> tp);

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void add_space_dimensions_@EMBEDPROJECT@(long m);

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void remove_space_dimensions(Variables_Set to_be_removed);

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void remove_higher_space_dimensions(long
						      new_dimension);

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void expand_space_dimension(Variable var, long m);

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void fold_space_dimensions(Variables_Set to_be_folded,
					     Variable var);

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native void map_space_dimensions(Partial_Function pfunc);

')

m4_define(`ppl_@CLASS@_string_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native String toString();

')

m4_define(`ppl_@CLASS@_begin_iterator_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @CLASS@_Iterator begin_iterator();

')

m4_define(`ppl_@CLASS@_end_iterator_code',
`dnl
%<--%<--%<-- @CLASS@.java
    public native @CLASS@_Iterator end_iterator();

')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`dnl
%<--%<--%<-- @CLASS@_Iterator.java
  public native @ALT_CPP_DISJUNCT@ get_disjunct();

')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
`dnl
%<--%<--%<-- @CLASS@.java
  public native void drop_disjunct(@CLASS@_Iterator itr);

')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
`dnl
%<--%<--%<-- @CLASS@_Iterator.java
  public native boolean equals(@CLASS@_Iterator itr);

')

m4_define(`ppl_@CLASS@_increment_iterator_code',
`dnl
%<--%<--%<-- @CLASS@_Iterator.java
  public native void next();

')

m4_define(`ppl_@CLASS@_decrement_iterator_code',
`dnl
%<--%<--%<-- @CLASS@_Iterator.java
  public native void prev();

')

m4_define(`ppl_@CLASS@_size_code',
`dnl
%<--%<--%<-- @CLASS@.java
  public native long size();

')

m4_define(`ppl_@CLASS@_total_memory_in_bytes_code',
`dnl
%<--%<--%<-- @CLASS@.java
  public native long total_memory_in_bytes();

')

m4_define(`ppl_@CLASS@_constrains_code',
`dnl
%<--%<--%<-- @CLASS@.java
  public native boolean constrains(Variable var);

')
