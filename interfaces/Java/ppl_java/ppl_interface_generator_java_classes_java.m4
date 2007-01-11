m4_define(`dnl', `m4_dnl')`'dnl
dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
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

dnl This file is to generate CLASS.java.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_java_classes_java_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_common_dat.m4')dnl
m4_include(`ppl_interface_generator_java_dat.m4')dnl
dnl
m4_divert(-1)dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_define(`m4_this_class', `m4_interface_class$1')`'dnl
m4_define(`m4_this_class_kind', `m4_class_kind$1')`'dnl
%<--%<--%<-- m4_this_class`'.java
package ppl_java;

public class m4_this_class extends PPL_Object {`'dnl
m4_ifelse(m4_this_class, Polyhedron,
`
%<--%<--%<-- C_`'m4_this_class`'.java
package ppl_java;

public class C_`'m4_this_class extends Polyhedron {

%<--%<--%<-- NNC_`'m4_this_class`'.java
package ppl_java;

public class NNC_`'m4_this_class extends Polyhedron {
',
m4_this_class_kind, Pointset_Powerset,
`
%<--%<--%<-- m4_this_class`'_Iterator.java
package ppl_java;

public class m4_this_class`'_Iterator extends PPL_Object {

  private m4_this_class`'_Iterator() {};

  private native void build_ppl_object(m4_this_class obj);
')`'dnl
m4_undefine(`m4_this_class')`'dnl
m4_undefine(`m4_this_class_kind')
')

%<--%<--%<-- m4_interface_class$1`'.java
public class m4_cplusplus_class$1 extends PPL_Object {

dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
m4_define(`m4_post_extra_class_code', `dnl
m4_replace_all_patterns_in_string($1,
  m4_class_build_cpp_object1_code,
  m4_pattern_list)`'dnl
m4_replace_all_patterns_in_string($1,
  m4_class_build_cpp_object2_code,
  m4_pattern_list)`'dnl
m4_replace_all_patterns_in_string($1,
  m4_class_build_cpp_object3_code,
  m4_pattern_list)`'dnl
m4_define(`m4_this_class', `m4_interface_class$1')`'dnl
m4_define(`m4_this_class_kind', `m4_class_kind$1')
%<--%<--%<-- m4_this_class`'.java
}`'dnl
m4_ifelse(m4_this_class, Polyhedron,
`

%<--%<--%<-- C_`'m4_this_class`'.java
}

%<--%<--%<-- NNC_`'m4_this_class`'.java
}
',
m4_this_class_kind, Pointset_Powerset,
`
%<--%<--%<-- m4_this_class`'_Iterator.java
}`'dnl
')`'dnl
m4_undefine(`m4_this_class')`'dnl
m4_undefine(`m4_this_class_kind')
')

m4_divert`'dnl
dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl
dnl End of file generation.
