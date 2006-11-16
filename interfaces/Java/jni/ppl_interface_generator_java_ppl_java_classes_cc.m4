m4_define(`dnl', `m4_dnl')`'dnl
dnl This file is to generate CLASS.java.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_divert(-1)dnl
m4_include(`ppl_interface_generator_java_ppl_java_classes_cc_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_common_dat.m4')dnl
m4_include(`ppl_interface_generator_java_dat.m4')dnl
dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_define(`m4_this_class', `m4_interface_class$1')`'dnl
%<--%<--%<-- ppl_java_`'m4_this_class`'.cc
`#'include "ppl_java_`'m4_this_class`'.h"
`#'include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;
m4_ifelse(m4_this_class, Polyhedron,
`
%<--%<--%<-- ppl_java_C_`'m4_this_class`'.cc
`#'include "ppl_java_C_`'m4_this_class`'.h"
`#'include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;

%<--%<--%<-- ppl_java_NNC_`'m4_this_class`'.cc
`#'include "ppl_java_NNC_`'m4_this_class`'.h"
`#'include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;
m4_undefine(`m4_this_class')
')
')

dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
m4_define(`m4_post_extra_class_code', `')

m4_divert`'dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl
dnl End of file generation.
