m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_prolog.icc.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_ocaml_ml_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_common_dat.m4')dnl
m4_include(`ppl_interface_generator_ocaml_dat.m4')dnl
dnl
m4_divert(-1)dnl

dnl m4_pre_all_classes_code
dnl
dnl Definition for converting a term to a class handle code for all
dnl classes must be placed before all the generated code so that one class
dnl can be copied from another.
m4_define(`m4_pre_all_classes_code', `')


dnl m4_pre_extra_class_code(Class_Counter)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
type m4_downcase(m4_interface_class$1)`'dnl
')

m4_divert`'dnl

include Ppl_ocaml_globals
include Ppl_ocaml_types
open Gmp

exception Error of string
let _ = Callback.register_exception "PPL_arithmetic_overflow" (Error "any string")
let _ = Callback.register_exception "PPL_internal_error" (Error "any string")
let _ = Callback.register_exception "PPL_unknown_standard_exception" (Error "any string")
let _ = Callback.register_exception "PPL_unexpected_error" (Error "any string")

dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl

external test_linear_expression:
  linear_expression -> unit = "test_linear_expression"

external test_linear_constraint:
  linear_constraint -> unit = "test_linear_constraint"

external test_linear_generator:
  linear_generator -> unit = "test_linear_generator"

external test_constraint_system:
  constraint_system -> unit = "test_constraint_system"

external test_generator_system:
  generator_system -> unit = "test_generator_system"
