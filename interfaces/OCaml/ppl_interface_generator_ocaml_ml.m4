m4_define(`dnl', `m4_dnl')
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
m4_ifelse(m4_interface_class$1, Polyhedron,
type c_`'m4_downcase(m4_interface_class$1)
type nnc_`'m4_downcase(m4_interface_class$1),
type m4_downcase(m4_interface_class$1))

')

m4_divert`'dnl
dnl

include Ppl_ocaml_globals
include Ppl_ocaml_types
open Gmp

exception Error of string
let _ = Callback.register_exception "PPL_arithmetic_overflow" (Error "any string")
let _ = Callback.register_exception "PPL_internal_error" (Error "any string")
let _ = Callback.register_exception "PPL_unknown_standard_exception" (Error "any string")
let _ = Callback.register_exception "PPL_not_an_unsigned_exception" (Error "any string")

let _ = Callback.register_exception "PPL_unexpected_error" (Error "any string")
dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl
