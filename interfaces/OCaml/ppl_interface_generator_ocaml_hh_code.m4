dnl This file is part of the Parma Polyhedra Library (PPL)
dnl used by ppl_interfaces_generator_ocaml_hh_files.m4
dnl for generating the access code for the OCaml interface.
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

m4_define(`m4_access_class_code',
`dnl
//! Give access to the embedded @CLASS@* in \p v.
inline @TOPOLOGY@@CPP_CLASS@*
p_@TOPOLOGY@@CLASS@_val(value v) {
  return unmark(*reinterpret_cast<@TOPOLOGY@@CPP_CLASS@**>(Data_custom_val(v)));
}

//! Give access to the embedded @CLASS@* in \p v.
inline @TOPOLOGY@@CPP_CLASS@*&
actual_p_@TOPOLOGY@@CLASS@_val(value v) {
  return *reinterpret_cast<@TOPOLOGY@@CPP_CLASS@**>(Data_custom_val(v));
}

void
custom_@TOPOLOGY@@CLASS@_finalize(value v) {
   if (!marked(actual_p_@TOPOLOGY@@CLASS@_val(v)))
      delete actual_p_@TOPOLOGY@@CLASS@_val(v);
}

static struct custom_operations @TOPOLOGY@@CLASS@_custom_operations = {
  "it.unipr.cs.ppl" "." PPL_VERSION "." "@TOPOLOGY@@CLASS@"@COMMA@
  custom_@TOPOLOGY@@CLASS@_finalize@COMMA@
  custom_compare_default@COMMA@
  custom_hash_default@COMMA@
  custom_serialize_default@COMMA@
  custom_deserialize_default
};

inline value
val_p_@TOPOLOGY@@CLASS@(const @TOPOLOGY@@CPP_CLASS@& ph) {
  value v = caml_alloc_custom(&@TOPOLOGY@@CLASS@_custom_operations,
			      sizeof(@TOPOLOGY@@CPP_CLASS@*), 0, 1);
  actual_p_@TOPOLOGY@@CLASS@_val(v) = const_cast<@TOPOLOGY@@CPP_CLASS@*>(&ph);
  return(v);
}

')
