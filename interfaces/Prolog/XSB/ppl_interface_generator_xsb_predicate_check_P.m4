m4_define(`dnl', `m4_dnl')
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

dnl This file generates ppl_ciao_predicate_check.pl
/* XSB Prolog interface: XSB Prolog part for checking all predicates.
m4_include(`ppl_interface_generator_copyright')
*/

:- compiler_options([xpp_on]).
:- import xpp_include_dir/1 from parse.
:- assert(xpp_include_dir('.')).

:- import append/3, length/2, member/2 from basics.
:- import
m4_divert(1)
   from ppl_xsb.

:- [ppl_xsb].

#include "pl_check.pl"

prolog_system('XSB').

main :-
    (check_all ->
        write('OK')
    ;
        write('FAILURE')
    ),
    nl.

:- main.
m4_divert`'dnl
m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
m4_define(`m4_extension', `m4_ifelse($4, 0, , `COMMA
')	  $1/$2')dnl
m4_patsubst(ppl_prolog_sys_code, COMMA, `,')`'dnl
m4_undivert(1)`'dnl
