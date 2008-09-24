m4_define(`dnl', `m4_dnl')`'dnl
m4_divert(-1)
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

dnl
dnl ==================================================================
dnl Common files are included here
dnl ==================================================================
dnl
m4_include(`ppl_interface_generator_common.m4')
m4_include(`ppl_interface_generator_common_dat.m4')
m4_include(`ppl_interface_generator_java_procedure_generators.m4')
m4_include(`ppl_interface_generator_java_test_java_code.m4')
dnl
dnl This file generates ppl_java_classes_test.java.
m4_divert`'
/* Java code for checking all classes.  -*- C++ -*-
m4_include(`ppl_interface_generator_copyright')dnl
*/

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Vector;
import parma_polyhedra_library.*;

public class ppl_java_generated_tests {
static {
    try {
        System.loadLibrary("ppl_java");
 }

   catch (UnsatisfiedLinkError  e) {
  System.out.println("Unable to load the library");
  System.exit(-1);
 }
}


m4_include(`ppl_java_tests_common')`'dnl
    public static void main(String[] args) {
	ppl_java_generated_tests test1 = new ppl_java_generated_tests();
	test1.initialize();
	// Here generated stuff.
m4_divert(1)`'dnl
    }

    // Here generated stuff.
m4_divert(2)`'dnl
}
m4_divert(-1)

dnl Ensure any schematic procedure macro that is not defined
dnl in the code file outputs a warning message.
m4_define(`m4_default_code', `m4_dumpdef($1`'_code)')

dnl ==================================================================
dnl Declare test for each domain
dnl ==================================================================

m4_pushdef(`m4_pre_extra_class_code', `dnl
m4_replace_all_patterns_in_string($1,
  m4_run_class_code,
  m4_pattern_list)`'dnl
')

m4_pushdef(`m4_post_extra_class_code', `dnl
')

m4_pushdef(`m4_extension', `')
m4_divert`'dnl
m4_all_code`'dnl
m4_undivert(1)`'dnl
m4_divert(-1)

dnl ==================================================================
dnl Test all methods
dnl ==================================================================

m4_popdef(`m4_pre_extra_class_code')
m4_popdef(`m4_post_extra_class_code')
m4_popdef(`m4_extension')
m4_pushdef(`m4_pre_extra_class_code', `dnl
m4_replace_all_patterns_in_string($1,
  m4_run_class_test_code,
  m4_pattern_list)`'dnl
m4_replace_all_patterns_in_string($1,
  m4_new_class_element_code,
  m4_pattern_list)`'dnl
')
m4_pushdef(`m4_post_extra_class_code', `dnl
}
catch (parma_polyhedra_library.Overflow_Error_Exception e) {
System.out.println("*Overflow detected*::exception caught");
}
System.gc();
return true;

    }

')

m4_divert`'dnl
m4_all_code`'dnl
m4_undivert(2)`'dnl
