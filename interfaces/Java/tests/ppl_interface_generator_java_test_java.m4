m4_define(`dnl', `m4_dnl')`'dnl
dnl
dnl ==================================================================
dnl Common files are included here
dnl ==================================================================
dnl
m4_include(`ppl_interface_generator_common.m4')`'dnl
m4_include(`ppl_interface_generator_common_dat.m4')`'dnl
m4_include(`ppl_interface_generator_java_dat.m4')dnl
dnl
dnl This file generates ppl_java_classes_test.java.
/* Java code for checking all classes.  -*- C++ -*-
m4_include(`ppl_interface_generator_copyright')dnl
*/

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Vector;
import ppl_java.*;

public class ppl_java_generated_tests {
  static {
        System.loadLibrary("ppl_java");
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
dnl
dnl ==================================================================
dnl Declare test for each domain
dnl ==================================================================
dnl
m4_include(`ppl_interface_generator_java_test_java_code.m4')`'dnl
m4_pushdef(`m4_pre_extra_class_code', `dnl
m4_replace_all_patterns_in_string($1,
  m4_run_class_code,
  m4_pattern_list)`'dnl
')`'dnl
dnl
m4_pushdef(`m4_post_extra_class_code', `dnl
')
dnl
m4_pushdef(`m4_extension', `')`'dnl
dnl
m4_all_code`'dnl
m4_undivert(1)`'dnl
dnl
dnl ==================================================================
dnl Test all methods
dnl ==================================================================
dnl
m4_popdef(`m4_pre_extra_class_code')`'dnl
m4_popdef(`m4_post_extra_class_code')`'dnl
m4_popdef(`m4_extension')`'dnl
m4_pushdef(`m4_pre_extra_class_code', `dnl
m4_replace_all_patterns_in_string($1,
  m4_run_class_test_code,
  m4_pattern_list)`'dnl
m4_replace_all_patterns_in_string($1,
  m4_new_class_element_code,
  m4_pattern_list)`'dnl
')`'dnl
m4_pushdef(`m4_post_extra_class_code', `dnl
	return true;
    }

')`'dnl
dnl
m4_all_code`'dnl
m4_undivert(2)`'dnl
