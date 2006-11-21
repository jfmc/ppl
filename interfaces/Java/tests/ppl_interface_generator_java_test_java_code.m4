m4_divert(-1)`'dnl
m4_define(`m4_run_class_code',
`	test1.run_@CLASS@_test();`'dnl
')

m4_define(`m4_run_class_test_code',
`
    public boolean run_@CLASS@_test() {
')

m4_define(`m4_new_class_element_code',
`
 @TOPOLOGY@@CLASS@ @LTOPOLOGY@@LCLASS@1 = new @TOPOLOGY@@CLASS@(@CONSTRAINER@s1);
')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code', `
        if (@LTOPOLOGY@@LCLASS@1.@HAS_PROPERTY@())
           System.out.println(
             "@HAS_PROPERTY@ is true for @LTOPOLOGY@@LCLASS@1.");
        else
           System.out.println(
             "@HAS_PROPERTY@ is false for @LTOPOLOGY@@LCLASS@1.");

')
m4_divert`'dnl