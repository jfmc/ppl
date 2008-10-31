Note that the ppl_java.jar JAR file is installed in the <prefix>/<libdir>/ppl
directory along with the JNI shared object.  This implies you should use
`System.load' in your program, passing the full patch of the dynamic shared
object.
