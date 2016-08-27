/* Test thread-safety for customized variable output in Java.
   Copyright (C) 2010-2016 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://bugseng.com/products/ppl/ . */

import parma_polyhedra_library.*;

public class Variable_Output_thread_safe1 {
    static {
        try {
            System.loadLibrary("ppl_java");
        }
        catch (UnsatisfiedLinkError  e) {
            System.out.println("Unable to load the library");
            System.out.println(e.getMessage());
            System.exit(-1);
        }
    }

    public static class PPL_Thread extends Thread {
        // Shared by all threads: each writes on its own slot.
        public static String[] outputs;

        private int index;

        public PPL_Thread(int i) {
            index = i;
        }

        public void run() {
            Parma_Polyhedra_Library.initialize_thread();

            // Changing output function to custom one.
            String prefix = "t" + index + "_";
            Variable.setStringifier(new Custom_Variable_Stringifier(prefix));

            // For debugging purposes, increase chances that another
            // thread is sheduled *after* setting the stringifier.
            yield();

            Variable A = new Variable(0);
            Variable B = new Variable(1);
            Variable C = new Variable(2);

            String result = "Thread prefix " + prefix + " : "
                + A.toString() + " "
                + B.toString() + " "
                + C.toString();
            PPL_Test.println_if_noisy(result);

            // Publish result.
            outputs[index] = result;

            Parma_Polyhedra_Library.finalize_thread();
        }
    }

    public static boolean test01() {
        final int num_threads = 50;
        PPL_Thread[] threads = new PPL_Thread[num_threads];
        PPL_Thread.outputs = new String[num_threads];

        try {
            // Create threads.
            for (int i = 0; i < num_threads; ++i)
                threads[i] = new PPL_Thread(i);
            // Start threads.
            for (int i = 0; i < num_threads; ++i)
                threads[i].start();
            // Join threads.
            for (int i = 0; i < num_threads; ++i)
                threads[i].join();
        }
        catch (InterruptedException e) {
            PPL_Test.println_if_noisy("Error joining threads");
        }

        // Check expected result.
        for (int i = 0; i < num_threads; ++i) {
            String prefix = "t" + i + "_";
            String expected = "Thread prefix " + prefix + " :";
            for (long var_id = 0; var_id < 3; ++var_id)
                expected += " " + prefix + var_id;
            boolean ok = expected.equals(PPL_Thread.outputs[i]);
            if (!ok)
                return false;
        }
        return true;
    }

    public static void main(String[] args) {
        Parma_Polyhedra_Library.initialize_library();
        boolean test_result_ok =
            Test_Executor.executeTests(Variable_Output_thread_safe1.class);
        Parma_Polyhedra_Library.finalize_library();
        if (!test_result_ok)
            System.exit(1);
        System.exit(0);
    }
}
