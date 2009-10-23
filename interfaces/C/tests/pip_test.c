/* Test the use of the PPL PIP solver from C code.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_c_test.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static const char* program_name = 0;

static void
my_exit(int status) {
  (void) ppl_finalize();
  exit(status);
}

static void
fatal(const char* format, ...) {
  va_list ap;
  fprintf(stderr, "%s: ", program_name);
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  my_exit(1);
}

#define N_VARS 2
#define N_PARAMETERS 2
#define N_CONSTRAINTS 4
int
main(int argc, char **argv) {
  ppl_PIP_Problem_t pip;
  ppl_Constraint_t ct;
  ppl_Coefficient_t c;
  ppl_Linear_Expression_t le;
  ppl_dimension_type i, j;
  mpz_t mpc;
  int ok;

  static int coef[N_CONSTRAINTS][N_VARS+N_PARAMETERS+1] = {
    { 2, 3, 0, 0, -8 },
    { 4, -1, 0, 0, -4 },
    { 0, -1, 0, 1, 0 },
    { -1, 0, 1, 0, 0 },
  };
  ppl_dimension_type parameter_dim[N_PARAMETERS];

  program_name = argv[0];
  if (argc != 1) {
    fprintf(stderr, "usage: %s\n", program_name);
    exit(1);
  }
  if (ppl_initialize() < 0)
    fatal("cannot initialize the Parma Polyhedra Library");

  for (i=0; i<N_PARAMETERS; ++i)
    parameter_dim[i] = i + N_VARS;
  ppl_new_PIP_Problem_from_space_dimension(&pip, N_VARS+N_PARAMETERS);
  ppl_PIP_Problem_add_to_parameter_space_dimensions(pip, parameter_dim,
                                                    N_PARAMETERS);
  mpz_init(mpc);
  ppl_new_Coefficient(&c);
  for (i=0; i<N_CONSTRAINTS; ++i) {
    ppl_new_Linear_Expression(&le);
    for (j=0; j<N_VARS+N_PARAMETERS; ++j) {
      mpz_set_si(mpc, coef[i][j]);
      ppl_assign_Coefficient_from_mpz_t(c, mpc);
      ppl_Linear_Expression_add_to_coefficient(le, j, c);
    }
    mpz_set_si(mpc, coef[i][j]);
    ppl_assign_Coefficient_from_mpz_t(c, mpc);
    ppl_Linear_Expression_add_to_inhomogeneous(le, c);
    ppl_new_Constraint(&ct, le, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_PIP_Problem_add_constraint(pip, ct);
    ppl_delete_Constraint(ct);
    ppl_delete_Linear_Expression(le);
  }
  ppl_delete_Coefficient(c);
  mpz_clear(mpc);

  ok = (ppl_PIP_Problem_solve(pip) == PPL_PIP_PROBLEM_STATUS_OPTIMIZED);
  if (ok) {
    ppl_dimension_type dim;
    ppl_const_PIP_Tree_Node_t solution;
    ppl_PIP_Problem_space_dimension(pip, &dim);
    ppl_PPL_Problem_solution(pip, &solution);
    /* display_solution(solution, N_VARS, N_PARAMETERS, parameter_dim); */
  }

  ppl_delete_PIP_Problem(pip);
  ppl_finalize();
  return ok ? 0 : 1;
}
