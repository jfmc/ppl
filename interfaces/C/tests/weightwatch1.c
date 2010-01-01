/* Test the deterministic timeout facility of the PPL C interface library.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_c.h"
#include <stdlib.h>

void
open_hypercube(int dimension, ppl_Polyhedron_t ph) {
  int i;
  mpz_t z_one;
  mpz_t z_minus_one;
  ppl_Coefficient_t coeff_one;
  ppl_Coefficient_t coeff_minus_one;
  ppl_Linear_Expression_t le;
  ppl_Constraint_t c;
  ppl_Constraint_System_t cs;

  mpz_init_set_si(z_one, 1);
  mpz_init_set_si(z_minus_one, -1);
  ppl_new_Coefficient(&coeff_one);
  ppl_assign_Coefficient_from_mpz_t(coeff_one, z_one);
  ppl_new_Coefficient(&coeff_minus_one);
  ppl_assign_Coefficient_from_mpz_t(coeff_minus_one, z_minus_one);
  ppl_new_Linear_Expression_with_dimension(&le, dimension);
  ppl_new_Constraint_System(&cs);
  for (i = 0; i < dimension; ++i) {
    ppl_Linear_Expression_add_to_coefficient(le, i, coeff_one);
    /* Variable(i) > 0 */
    ppl_new_Constraint(&c, le, PPL_CONSTRAINT_TYPE_GREATER_THAN);
    ppl_Constraint_System_insert_Constraint(cs, c);
    ppl_delete_Constraint(c);
    /* Variable(i) < 1 */
    ppl_Linear_Expression_add_to_inhomogeneous(le, coeff_minus_one);
    ppl_new_Constraint(&c, le, PPL_CONSTRAINT_TYPE_LESS_THAN);
    ppl_Constraint_System_insert_Constraint(cs, c);
    ppl_delete_Constraint(c);
    /* Zero `le' */
    ppl_Linear_Expression_add_to_coefficient(le, i, coeff_minus_one);
    ppl_Linear_Expression_add_to_inhomogeneous(le, coeff_one);
  }
  ppl_Polyhedron_add_constraints(ph, cs);
  ppl_delete_Constraint_System(cs);
  ppl_delete_Linear_Expression(le);
  ppl_delete_Coefficient(coeff_minus_one);
  ppl_delete_Coefficient(coeff_one);
  mpz_clear(z_minus_one);
  mpz_clear(z_one);
}

void
weighted_compute_open_hypercube_generators(unsigned weight,
                                           int max_dimension) {
  int i;
  int result;
  ppl_const_Generator_System_t gs;
  ppl_Polyhedron_t ph;

  for (i = 0; i <= max_dimension; ++i) {
    ppl_new_NNC_Polyhedron_from_space_dimension(&ph, i, 0);
    open_hypercube(i, ph);
    ppl_set_deterministic_timeout(weight);
    result = ppl_Polyhedron_get_generators(ph, &gs);
    ppl_reset_deterministic_timeout();
    ppl_delete_Polyhedron(ph);
    if (result == PPL_TIMEOUT_EXCEPTION)
      /* Deterministic timeout expired */
      return;
    else if (result != 0)
      /* Unexpected error */
      exit(1);
  }
  /* Should not reach this point */
  exit(1);
}

int
main() {
  ppl_initialize();
  weighted_compute_open_hypercube_generators(1000, 20);
  ppl_finalize();
  return 0;
}

