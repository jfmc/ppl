/* Solve linear programming problems by vertex/point enumeration.
   Just a toy to test the C interface of the library.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_c.h"
#include <sys/resource.h>
#include <gmp.h>
#include <glpk.h>
#include <getopt.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <string.h>

static struct option long_options[] = {
  {"check",          no_argument,       0, 'c'},
  {"help",           no_argument,       0, 'h'},
  {"min",            no_argument,       0, 'm'},
  {"max",            no_argument,       0, 'M'},
  {"max-cpu",        required_argument, 0, 'C'},
  {"max-memory",     required_argument, 0, 'V'},
  {"output",         required_argument, 0, 'o'},
  {"timings",        no_argument,       0, 't'},
  {"verbose",        no_argument,       0, 'v'},
  {0, 0, 0, 0}
};

static const char* usage_string
= "Usage: %s [OPTION]... [FILE]...\n\n"
"  -c, --check             checks plausibility of the optimum value found\n"
"  -m, --min               minimizes the objective function\n"
"  -M, --max               maximizes the objective function (default)\n"
"  -CSECS, --max-cpu=SECS  limits CPU usage to SECS seconds\n"
"  -VMB, --max-memory=MB   limits memory usage to MB megabytes\n"
"  -h, --help              prints this help text to stderr\n"
"  -oPATH, --output=PATH   appends output to PATH\n"
"  -t, --timings           prints timings to stderr\n"
"  -v, --verbose           outputs also the constraints "
"and objective function\n";


#define OPTION_LETTERS "bcmMC:V:ho:tv"

static const char* program_name = 0;

static unsigned long max_seconds_of_cpu_time = 0;
static unsigned long max_bytes_of_virtual_memory = 0;
static const char* output_argument = 0;
FILE* output_file = NULL;
static int check_optimum = 0;
static int print_timings = 0;
static int verbose = 0;
static int maximize = 1;

static void
my_exit(int status) {
  (void) ppl_finalize();
  exit(status);
}

static void
fatal(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  fprintf(stderr, "%s: ", program_name);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  my_exit(1);
}

static void
process_options(int argc, char *argv[]) {
  int option_index;
  int c;
  char* endptr;
  long l;

  while (1) {
    option_index = 0;
    c = getopt_long(argc, argv, OPTION_LETTERS, long_options, &option_index);
    if (c == EOF)
      break;

    switch (c) {
    case 0:
      break;

    case 'c':
      check_optimum = 1;
      fatal("option --check (-c) not implemented yet");
      break;

    case 'm':
      maximize = 0;
      break;

    case 'M':
      maximize = 1;
      break;

    case '?':
    case 'h':
      fprintf(stderr, usage_string, argv[0]);
      my_exit(0);
      break;

    case 'C':
      l = strtol(optarg, &endptr, 10);
      if (*endptr || l < 0)
	fatal("a non-negative integer must follow `-c'");
      else
	max_seconds_of_cpu_time = l;
      break;

    case 'V':
      l = strtol(optarg, &endptr, 10);
      if (*endptr || l < 0)
	fatal("a non-negative integer must follow `-m'");
      else
	max_bytes_of_virtual_memory = l*1024*1024;
      break;

    case 'o':
      output_argument = optarg;
      break;

    case 't':
      print_timings = 1;
      break;

    case 'v':
      verbose = 1;
      break;

    default:
      abort();
    }
  }

  if (optind >= argc)
    fatal("no input files");

  if (argc - optind > 1)
    /* We have multiple input files. */
    fatal("only one input file is accepted");

  if (output_argument) {
    output_file = fopen(output_argument, "a");
    if (output_file == NULL)
      fatal("cannot open output file `%s'", output_argument);
  }
  else
    output_file = stdout;
}

/* To save the time when start_clock is called. */
static struct timeval saved_ru_utime;

static void
start_clock() {
  struct rusage rsg;
  if (getrusage(RUSAGE_SELF, &rsg) != 0)
    fatal("getrusage failed: %s", strerror(errno));
  else
    saved_ru_utime = rsg.ru_utime;
}

static void
print_clock(FILE* f) {
  struct rusage rsg;
  if (getrusage(RUSAGE_SELF, &rsg) != 0)
    fatal("getrusage failed: %s", strerror(errno));
  else {
    time_t current_secs = rsg.ru_utime.tv_sec;
    time_t current_usecs = rsg.ru_utime.tv_usec;
    time_t saved_secs = saved_ru_utime.tv_sec;
    time_t saved_usecs = saved_ru_utime.tv_usec;
    int secs;
    int hsecs;
    if (current_usecs < saved_usecs) {
      hsecs = (((1000000 + current_usecs) - saved_usecs) + 5000) / 10000;
      secs = (current_secs - saved_secs) -1;
    }
    else {
      hsecs = ((current_usecs - saved_usecs) + 5000) / 10000;
      secs = current_secs - saved_secs;
    }
    assert(hsecs >= 0 && hsecs < 100 && secs >= 0);
    fprintf(f, "%d.%.2d", secs, hsecs);
  }
}

void
set_alarm_on_cpu_time(unsigned int seconds, void (*handler)(int)) {
  sigset_t mask;
  struct sigaction s;
  struct rlimit t;

  sigemptyset(&mask);

  s.sa_handler = handler;
  s.sa_mask = mask;
#if defined(SA_ONESHOT)
  s.sa_flags = SA_ONESHOT;
#elif defined(SA_RESETHAND)
  s.sa_flags = SA_RESETHAND;
#else
  #error "Need either SA_ONESHOT or SA_RESETHAND to be defined."
#endif

  if (sigaction(SIGXCPU, &s, 0) != 0)
    fatal("sigaction failed: %s", strerror(errno));

  if (getrlimit(RLIMIT_CPU, &t) != 0)
    fatal("getrlimit failed: %s", strerror(errno));

  if (seconds < t.rlim_cur) {
    t.rlim_cur = seconds;
    if (setrlimit(RLIMIT_CPU, &t) != 0)
      fatal("setrlimit failed: %s", strerror(errno));
  }
}

void
limit_virtual_memory(unsigned int bytes) {
  struct rlimit t;

  if (getrlimit(RLIMIT_AS, &t) != 0)
    fatal("getrlimit failed: %s", strerror(errno));

  if (bytes < t.rlim_cur) {
    t.rlim_cur = bytes;
    if (setrlimit(RLIMIT_AS, &t) != 0)
      fatal("setrlimit failed: %s", strerror(errno));
  }
}

static void
my_timeout(int dummy) {
  fprintf(stderr, "TIMEOUT\n");
  if (output_argument)
    fprintf(output_file, "TIMEOUT\n");
  my_exit(0);
}

static mpz_t tmp_z;
static mpq_t tmp1_q;
static mpq_t tmp2_q;
static ppl_Coefficient_t ppl_coeff;
static LPX* lp;

static void
print_variable(FILE* f, ppl_dimension_type var) {
  const char* name = lpx_get_col_name(lp, var+1);
  if (name != NULL)
    fprintf(f, "%s", name);
  else
    fprintf(f, "_");
}

static void
print_constraint(FILE* f, ppl_const_Constraint_t c) {
  int dimension = ppl_Constraint_space_dimension(c);
  int var;
  int first = 1;
  for (var = 0; var < dimension; ++var) {
    ppl_Constraint_coefficient(c, var, ppl_coeff);
    ppl_Coefficient_to_mpz_t(ppl_coeff, tmp_z);
    if (mpz_sgn(tmp_z) != 0) {
      if (!first) {
	if (mpz_sgn(tmp_z) > 0)
	  fputc('+', f);
	else {
	  fputc('-', f);
	  mpz_neg(tmp_z, tmp_z);
	}
      }
      else
	first = 0;
      if (mpz_cmp_si(tmp_z, -1) == 0)
	  fputc('-', f);
      else if (mpz_cmp_si(tmp_z, 1) != 0) {
	mpz_out_str(f, 10, tmp_z);
	fputc('*', f);
      }
      print_variable(f, var);
    }
  }
  if (first)
    fputc('0', f);
  switch (ppl_Constraint_type(c)) {
  case PPL_CONSTRAINT_TYPE_EQUAL:
    fprintf(f, " = ");
    break;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL:
    fprintf(f, " >= ");
    break;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN:
    fprintf(f, " > ");
    break;
  case PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL:
    fprintf(f, " <= ");
    break;
  case PPL_CONSTRAINT_TYPE_LESS_THAN:
    fprintf(f, " < ");
    break;
  }
  ppl_Constraint_inhomogeneous_term(c, ppl_coeff);
  ppl_Coefficient_to_mpz_t(ppl_coeff, tmp_z);
  mpz_neg(tmp_z, tmp_z);
  mpz_out_str(f, 10, tmp_z);

  fflush(f);
}

static void
add_constraints(ppl_LinExpression_t ppl_le,
		int type, mpq_t rational_lb, mpq_t rational_ub, mpz_t den_lcm,
		ppl_ConSys_t ppl_cs) {
  ppl_Constraint_t ppl_c;
  ppl_LinExpression_t ppl_le2;
  switch (type) {
  case LPX_FR:
    break;

  case LPX_LO:
    mpz_mul(tmp_z, den_lcm, mpq_numref(rational_lb));
    mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_inhomogeneous(ppl_le, ppl_coeff);
    ppl_new_Constraint(&ppl_c, ppl_le,
		       PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL);
    if (verbose) {
      print_constraint(output_file, ppl_c);
      fprintf(output_file, "\n");
    }
    ppl_ConSys_insert_Constraint(ppl_cs, ppl_c);
    ppl_delete_Constraint(ppl_c);
    break;

  case LPX_UP:
    mpz_mul(tmp_z, den_lcm, mpq_numref(rational_ub));
    mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_inhomogeneous(ppl_le, ppl_coeff);
    ppl_new_Constraint(&ppl_c, ppl_le,
		       PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL);
    if (verbose) {
      print_constraint(output_file, ppl_c);
      fprintf(output_file, "\n");
    }
    ppl_ConSys_insert_Constraint(ppl_cs, ppl_c);
    ppl_delete_Constraint(ppl_c);
    break;

  case LPX_DB:
    ppl_new_LinExpression_from_LinExpression(&ppl_le2, ppl_le);

    mpz_mul(tmp_z, den_lcm, mpq_numref(rational_lb));
    mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_inhomogeneous(ppl_le, ppl_coeff);
    ppl_new_Constraint(&ppl_c, ppl_le,
		       PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL);
    if (verbose) {
      print_constraint(output_file, ppl_c);
      fprintf(output_file, "\n");
    }
    ppl_ConSys_insert_Constraint(ppl_cs, ppl_c);
    ppl_delete_Constraint(ppl_c);

    mpz_mul(tmp_z, den_lcm, mpq_numref(rational_ub));
    mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_inhomogeneous(ppl_le2, ppl_coeff);
    ppl_new_Constraint(&ppl_c, ppl_le2,
		       PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL);
    ppl_delete_LinExpression(ppl_le2);
    if (verbose) {
      print_constraint(output_file, ppl_c);
      fprintf(output_file, "\n");
    }
    ppl_ConSys_insert_Constraint(ppl_cs, ppl_c);
    ppl_delete_Constraint(ppl_c);
    break;

  case LPX_FX:
    mpz_mul(tmp_z, den_lcm, mpq_numref(rational_lb));
    mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_inhomogeneous(ppl_le, ppl_coeff);
    ppl_new_Constraint(&ppl_c, ppl_le,
		       PPL_CONSTRAINT_TYPE_EQUAL);
    if (verbose) {
      print_constraint(output_file, ppl_c);
      fprintf(output_file, "\n");
    }
    ppl_ConSys_insert_Constraint(ppl_cs, ppl_c);
    ppl_delete_Constraint(ppl_c);
    break;

 default:
    fatal("internal error");
    break;
  }
}

static void
solve(char* file_name) {
  ppl_Polyhedron_t ppl_ph;
  ppl_ConSys_t ppl_cs;
  ppl_const_GenSys_t ppl_const_gs;
  ppl_GenSys_const_iterator_t git1, git2, ogit;
  ppl_const_Generator_t ppl_const_g;
  ppl_LinExpression_t ppl_le;
  int dimension, row, num_rows, column, nz, i, type;
  int* coefficient_index;
  double lb, ub;
  double* coefficient_value;
  mpq_t rational_lb, rational_ub;
  mpq_t* rational_coefficient;
  mpq_t* objective;
  ppl_LinExpression_t ppl_objective_le;
  mpq_t* candidate;
  int first_candidate;
  mpq_t optimum;
  mpz_t den_lcm;
  int empty;
  int unbounded;
  /* The following is initialized only to avoid a compiler warning. */
  int first_printed = 0;

  if (print_timings)
    start_clock();

  lp = lpx_read_mps(file_name);
  if (lp == NULL)
    fatal("cannot read MPS file `%s'", file_name);

  if (print_timings) {
    fprintf(stderr, "Time to read the input file: ");
    print_clock(stderr);
    fprintf(stderr, " s\n");
    start_clock();
  }

  dimension = lpx_get_num_cols(lp);

  coefficient_index = (int*) malloc((dimension+1)*sizeof(int));
  coefficient_value = (double*) malloc((dimension+1)*sizeof(double));
  rational_coefficient = (mpq_t*) malloc((dimension+1)*sizeof(mpq_t));


  ppl_new_ConSys(&ppl_cs);

  mpq_init(rational_lb);
  mpq_init(rational_ub);
  for (i = 1; i <= dimension; ++i)
    mpq_init(rational_coefficient[i]);

  mpz_init(den_lcm);

  if (verbose)
    fprintf(output_file, "Constraints:\n");

  /* Set up the row (ordinary) constraints. */
  num_rows = lpx_get_num_rows(lp);
  for (row = 1; row <= num_rows; ++row) {
    /* Initialize the least common multiple computation. */
    mpz_set_si(den_lcm, 1);
    /* Set `nz' to the number of non-zero coefficients. */
    nz = lpx_get_mat_row(lp, row, coefficient_index, coefficient_value);
    for (i = 1; i <= nz; ++i) {
      mpq_set_d(rational_coefficient[i], coefficient_value[i]);
      /* Update den_lcm. */
      mpz_lcm(den_lcm, den_lcm, mpq_denref(rational_coefficient[i]));
    }
    lpx_get_row_bnds(lp, row, &type, &lb, &ub);
    mpq_set_d(rational_lb, lb);
    mpz_lcm(den_lcm, den_lcm, mpq_denref(rational_lb));
    mpq_set_d(rational_ub, ub);
    mpz_lcm(den_lcm, den_lcm, mpq_denref(rational_ub));

    ppl_new_LinExpression_with_dimension(&ppl_le, dimension);

    for (i = 1; i <= nz; ++i) {
      mpz_mul(tmp_z, den_lcm, mpq_numref(rational_coefficient[i]));
      ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
      ppl_LinExpression_add_to_coefficient(ppl_le, coefficient_index[i]-1,
					   ppl_coeff);
    }

    add_constraints(ppl_le, type, rational_lb, rational_ub, den_lcm, ppl_cs);

    ppl_delete_LinExpression(ppl_le);
  }

  /*
    FIXME: here we could build the polyhedron and minimize it before
    adding the variable bounds.
  */

  /* Set up the columns constraints, i.e., variable bounds. */
  for (column = 1; column <= dimension; ++column) {

    lpx_get_col_bnds(lp, column, &type, &lb, &ub);

    mpq_set_d(rational_lb, lb);
    mpq_set_d(rational_ub, ub);

    /* Initialize the least common multiple computation. */
    mpz_set_si(den_lcm, 1);
    mpz_lcm(den_lcm, den_lcm, mpq_denref(rational_lb));
    mpz_lcm(den_lcm, den_lcm, mpq_denref(rational_ub));

    ppl_new_LinExpression_with_dimension(&ppl_le, dimension);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, den_lcm);
    ppl_LinExpression_add_to_coefficient(ppl_le, column-1, ppl_coeff);

    add_constraints(ppl_le, type, rational_lb, rational_ub, den_lcm, ppl_cs);

    ppl_delete_LinExpression(ppl_le);
  }

  /* Create the polyhedron and get rid of the constraint system. */
  ppl_new_C_Polyhedron_recycle_ConSys(&ppl_ph, ppl_cs);
  ppl_delete_ConSys(ppl_cs);

  if (print_timings) {
    fprintf(stderr, "Time to create a PPL polyhedron: ");
    print_clock(stderr);
    fprintf(stderr, " s\n");
    start_clock();
  }

  empty = ppl_Polyhedron_check_empty(ppl_ph);

  if (print_timings) {
    fprintf(stderr, "Time to check for emptiness: ");
    print_clock(stderr);
    fprintf(stderr, " s\n");
    start_clock();
  }

  if (empty) {
    fprintf(output_file, "Unfeasible problem.\n");
    /* FIXME: check!!! */
    return;
  }

  /* Deal with the objective function. */
  objective = (mpq_t*) malloc((dimension+1)*sizeof(mpq_t));

  /* Initialize the least common multiple computation. */
  mpz_set_si(den_lcm, 1);

  mpq_init(objective[0]);
  mpq_set_d(objective[0], lpx_get_obj_c0(lp));
  for (i = 1; i <= dimension; ++i) {
    mpq_init(objective[i]);
    mpq_set_d(objective[i], lpx_get_col_coef(lp, i));
    /* Update den_lcm. */
    mpz_lcm(den_lcm, den_lcm, mpq_denref(objective[i]));
  }

  /* Set the LinExpression ppl_objective_le to be the objective function. */
  ppl_new_LinExpression_with_dimension(&ppl_objective_le, dimension);
  /* The inhomogeneous term is completely useless for our purpose. */
  if (verbose) {
    first_printed = 1;
    fprintf(output_file, "Objective function:\n");
    mpz_mul(tmp_z, den_lcm, mpq_numref(objective[0]));
    if (mpz_sgn(tmp_z) != 0) {
      mpz_out_str(output_file, 10, tmp_z);
      first_printed = 0;
    }
  }
  for (i = 1; i <= dimension; ++i) {
    mpz_mul(tmp_z, den_lcm, mpq_numref(objective[i]));
    if (verbose)
      if (mpz_sgn(tmp_z) != 0) {
	if (first_printed)
	  first_printed = 0;
	else {
	  if (mpz_sgn(tmp_z) > 0)
	    fprintf(output_file, "+");
	}
	mpz_out_str(output_file, 10, tmp_z);
	fprintf(output_file, "*");
	print_variable(output_file, i-1);
      }

    if (!maximize)
      mpz_neg(tmp_z, tmp_z);
    ppl_assign_Coefficient_from_mpz_t(ppl_coeff, tmp_z);
    ppl_LinExpression_add_to_coefficient(ppl_objective_le, i-1, ppl_coeff);
  }

  if (verbose)
    fprintf(output_file, "\n%s\n",
	    (maximize ? "Maximizing." : "Minimizing."));

  /* Check whether the problem is unbounded. */
  unbounded = !ppl_Polyhedron_bounds_from_above(ppl_ph, ppl_objective_le);

  if (print_timings) {
    fprintf(stderr, "Time to check for unboundedness: ");
    print_clock(stderr);
    fprintf(stderr, " s\n");
    start_clock();
  }

  if (unbounded) {
    fprintf(output_file, "Unbounded problem.\n");
    /* FIXEM: check!!! */
    return;
  }

  ppl_Polyhedron_generators(ppl_ph, &ppl_const_gs);

  if (print_timings) {
    fprintf(stderr, "Time to compute the polyhedron's generators: ");
    print_clock(stderr);
    fprintf(stderr, " s\n");
    start_clock();
  }

  candidate = (mpq_t*) malloc((dimension)*sizeof(mpq_t));
  for (i = 0; i < dimension; ++i)
    mpq_init(candidate[i]);

  mpq_init(optimum);
  ppl_new_GenSys_const_iterator(&ogit);
  first_candidate = 1;

  ppl_new_GenSys_const_iterator(&git1);
  ppl_new_GenSys_const_iterator(&git2);
  ppl_GenSys_begin(ppl_const_gs, git1);
  ppl_GenSys_end(ppl_const_gs, git2);
  while (ppl_GenSys_const_iterator_equal_test(git1, git2) == 0) {
    ppl_GenSys_const_iterator_dereference(git1, &ppl_const_g);
    if (ppl_Generator_type(ppl_const_g) == PPL_GENERATOR_TYPE_POINT) {
      ppl_Generator_divisor(ppl_const_g, ppl_coeff);
      ppl_Coefficient_to_mpz_t(ppl_coeff, tmp_z);
      for (i = 0; i < dimension; ++i) {
	mpz_set(mpq_denref(candidate[i]), tmp_z);
	ppl_Generator_coefficient(ppl_const_g, i, ppl_coeff);
	ppl_Coefficient_to_mpz_t(ppl_coeff, mpq_numref(candidate[i]));
      }

      /* Here we have a candidate.  Evaluate the objective function. */
      mpq_set(tmp1_q, objective[0]);
      for (i = 0; i < dimension; ++i) {
	mpq_mul(tmp2_q, candidate[i], objective[i+1]);
	mpq_add(tmp1_q, tmp1_q, tmp2_q);
      }

      if (first_candidate
	  || (maximize && (mpq_cmp(tmp1_q, optimum) > 0))
	  || (!maximize && (mpq_cmp(tmp1_q, optimum) < 0))) {
	first_candidate = 0;
	mpq_set(optimum, tmp1_q);
	ppl_assign_GenSys_const_iterator_from_GenSys_const_iterator(ogit,
								      git1);
      }
    }
    ppl_GenSys_const_iterator_increment(git1);
  }

  assert(!first_candidate);

  fprintf(output_file, "Optimum value:\n%f\n", mpq_get_d(optimum));
  fprintf(output_file, "Optimum location:\n");
  ppl_GenSys_const_iterator_dereference(ogit, &ppl_const_g);
  ppl_Generator_divisor(ppl_const_g, ppl_coeff);
  ppl_Coefficient_to_mpz_t(ppl_coeff, tmp_z);
  for (i = 0; i < dimension; ++i) {
    mpz_set(mpq_denref(tmp1_q), tmp_z);
    ppl_Generator_coefficient(ppl_const_g, i, ppl_coeff);
    ppl_Coefficient_to_mpz_t(ppl_coeff, mpq_numref(tmp1_q));
    print_variable(output_file, i);
    fprintf(output_file, " = %f\n", mpq_get_d(tmp1_q));
  }

  free(candidate);

  ppl_delete_Polyhedron(ppl_ph);
  lpx_delete_prob(lp);
}

static void
error_handler(enum ppl_enum_error_code code,
	      const char* description) {
  fatal("PPL error code %d\n%s", code, description);
}

int
main(int argc, char* argv[]) {
  program_name = argv[0];
  if (ppl_initialize() < 0)
    fatal("cannot initialize the Parma Polyhedra Library");

  if (ppl_set_error_handler(error_handler) < 0)
    fatal("cannot install the custom error handler");

  /* Process command line options */
  process_options(argc, argv);

  /* Initialize globals. */
  mpz_init(tmp_z);
  mpq_init(tmp1_q);
  mpq_init(tmp2_q);
  ppl_new_Coefficient(&ppl_coeff);

  if (max_seconds_of_cpu_time > 0)
    set_alarm_on_cpu_time(max_seconds_of_cpu_time, my_timeout);

  if (max_bytes_of_virtual_memory > 0)
    limit_virtual_memory(max_bytes_of_virtual_memory);

  while (optind < argc)
    solve(argv[optind++]);

  /* Finalize globals. */
  ppl_delete_Coefficient(ppl_coeff);
  mpq_clear(tmp2_q);
  mpq_clear(tmp1_q);
  mpz_clear(tmp_z);

  /* Close output file, if any. */
  if (output_argument)
    fclose(output_file);

  my_exit(0);

  return 0;
}
