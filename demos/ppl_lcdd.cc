/* A sort of clone of the cddlib test program `lcdd'.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "config.h"
#include "ppl_install.hh"
#include <cstdarg>
#include <csignal>
#include <cerrno>
#include <iostream>
#include <fstream>

#ifdef HAVE_GETOPT_H
# include <getopt.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

namespace PPL = Parma_Polyhedra_Library;

#if PPL_VERSION_MAJOR == 0 && PPL_VERSION_MINOR < 6
# error "PPL version 0.6 or following is required"
#endif

namespace {

//const char* ppl_source_version = PPL_VERSION;

struct option long_options[] = {
  {"help",           no_argument,       0, 'h'},
  {"max-cpu",        required_argument, 0, 'C'},
  {"max-memory",     required_argument, 0, 'V'},
  {"output",         required_argument, 0, 'o'},
  {"timings",        no_argument,       0, 't'},
  {"verbose",        no_argument,       0, 'v'},
  {0, 0, 0, 0}
};

static const char* usage_string
= "Usage: %s [OPTION]... [FILE]...\n\n"
"  -CSECS, --max-cpu=SECS  limits CPU usage to SECS seconds\n"
"  -VMB, --max-memory=MB   limits memory usage to MB megabytes\n"
"  -h, --help              prints this help text to stderr\n"
"  -oPATH, --output=PATH   appends output to PATH\n"
"  -t, --timings           prints timings to stderr\n"
"  -v, --verbose           produces lots of output\n";

#define OPTION_LETTERS "bC:V:ho:tv"

const char* program_name = 0;

unsigned long max_seconds_of_cpu_time = 0;
unsigned long max_bytes_of_virtual_memory = 0;
const char* input_file_name = 0;
const char* output_file_name = 0;
FILE* output_file = NULL;
bool print_timings = false;
bool verbose = false;

void
fatal(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  fprintf(stderr, "%s: ", program_name);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

void
error(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  fprintf(stderr, "%s: in `%s': ", program_name, input_file_name);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

void
warning(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  fprintf(stderr, "%s: Warning: in `%s': ", program_name, input_file_name);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
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
  #error "Either SA_ONESHOT or SA_RESETHAND must be defined."
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

void
timeout(int) {
  fprintf(stderr, "TIMEOUT\n");
  if (output_file_name)
    fprintf(output_file, "TIMEOUT\n");
  exit(0);
}

void
process_options(int argc, char* argv[]) {
  while (true) {
    int option_index = 0;
    int c = getopt_long(argc, argv, OPTION_LETTERS, long_options,
			&option_index);
    if (c == EOF)
      break;


    char* endptr;
    long l;
    switch (c) {
    case 0:
      break;

    case '?':
    case 'h':
      fprintf(stderr, usage_string, argv[0]);
      exit(0);
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
      output_file_name = optarg;
      break;

    case 't':
      print_timings = true;
      break;

    case 'v':
      verbose = true;
      break;

    default:
      abort();
    }
  }

  if (optind >= argc) {
    if (verbose)
      fprintf(stderr,
	      "Parma Polyhedra Library version:\n%s\n\n"
	      "Parma Polyhedra Library banner:\n%s",
	      PPL::version(),
	      PPL::banner());
    else
      fatal("no input files");
  }

  if (argc - optind > 1)
    /* We have multiple input files. */
    fatal("only one input file is accepted");

  if (output_file_name) {
    output_file = fopen(output_file_name, "a");
    if (output_file == NULL)
      fatal("cannot open output file `%s'", output_file_name);
  }
  else
    output_file = stdout;
}

void
normalize(const std::vector<mpq_class>& source, std::vector<mpz_class>& dest) {
  unsigned n = source.size();
  mpz_class lcm = 1;
  for (unsigned i = 0; i < n; ++i)
    mpz_lcm(lcm.get_mpz_t(), lcm.get_mpz_t(), source[i].get_den().get_mpz_t());
  for (unsigned i = 0; i < n; ++i)
    dest[i] = lcm * source[i].get_num();
}

enum Number_Type { INTEGER, RATIONAL, REAL };

void
read_coefficients(std::istream& input,
		  Number_Type number_type,
		  std::vector<mpz_class>& coefficients) {
  unsigned num_coefficients = coefficients.size();
  switch (number_type) {
  case INTEGER: {
    for (unsigned i = 0; i < num_coefficients; ++i)
      if (!(input >> coefficients[i]))
	error("missing or invalid integer coefficient");
    break;
  }
  case RATIONAL: {
    std::vector<mpq_class> rational_coefficients(num_coefficients);
    for (unsigned i = 0; i < num_coefficients; ++i)
      if (!(input >> rational_coefficients[i]))
	error("missing or invalid rational coefficient");
    normalize(rational_coefficients, coefficients);
    break;
  }
  case REAL: {
    std::vector<mpq_class> rational_coefficients(num_coefficients);
    for (unsigned i = 0; i < num_coefficients; ++i) {
      double d;
      if (!(input >> d))
	error("missing or invalid real coefficient");
      rational_coefficients[i] = mpq_class(d);
    }
    normalize(rational_coefficients, coefficients);
    break;
  }
  default:
    fatal("internal error: wrong number type");
  }
}

void
slurp(const char* path) {
  std::ifstream input;
  input.open(path, std::ios_base::in);
  if (!input)
    fatal("cannot open input file `%s'", path);
  input_file_name = path;

  enum Representation { H, V };
  // By default we have an H-representation.
  Representation rep = H;
  std::string s;

  std::set<unsigned> linearity;
  while (true) {
    if (!(input >> s))
      error("premature end of file while seeking for `begin'");

    if (s == "V-representation")
      rep = V;
    else if (s == "H-representation")
      rep = H;
    else if (s == "linearity") {
      unsigned num_linear;
      if (!(input >> num_linear))
	error("missing or invalid number of linearity indexes");
      while (num_linear--) {
	unsigned i;
	if (!(input >> i))
	  error("missing or invalid linearity index");
	linearity.insert(i);
      }
      if (verbose) {
	std::cerr << "Linearity: ";
	for (std::set<unsigned>::const_iterator j = linearity.begin(),
	       jend = linearity.end(); j != jend; ++j)
	  std::cerr << *j << " ";
	std::cerr << std::endl;
      }
    }
    else if (s == "begin")
      break;
    else
      // A comment: skip to end of line.
      input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  unsigned num_rows;
  if (!(input >> num_rows))
    error("illegal or missing number of rows");

  unsigned num_columns;
  if (!(input >> num_columns))
    error("illegal or missing number of columns");

  if (!(input >> s))
    error("missing number type");
  Number_Type number_type = INTEGER;
  if (s == "integer")
    number_type = INTEGER;
  else if (s == "rational")
    number_type = RATIONAL;
  else if (s == "real")
    number_type = REAL;
  else
    error("illegal number type `%s'", s.c_str());

  if (verbose)
    std::cerr << "Problem dimension: " << num_rows << " x " << num_columns
	      << "; number type: " << s
	      << std::endl;

  PPL::ConSys cs;
  PPL::GenSys gs;

  if (rep == V) {
    std::vector<mpz_class> coefficients(num_columns-1);
    for (unsigned i = 0; i < num_rows; ++i) {
      int vertex_marker;
      if (!(input >> vertex_marker) || vertex_marker < 0 || vertex_marker > 1)
	error("illegal or missing vertex marker");
      read_coefficients(input, number_type, coefficients);
      PPL::LinExpression e;
      for (unsigned i = num_columns-1; i-- > 0; )
	e += coefficients[i] * PPL::Variable(i);
      if (vertex_marker == 1)
	gs.insert(point(e));
      else
	gs.insert(ray(e));
    }
    if (verbose) {
      using namespace PPL::IO_Operators;
      std::cerr << "Generator system:\n" << gs << std::endl;
    }
  }
  else {
    assert(rep == H);
    std::vector<mpz_class> coefficients(num_columns);
    for (unsigned i = 1; i <= num_rows; ++i) {
      read_coefficients(input, number_type, coefficients);
      PPL::LinExpression e;
      for (unsigned i = num_columns; i-- > 1; )
	e += coefficients[i] * PPL::Variable(i-1);
      cs.insert(e <= coefficients[0]);
    }
    if (verbose) {
      using namespace PPL::IO_Operators;
      std::cerr << "Constraint system:\n" << cs << std::endl;
    }
  }

  if (!(input >> s))
    error("premature end of file while seeking for `end'");

  if (s != "end")
    error("`%s' found while seeking for `end'", s.c_str());

  while (input >> s) {
    warning("ignoring command `%s'", s.c_str());
    input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  // If we are still here, we just make a conversion.
  if (rep == V) {
    PPL::C_Polyhedron ph(gs);
    ph.constraints();
  }
  else {
    PPL::C_Polyhedron ph(cs);
    ph.generators();
  }
}

} // namespace

#define PPL_VERSION "0.6pre5"

int
main(int argc, char* argv[]) {
  program_name = argv[0];

  if (strcmp(PPL_VERSION, PPL::version()) != 0)
    fatal("was compiled with PPL version %s, but linked with version %s",
	  PPL_VERSION, PPL::version());

  //if (ppl_io_set_variable_output_function(variable_output_function) < 0)
  //  fatal("cannot install the custom variable output function");

  // Process command line options.
  process_options(argc, argv);

  if (max_seconds_of_cpu_time > 0)
    set_alarm_on_cpu_time(max_seconds_of_cpu_time, timeout);

  if (max_bytes_of_virtual_memory > 0)
    limit_virtual_memory(max_bytes_of_virtual_memory);

  while (optind < argc)
    slurp(argv[optind++]);

  // Close output file, if any.
  if (output_file_name)
    fclose(output_file);

  return 0;
}
