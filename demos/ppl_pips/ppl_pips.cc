/* A sort of clone of the cddlib test program `lcdd'.
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

//#define USE_PIPLIB 1

#if (!defined(USE_PPL) && !defined(USE_PIPLIB))
#define USE_PPL 1
#elif (defined(USE_PPL) && defined(USE_PIPLIB))
#error "Exactly one among USE_PPL and USE_PIPLIB must be defined."
#endif

#if defined(USE_PPL)

#include "ppl.hh"

namespace PPL = Parma_Polyhedra_Library;

#if PPL_VERSION_MAJOR == 0 && PPL_VERSION_MINOR < 6
#error "PPL version 0.6 or following is required"
#endif

typedef PPL::C_Polyhedron POLYHEDRON_TYPE;

#if !PPL_CXX_SUPPORTS_ATTRIBUTE_WEAK
extern "C" void
ppl_set_GMP_memory_allocation_functions(void) {
}
#endif

#elif defined(USE_PIPLIB)

#error "PIPlib not supported yet"

#endif

#include "timings.hh"
#include <gmpxx.h>
#include <vector>
#include <set>
#include <limits>
#include <cassert>
#include <cstdarg>
#include <csignal>
#include <cerrno>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>

#ifdef PPL_HAVE_GETOPT_H
#include <getopt.h>

// Try to accommodate non-GNU implementations of `getopt()'.
#if !defined(no_argument) && defined(NO_ARG)
#define no_argument NO_ARG
#endif

#if !defined(required_argument) && defined(REQUIRED_ARG)
#define required_argument REQUIRED_ARG
#endif

#if !defined(optional_argument) && defined(OPTIONAL_ARG)
#define optional_argument OPTIONAL_ARG
#endif

#endif // defined(PPL_HAVE_GETOPT_H)

#ifdef PPL_HAVE_UNISTD_H
// Include this for `getopt()': especially important if we do not have
// <getopt.h>.
# include <unistd.h>
#endif

#ifdef PPL_HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef PPL_HAVE_SYS_RESOURCE_H
// This should be included after <time.h> and <sys/time.h> so as to make
// sure we have the definitions for, e.g., `ru_utime'.
# include <sys/resource.h>
#endif

namespace {

void
pip_display_sol(std::ostream& out,
                const Parma_Polyhedra_Library::PIP_Tree pip,
                const Parma_Polyhedra_Library::Variables_Set& params,
                const Parma_Polyhedra_Library::Variables_Set& vars,
                Parma_Polyhedra_Library::dimension_type space_dimension,
                int indent=0) {
  using namespace std;
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!pip) {
    out << setw(indent*2) << "" << "_|_" << endl;
  } else {
    Variables_Set parameters(params);
    dimension_type new_params
        = pip->insert_artificials(parameters, space_dimension);
    if (new_params > 0) {
      PIP_Tree_Node::Artificial_Parameter_Sequence::const_iterator i, i_end;
      i_end = pip->art_parameter_end();
      for (i=pip->art_parameter_begin(); i!=i_end; ++i) {
        out << setw(indent*2) << "" << "Parameter "
             << Linear_Expression(Variable(space_dimension++))
             << " = " << *i << endl;
      }
    }
    const Constraint_System &constraints = pip->constraints();
    bool constraints_empty = constraints.empty();
    if (!constraints_empty) {
      out << setw(indent*2) << "" << "if ";
      Constraint_System::const_iterator begin = constraints.begin();
      Constraint_System::const_iterator end = constraints.end();
      Constraint_System::const_iterator i;
      for (i = begin; i != end; ++i)
        out << ((i==begin)?"":" and ") << *i;
      out << " then" << endl;
    }
    const PIP_Decision_Node* dn = pip->as_decision();
    if (dn) {
      pip_display_sol(out, dn->child_node(true), parameters, vars,
                      space_dimension, indent+1);
      out << setw(indent*2) << "" << "else" << endl;
      pip_display_sol(out, dn->child_node(false), parameters, vars,
                      space_dimension, indent+1);
    } else {
      const PIP_Solution_Node* sn = pip->as_solution();
      Variables_Set::const_iterator begin = vars.begin();
      Variables_Set::const_iterator end = vars.end();
      Variables_Set::const_iterator i;
      out << setw(indent*2+(constraints_empty?0:2)) << "" << "{";
      for (i=begin; i!=end; ++i)
        out << ((i==begin)?"":" ; ")
             << sn->parametric_values(Variable(*i), parameters);
      out << "}" << endl;
      if (!constraints_empty) {
        out << setw(indent*2) << "" << "else" << endl;
        out << setw(indent*2+2) << "" << "_|_" << endl;
      }
    }
  }
}

class PIP_Parser {
public:
  PIP_Parser() : pip(), comment() {
  }

  ~PIP_Parser() {
  }

  const PPL::PIP_Problem& problem() const {
    return pip;
  }

  bool read(std::istream& in) {
    using namespace PPL;
    dimension_type num_params;
    dimension_type num_ctx_rows;
    dimension_type num_vars;
    dimension_type num_constraints;
    int tmp = 0;
    int solve_integer = 0;
    dimension_type bignum_column;
    dimension_type i, j;

    if (!expect(in, '('))
      return false;
    if (!expect(in, '('))
      return false;
    if (!read_comment(in))
      return false;

    in >> num_vars >> num_params >> num_constraints >> num_ctx_rows >> tmp
       >> solve_integer;
    bignum_column = (tmp == -1) ? not_a_dimension() : tmp;
#if 0
    std::cout << "num_vars = " << num_vars << std::endl;
    std::cout << "num_params = " << num_params << std::endl;
    std::cout << "num_constraints = " << num_constraints << std::endl;
    std::cout << "num_ctx_rows = " << num_ctx_rows << std::endl;
    std::cout << "bignum_column = " << tmp << std::endl;
    std::cout << "solve_integer = " << solve_integer << std::endl;
#endif
    if (bignum_column != not_a_dimension()) {
      std::cerr << "No support for bignums yet." << std::endl;
      return false;
    }
    if (solve_integer != 1) {
      std::cerr << "Can only solve integer problems." << std::endl;
      return false;
    }


    if (!expect(in, '('))
      return false;
    dimension_type constraint_width = num_vars+num_params+1;
    Coefficient constraints[num_constraints][constraint_width];
    for (i=0; i<num_constraints; ++i)
      if (!read_vector(in, constraint_width, constraints[i]))
        return false;

    Coefficient context[num_ctx_rows][num_params+1];
    for (i=0; i<num_ctx_rows; ++i)
      if (!read_vector(in, num_params+1, context[i]))
        return false;

    pip.add_space_dimensions_and_embed(num_vars, num_params);
    for (i=0; i<num_constraints; ++i) {
      Linear_Expression e;
      j = 0;
      for (; j<num_vars; ++j) {
        e += constraints[i][j] * Variable(j);
      }
      e += constraints[i][j++];
      for (; j<constraint_width; ++j) {
        e += constraints[i][j] * Variable(j-1);
      }
      pip.add_constraint(Constraint(e >= 0));
    }
    for (i=0; i<num_ctx_rows; ++i) {
      Linear_Expression e;
      for (j=0; j<num_params; ++j) {
        e += context[i][j] * Variable(num_vars+j);
      }
      pip.add_constraint(Constraint(e + context[i][j] >= 0));
    }
    return true;
  }

  // output the solution in PIPlib-like format
  /* void output_solution_piplib(std::ostream& out) {
    const PPL::Variables_Set& params = pip.parameter_space_dimensions();
    PPL::Variables_Set vars;
    for (PPL::dimension_type i=0; i<pip.space_dimension(); ++i) {
      if (params.count(i) == 0)
        vars.insert(i);
    }
    const PPL::PIP_Tree solution = pip.solution();
    out << "((" << comment << ")\n(";
    pip_disp_sol_mat(out, solution, params, vars, pip.space_dimension());
    out << "))" << std::endl;
  } */

  // output the solution in PIPlib-like format
  void output_solution_tree(std::ostream& out) {
    const PPL::Variables_Set& params = pip.parameter_space_dimensions();
    PPL::Variables_Set vars;
    for (PPL::dimension_type i=0; i<pip.space_dimension(); ++i) {
      if (params.count(i) == 0)
        vars.insert(i);
    }
    const PPL::PIP_Tree solution = pip.solution();
    pip_display_sol(out, solution, params, vars, pip.space_dimension());
  }

private:
  bool expect(std::istream& in, char c) {
    char a;
    do {
      in >> a;
    } while (a != c && in.good());
    return a == c;
  }

  bool read_comment(std::istream& in) {
    comment = "";
    int count = 1;
    char c;
    do {
      if (!in.get(c))
        return false;
      if (c == '(')
        ++count;
      else if (c == ')')
        --count;
      if (count > 0)
        comment += c;
    } while (count > 0);
    return true;
  }

  bool read_vector(std::istream& in, PPL::dimension_type size,
                   PPL::Coefficient tab[]) {
    if (!expect(in, '#'))
      return false;
    if (!expect(in, '['))
      return false;
    std::string s;
    if (getline(in, s, ']').bad())
      return false;
    std::istringstream iss(s);
    for (PPL::dimension_type i=0; i<size; ++i)
      if (!(iss >> tab[i]))
        return false;
    return true;
  }

  // The problem object
  PPL::PIP_Problem pip;

  // The comment string in the source file
  std::string comment;
}; // class PIP_Parser

#ifdef PPL_HAVE_GETOPT_H
struct option long_options[] = {
  {"max-cpu",        required_argument, 0, 'C'},
  {"max-memory",     required_argument, 0, 'R'},
  {"help",           no_argument,       0, 'h'},
  {"output",         required_argument, 0, 'o'},
  {"timings",        no_argument,       0, 't'},
  {"verbose",        no_argument,       0, 'v'},
#if defined(USE_PPL)
  {"version",        no_argument,       0, 'V'},
  {"check",          required_argument, 0, 'c'},
#endif
  {0, 0, 0, 0}
};
#endif

static const char* usage_string
= "Usage: %s [OPTION]... [FILE]\n"
"Reads the definition of a Parametric Integer Programming problem\n"
"and displays the lexicographic minimum in terms of the values of the\n"
"parameters.\n\n"
"Options:\n"
"  -RMB, --max-memory=MB   limits memory usage to MB megabytes\n"
"  -h, --help              prints this help text to stdout\n"
"  -oPATH, --output=PATH   appends output to PATH\n"
"  -t, --timings           prints timings to stderr\n"
"  -v, --verbose           produces lots of output\n"
#if defined(USE_PPL)
"  -V, --version           prints version information to stdout\n"
"  -cPATH, --check=PATH    checks if the result is equal to what is in PATH\n"
#endif
#ifndef PPL_HAVE_GETOPT_H
"\n"
"NOTE: this version does not support long options.\n"
#endif
"\n"
"Report bugs to <ppl-devel@cs.unipr.it>.\n";

#if defined(USE_PPL)
#define OPTION_LETTERS "R:ho:tvVc:"
#else
#define OPTION_LETTERS "R:ho:tv"
#endif

const char* program_name = 0;

unsigned long max_bytes_of_virtual_memory = 0;
bool print_timings = false;
bool verbose = false;
const char* check_file_name = 0;

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

const char* input_file_name = 0;
std::istream* input_stream_p = 0;

void
set_input(const char* file_name) {
  if (input_stream_p && *input_stream_p != std::cin)
    delete input_stream_p;

  if (file_name) {
    input_stream_p = new std::ifstream(file_name, std::ios_base::in);
    if (!*input_stream_p)
      fatal("cannot open input file `%s'", file_name);
    input_file_name = file_name;
  }
  else {
    input_stream_p = &std::cin;
    input_file_name = "<cin>";
  }
}

std::istream&
input() {
  assert(input_stream_p);
  return *input_stream_p;
}

const char* output_file_name = 0;
std::ostream* output_stream_p = 0;

void
set_output(const char* file_name) {
  if (output_stream_p && *output_stream_p != std::cout)
    delete output_stream_p;

  if (file_name) {
    output_stream_p = new std::ofstream(file_name,
					std::ios_base::out
					| std::ios_base::app);
    if (!*output_stream_p)
      fatal("cannot open output file `%s'", file_name);
    output_file_name = file_name;
  }
  else {
    output_stream_p = &std::cout;
    output_file_name = "<cout>";
  }
}

std::ostream&
output() {
  assert(output_stream_p);
  return *output_stream_p;
}

} // namespace

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

#if PPL_HAVE_DECL_RLIMIT_AS

void
limit_virtual_memory(const unsigned bytes) {
  struct rlimit t;

  if (getrlimit(RLIMIT_AS, &t) != 0)
    fatal("getrlimit failed: %s", strerror(errno));

  if (bytes < t.rlim_cur) {
    t.rlim_cur = bytes;
    if (setrlimit(RLIMIT_AS, &t) != 0)
      fatal("setrlimit failed: %s", strerror(errno));
  }
}

#else

void
limit_virtual_memory(unsigned) {
}

#endif // !PPL_HAVE_DECL_RLIMIT_AS

void
process_options(int argc, char* argv[]) {
  while (true) {
#ifdef PPL_HAVE_GETOPT_H
    int option_index = 0;
    int c = getopt_long(argc, argv, OPTION_LETTERS, long_options,
			&option_index);
#else
    int c = getopt(argc, argv, OPTION_LETTERS);
#endif

    if (c == EOF)
      break;

    char* endptr;
    long l;
    switch (c) {
    case 0:
      break;

    case '?':
    case 'h':
      fprintf(stdout, usage_string, argv[0]);
      exit(0);
      break;

    case 'R':
      l = strtol(optarg, &endptr, 10);
      if (*endptr || l < 0)
	fatal("a non-negative integer must follow `-R'");
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

#if defined(USE_PPL)

    case 'V':
      fprintf(stdout, "%s\n", PPL_VERSION);
      exit(0);
      break;

    case 'c':
      check_file_name = optarg;
      break;

#endif

    default:
      abort();
    }
  }

  if (argc - optind > 1)
    // We have multiple input files.
    fatal("at most one input file is accepted");

  // We have one input files.
  if (optind < argc)
    input_file_name = argv[optind];
  else
    // If no input files have been specified: we will read from standard input.
    assert(input_file_name == 0);
}

void
maybe_start_clock() {
  if (print_timings)
    start_clock();
}

void
maybe_print_clock() {
  if (print_timings) {
    std::cerr << input_file_name << " ";
    print_clock(std::cerr);
    std::cerr << std::endl;
  }
}

int
main(int argc, char* argv[]) try {
  program_name = argv[0];

#if defined(USE_PPL)
  if (strcmp(PPL_VERSION, PPL::version()) != 0)
    fatal("was compiled with PPL version %s, but linked with version %s",
	  PPL_VERSION, PPL::version());

  if (verbose)
    std::cerr << "Parma Polyhedra Library version:\n" << PPL::version()
	      << "\n\nParma Polyhedra Library banner:\n" << PPL::banner()
	      << std::endl;
#endif

  // Process command line options.
  process_options(argc, argv);

  if (max_bytes_of_virtual_memory > 0)
    limit_virtual_memory(max_bytes_of_virtual_memory);

  // Set up the input and output streams.
  set_input(input_file_name);
  set_output(output_file_name);

//  POLYHEDRON_TYPE ph;
//  Representation rep = read_polyhedron(input(), ph);
  PIP_Parser parser;
  if (!parser.read(*input_stream_p))
    return 1;

  maybe_start_clock();

  // Compute the dual simplex on the problem.
  const PPL::PIP_Problem& pip = parser.problem();

  pip.solve();

#if defined(USE_PPL) || defined(USE_POLKA)
  maybe_print_clock();
#endif

  // Write the solution.
  parser.output_solution_tree(*output_stream_p);

  return 0;
}
catch (const std::bad_alloc&) {
  fatal("out of memory");
  exit(1);
}
catch (const std::overflow_error& e) {
  fatal("arithmetic overflow (%s)", e.what());
  exit(1);
}
catch (...) {
  fatal("internal error: please submit a bug report to ppl-devel@cs.unipr.it");
  exit(1);
}
