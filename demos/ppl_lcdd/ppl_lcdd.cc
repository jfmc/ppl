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
  {"max-cpu",        required_argument, 0, 'C'},
  {"max-memory",     required_argument, 0, 'V'},
  {"help",           no_argument,       0, 'h'},
  {"output",         required_argument, 0, 'o'},
  {"timings",        no_argument,       0, 't'},
  {"verbose",        no_argument,       0, 'v'},
  {"check",          required_argument, 0, 'c'},
  {0, 0, 0, 0}
};

static const char* usage_string
= "Usage: %s [OPTION]... [FILE]...\n\n"
"  -CSECS, --max-cpu=SECS  limits CPU usage to SECS seconds\n"
"  -VMB, --max-memory=MB   limits memory usage to MB megabytes\n"
"  -h, --help              prints this help text to stderr\n"
"  -oPATH, --output=PATH   appends output to PATH\n"
"  -t, --timings           prints timings to stderr\n"
"  -v, --verbose           produces lots of output\n"
"  -cPATH, --check=PATH    checks if the result is equal to what is in PATH\n";

#define OPTION_LETTERS "C:V:ho:tvc:"

const char* program_name = 0;

unsigned long max_seconds_of_cpu_time = 0;
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

namespace {

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
    output_stream_p = new std::ofstream(file_name, std::ios_base::out);
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

void
set_alarm_on_cpu_time(const unsigned seconds, void (*handler)(int)) {
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

void
timeout(int) {
  try {
    std::cerr << "TIMEOUT"
	      << std::endl;
  }
  catch (...) {
  }

  try {
    if (output_file_name)
      output() << "TIMEOUT"
	       << std::endl;
  }
  catch (...) {
  }

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

    case 'c':
      check_file_name = optarg;
      break;

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
normalize(const std::vector<mpq_class>& source,
	  std::vector<mpz_class>& dest,
	  mpz_class& denominator) {
  unsigned n = source.size();
  denominator = 1;
  for (unsigned i = 0; i < n; ++i)
    mpz_lcm(denominator.get_mpz_t(),
	    denominator.get_mpz_t(),
	    source[i].get_den().get_mpz_t());
  for (unsigned i = 0; i < n; ++i)
    dest[i] = denominator*source[i];
}

template <typename T>
bool
guarded_read(std::istream& in, T& x) {
  try {
    return in >> x;
  }
  catch (...) {
    return false;
  }
}

template <typename T>
void
guarded_write(std::ostream& out, const T& x) {
  bool succeeded = false;
  try {
    succeeded = out << x;
  }
  catch (...) {
  }
  if (!succeeded)
    fatal("cannot write to output file `%s'", output_file_name);
}

enum Number_Type { INTEGER, RATIONAL, REAL };

void
read_coefficients(std::istream& in,
		  const Number_Type number_type,
		  std::vector<mpz_class>& coefficients,
		  mpz_class& denominator) {
  unsigned num_coefficients = coefficients.size();
  switch (number_type) {
  case INTEGER: {
    for (unsigned i = 0; i < num_coefficients; ++i)
      if (!guarded_read(in, coefficients[i]))
	error("missing or invalid integer coefficient");
    denominator = 1;
    break;
  }
  case RATIONAL: {
    std::vector<mpq_class> rational_coefficients(num_coefficients);
    for (unsigned i = 0; i < num_coefficients; ++i)
      if (!guarded_read(in, rational_coefficients[i]))
	error("missing or invalid rational coefficient");
    normalize(rational_coefficients, coefficients, denominator);
    break;
  }
  case REAL: {
    std::vector<mpq_class> rational_coefficients(num_coefficients);
    for (unsigned i = 0; i < num_coefficients; ++i) {
      double d;
      if (!guarded_read(in, d))
	error("missing or invalid real coefficient");
      rational_coefficients[i] = mpq_class(d);
    }
    normalize(rational_coefficients, coefficients, denominator);
    break;
  }
  default:
    fatal("internal error: wrong number type");
  }
}

void
read_indexes_set(std::istream& in,
		 std::set<unsigned>& dest,
		 const char* what) {
  assert(dest.empty());
  unsigned num_elements;
  if (!guarded_read(in, num_elements))
    error("missing or invalid number of set elements in `%s'", what);
  while (num_elements--) {
    unsigned i;
    if (!guarded_read(in, i))
      error("missing or invalid set element in `%s'", what);
    dest.insert(i);
  }
}

enum Representation { H, V };

Representation
read_polyhedron(std::istream& in, PPL::C_Polyhedron& ph) {
  // By default we have an H-representation.
  Representation rep = H;

  std::string s;
  std::set<unsigned> linearity;
  while (true) {
    if (!guarded_read(in, s))
      error("premature end of file while seeking for `begin'");

    if (s == "V-representation")
      rep = V;
    else if (s == "H-representation")
      rep = H;
    else if (s == "linearity" || s == "equality" || s == "partial_enum") {
      read_indexes_set(in, linearity, "linearity");
      if (verbose) {
	std::cerr << "Linearity: ";
	for (std::set<unsigned>::const_iterator j = linearity.begin(),
	       linearity_end = linearity.end(); j != linearity_end; ++j)
	  std::cerr << *j << " ";
	std::cerr << std::endl;
      }
    }
    else if (s == "begin")
      break;
    else
      // A comment: skip to end of line.
      in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  unsigned num_rows;
  if (!guarded_read(in, num_rows))
    error("illegal or missing number of rows");

  unsigned num_columns;
  if (!guarded_read(in, num_columns))
    error("illegal or missing number of columns");

  if (!guarded_read(in, s))
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

  std::set<unsigned>::iterator linearity_end = linearity.end();
  if (rep == V) {
    std::vector<mpz_class> coefficients(num_columns-1);
    mpz_class denominator;
    bool has_a_point = false;
    for (unsigned i = 0; i < num_rows; ++i) {
      int vertex_marker;
      if (!guarded_read(in, vertex_marker)
	  || vertex_marker < 0 || vertex_marker > 1)
	error("illegal or missing vertex marker");
      read_coefficients(in, number_type, coefficients, denominator);
      PPL::LinExpression e;
      for (unsigned j = num_columns-1; j-- > 0; )
	e += coefficients[j] * PPL::Variable(j);
      if (vertex_marker == 1) {
	assert(linearity.find(i+1) == linearity_end);
	gs.insert(point(e, denominator));
	has_a_point = true;
      }
      else if (linearity.find(i+1) != linearity_end)
	gs.insert(line(e));
      else
	gs.insert(ray(e));
    }
    // Every non-empty generator system must have at least one point.
    if (num_rows > 0 && !has_a_point)
      gs.insert(PPL::point());

    if (verbose) {
      using namespace PPL::IO_Operators;
      std::cerr << "Generator system:\n" << gs << std::endl;
    }
  }
  else {
    assert(rep == H);
    std::vector<mpz_class> coefficients(num_columns);
    mpz_class denominator;
    for (unsigned i = 0; i < num_rows; ++i) {
      read_coefficients(in, number_type, coefficients, denominator);
      PPL::LinExpression e;
      for (unsigned j = num_columns; j-- > 1; )
	e += coefficients[j] * PPL::Variable(j-1);
      e += coefficients[0];
      if (linearity.find(i+1) != linearity_end)
	cs.insert(e == 0);
      else
	cs.insert(e >= 0);
    }
    if (verbose) {
      using namespace PPL::IO_Operators;
      std::cerr << "Constraint system:\n" << cs << std::endl;
    }
  }

  if (!guarded_read(in, s))
    error("premature end of file while seeking for `end'");

  if (s != "end")
    error("`%s' found while seeking for `end'", s.c_str());


  ph = rep == V ? PPL::C_Polyhedron(gs) : PPL::C_Polyhedron(cs);

  return rep;
}

void
write_polyhedron(std::ostream& out,
		 const PPL::C_Polyhedron& ph,
		 const Representation rep) {
  if (rep == H)
    guarded_write(out, "H-representation\n");
  else
    guarded_write(out, "V-representation\n");

  unsigned num_rows = 0;
  std::set<unsigned> linearity;
  if (rep == H) {
    const PPL::ConSys& cs = ph.constraints();
    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      ++num_rows;
      if (i->is_equality())
	linearity.insert(linearity.end(), num_rows);
    }
  }
  else {
    assert(rep == V);
    const PPL::GenSys& gs = ph.generators();
    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i) {
      ++num_rows;
      if (i->is_line())
	linearity.insert(linearity.end(), num_rows);
    }
  }

  if (!linearity.empty()) {
    guarded_write(out, "linearity ");
    guarded_write(out, linearity.size());
    for (std::set<unsigned>::const_iterator j = linearity.begin(),
	   linearity_end = linearity.end(); j != linearity_end; ++j) {
      guarded_write(out, ' ');
      guarded_write(out, *j);
    }
    guarded_write(out, '\n');
  }

  PPL::dimension_type space_dim = ph.space_dimension();

  guarded_write(out, "begin\n");
  guarded_write(out, num_rows);
  guarded_write(out, ' ');
  guarded_write(out, space_dim+1);
  guarded_write(out, ' ');

  if (rep == H) {
    guarded_write(out, "integer\n");
    const PPL::ConSys& cs = ph.constraints();
    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      const PPL::Constraint& c = *i;
      guarded_write(out, c.inhomogeneous_term());
      for (PPL::dimension_type j = 0; j < space_dim; ++j) {
	guarded_write(out, ' ');
	guarded_write(out, c.coefficient(PPL::Variable(j)));
      }
      guarded_write(out, '\n');
    }
  }
  else {
    assert(rep == V);
    guarded_write(out, "rational\n");
    const PPL::GenSys& gs = ph.generators();
    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i) {
      const PPL::Generator& g = *i;
      if (g.is_point()) {
	guarded_write(out, '1');
	const PPL::Integer& divisor = g.divisor();
	for (PPL::dimension_type j = 0; j < space_dim; ++j) {
	  guarded_write(out, ' ');
	  if (g.coefficient(PPL::Variable(j)) == 0)
	    guarded_write(out, '0');
	  else
	    guarded_write(out, mpq_class(g.coefficient(PPL::Variable(j)),
					 divisor));
	}
      }
      else {
	// `g' is a ray or a line.
	guarded_write(out, '0');
	for (PPL::dimension_type j = 0; j < space_dim; ++j) {
	  guarded_write(out, ' ');
	  guarded_write(out, g.coefficient(PPL::Variable(j)));
	}
      }
      guarded_write(out, '\n');
    }
  }
  guarded_write(out, "end\n");

  // Flush `out'.
  bool flush_succeeded = false;
  try {
    flush_succeeded = out.flush();
  }
  catch (...) {
  }
  if (!flush_succeeded)
    fatal("cannot write to output file `%s'", output_file_name);
}

} // namespace

#define PPL_VERSION "0.6pre5"

int
main(int argc, char* argv[]) {
  program_name = argv[0];

  if (strcmp(PPL_VERSION, PPL::version()) != 0)
    fatal("was compiled with PPL version %s, but linked with version %s",
	  PPL_VERSION, PPL::version());

  if (verbose)
    std::cerr << "Parma Polyhedra Library version:\n" << PPL::version()
	      << "\n\nParma Polyhedra Library banner:\n" << PPL::banner()
	      << std::endl;

  // Process command line options.
  process_options(argc, argv);

  if (max_seconds_of_cpu_time > 0)
    set_alarm_on_cpu_time(max_seconds_of_cpu_time, timeout);

  if (max_bytes_of_virtual_memory > 0)
    limit_virtual_memory(max_bytes_of_virtual_memory);

  // Set up the input and output streams.
  set_input(input_file_name);
  set_output(output_file_name);

  PPL::C_Polyhedron ph;
  Representation rep = read_polyhedron(input(), ph);
  //write_polyhedron(std::cout, ph, rep);

  enum Command { None, H_to_V, V_to_H, Project };
  Command command = None;

  // Read commands, if any.
  std::string s;
  while (guarded_read(input(), s)) {
    if (s == "linearity" || s == "equality" || s == "partial_enum")
      error("the `linearity' command must occur before `begin'");
    else if (s == "project") {
      command = Project;
      std::set<unsigned> indexes;
      read_indexes_set(input(), indexes, "project");
      if (verbose) {
	std::cerr << "Project: ";
	for (std::set<unsigned>::const_iterator j = indexes.begin(),
	       indexes_end = indexes.end(); j != indexes_end; ++j)
	  std::cerr << *j << " ";
	std::cerr << std::endl;
      }
      PPL::Variables_Set vs;
      for (std::set<unsigned>::const_iterator j = indexes.begin(),
	     indexes_end = indexes.end(); j != indexes_end; ++j)
	vs.insert(PPL::Variable(*j - 1));
      ph.remove_dimensions(vs);
      write_polyhedron(output(), ph, H);
      goto commands_done;
    }
    else
      warning("ignoring command `%s'", s.c_str());
    input().ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  // If we are still here, we just make a conversion.
  if (rep == V) {
    command = V_to_H;
    ph.minimized_constraints();
    write_polyhedron(output(), ph, H);
  }
  else {
    command = H_to_V;
    ph.minimized_generators();
    write_polyhedron(output(), ph, V);
  }

 commands_done:
  // Check the result, if requested to do so.
  if (check_file_name) {
    set_input(check_file_name);
    // Read the polyhedron containing the expected result.
    PPL::C_Polyhedron e_ph;
    Representation e_rep = read_polyhedron(input(), e_ph);

    switch (command) {
    case Project:
      if (ph != e_ph) {
	if (verbose)
	  std::cerr << "Check failed"
		    << std::endl;
	return 1;
      }
      break;
    case H_to_V:
    case V_to_H:
    case None:
      break;
    }
  }

  return 0;
}
