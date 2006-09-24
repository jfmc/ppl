/* Declaration of simple print functions used in test programs.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_print_hh
#define PPL_print_hh 1

#include "ppl.hh"
#include "Partial_Function.types.hh"
#include <string>
#include <iostream>

#ifndef NOISY
#define NOISY 0
#endif

#ifndef VERY_NOISY
#define VERY_NOISY 0
#endif

static bool
check_noisy(const char* environment_variable) {
#if HAVE_DECL_GETENV
  return getenv(environment_variable) != 0;
#else
#if NOISY
  if (strcmp(environment_variable, "PPL_NOISY_TESTS") == 0)
    return true;
#endif
#if VERY_NOISY
  if (strcmp(environment_variable, "PPL_VERY_NOISY_TESTS") == 0)
    return true;
#endif
  return false;
#endif
}

template<typename CharT, typename Traits = std::char_traits<CharT> >
class nullbuf : public std::basic_streambuf<CharT, Traits> {
protected:
  virtual typename Traits::int_type overflow(typename Traits::int_type c) {
    return Traits::not_eof(c);
  }
};

template <class CharT, class Traits = std::char_traits<CharT> >
class noisy_ostream : public std::basic_ostream<CharT, Traits> {
private:
  nullbuf<CharT, Traits> black_hole;

public:
  noisy_ostream(const std::basic_ostream<CharT, Traits>& os,
		const char* environment_variable)
    : std::basic_ostream<CharT, Traits>(check_noisy(environment_variable)
					? os.rdbuf()
					: &black_hole) {
  }
};

static noisy_ostream<char> nout(std::cout, "PPL_NOISY_TESTS");
static noisy_ostream<char> vnout(std::cout, "PPL_VERY_NOISY_TESTS");

void
print_constraint(const Parma_Polyhedra_Library::Constraint& c,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_constraints(const Parma_Polyhedra_Library::Constraint_System& cs,
		  const std::string& intro = "",
		  std::ostream& s = nout);

void
print_constraints(const Parma_Polyhedra_Library::Polyhedron& ph,
		  const std::string& intro = "",
		  std::ostream& s = nout);

template <typename T>
void
print_constraints(const Parma_Polyhedra_Library::BD_Shape<T>& bd,
		  const std::string& intro = "",
		  std::ostream& s = nout) {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!intro.empty())
    s << intro << std::endl;
  s << bd << std::endl;
}

template <typename T>
void
print_constraints(const Parma_Polyhedra_Library::Octagonal_Shape<T>& oc,
		  const std::string& intro = "",
		  std::ostream& s = nout) {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!intro.empty())
    s << intro << std::endl;
  s << oc << std::endl;
}

template <typename PH>
void
print_constraints(const Parma_Polyhedra_Library::Pointset_Powerset<PH>& pps,
		  const std::string& intro = "",
		  std::ostream& s = nout) {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!intro.empty())
    s << intro << std::endl;
  s << pps << std::endl;
}

template <typename PH>
void
print_congruences(const Parma_Polyhedra_Library::Pointset_Powerset<PH>& pps,
		  const std::string& intro = "",
		  std::ostream& s = nout) {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!intro.empty())
    s << intro << std::endl;
  s << pps << std::endl;
}

template <typename PH>
void
print_constraints(const Parma_Polyhedra_Library::Pointset_Ask_Tell<PH>& pat,
		  const std::string& intro = "",
		  std::ostream& s = nout) {
  using namespace Parma_Polyhedra_Library::IO_Operators;
  if (!intro.empty())
    s << intro << std::endl;
  s << pat << std::endl;
}

void
print_congruence(const Parma_Polyhedra_Library::Congruence& c,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_congruences(const Parma_Polyhedra_Library::Congruence_System& cgs,
		  const std::string& intro = "",
		  std::ostream& s = nout);

void
print_congruences(const Parma_Polyhedra_Library::Grid& gr,
		  const std::string& intro = "",
		  std::ostream& s = nout);

void
print_generator(const Parma_Polyhedra_Library::Generator& g,
		const std::string& intro = "",
		std::ostream& s = nout);

void
print_generator(const Parma_Polyhedra_Library::Grid_Generator& g,
		const std::string& intro = "",
		std::ostream& s = nout);

void
print_generators(const Parma_Polyhedra_Library::Generator_System& gs,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_generators(const Parma_Polyhedra_Library::Grid_Generator_System& gs,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_generators(const Parma_Polyhedra_Library::Polyhedron& ph,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_generators(const Parma_Polyhedra_Library::Grid& gr,
		 const std::string& intro = "",
		 std::ostream& s = nout);

void
print_function(const Parma_Polyhedra_Library::Partial_Function& function,
	       const std::string& intro = "",
	       std::ostream& s = nout);

#endif // !defined(PPL_print_hh)
