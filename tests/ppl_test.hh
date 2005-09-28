/* Header file for test programs.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include "Random_Number_Generator.defs.hh"
#include <stdexcept>
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

// Turn token S into a string: stringify(x + y) => "x + y".
#define stringify(s) #s

#ifdef DERIVED_TEST
#define C_Polyhedron NNC_Polyhedron
#endif

namespace Parma_Polyhedra_Library {

//typedef BD_Shape<mpq_class> TBD_Shape;
typedef BD_Shape<int> TBD_Shape;

//! Look for variation in \p a.
/*!
  Return <CODE>true</CODE> if \p a contains variation from
  consistency, else return <CODE>false</CODE>.  Variation can be found
  via the OK method, or via a comparison between \p a and an object
  created from the ASCII dump of \p a.

  It is assumed that \p a is up to date.

  If the loading of the ASCII dump fails then an error message is
  printed and `exit' is called.

  \p T must provide:
    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);
  and there must be a:
    bool operator==(const T& x, const T& y);
*/
template <typename T>
static bool
find_variation_template(T& a) {
  using namespace Parma_Polyhedra_Library::IO_Operators;

  if (!a.OK()) {
    nout << "OK() failed\nASCII dump:" << endl;
    a.ascii_dump(nout);
    return true;
  }

  /* FIX In some PPL classes (e.g. Congruence) the simple constructors
     are private. */
  //T b;
  T b(a);
  stringstream dump;
  a.ascii_dump(dump);
  if (!b.ascii_load(dump)) {
    std::cerr << "Failed to load `b' from the ASCII dump of `a'." << endl;
    std::cerr << "ASCII dump of `a':" << endl;
    std::cerr << dump.str();
    exit(1);
  }

  if (a == b)
    return false;

  nout << "`b' loaded from ASCII dump of `a' should equal `a'" << endl
       << "ASCII dump of `a':" << endl
       << "------------------" << endl;
  a.ascii_dump(nout);
  nout << "ASCII dump of `b' (after comparison):" << endl
       << "-------------------------------------" << endl;
  b.ascii_dump(nout);
  nout << "ASCII dump of `a' after comparison:" << endl
       << "-----------------------------------" << endl;
  a.ascii_dump(nout);

  abort();

  return true;
}

}
