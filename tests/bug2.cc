#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

int main() {
  set_handlers();

  istringstream s("space_dim 2\n"
		  "-ZE -EM  -CM -GM  +CS +GS  +SC -SG \n"
		  "con_sys (up-to-date)\n"
		  "4 x 3 (not_sorted)\n"
		  "-4 1 0   >=\n"
		  "0 0 1   >=\n"
		  "7 -1 0   >=\n"
		  "3 0 -1   >=\n"
		  "\n"
		  "gen_sys (up-to-date)\n"
		  "4 x 3 (not_sorted)\n"
		  "1 4 0   V\n"
		  "1 4 3   V\n"
		  "1 7 0   V\n"
		  "1 7 3   V\n"
		  "\n"
		  "sat_c\n"
		  "4 x 4\n"
		  "0 0 1 1 \n"
		  "0 1 1 0 \n"
		  "1 0 0 1 \n"
		  "1 1 0 0 \n"
		  "\n"
		  "sat_g\n"
		  "0 x 0\n"
		  "\n"
		  "\n"
		  "space_dim 2\n"
		  "-ZE -EM  -CM -GM  -CS +GS  -SC -SG \n"
		  "con_sys (not_up-to-date)\n"
		  "0 x 0 (not_sorted)\n"
		  "\n"
		  "gen_sys (up-to-date)\n"
		  "4 x 3 (not_sorted)\n"
		  "1 4 0   V\n"
		  "1 4 3   V\n"
		  "1 7 0   V\n"
		  "1 7 3   V\n"
		  "\n"
		  "sat_c\n"
		  "0 x 0\n"
		  "\n"
		  "sat_g\n"
		  "0 x 0\n");

  Polyhedron x;
  s >> x;
  assert(x.OK());

  Polyhedron y;
  s >> y;
  assert(y.OK());

  // Call operator <=() and throw away the result.
  (void) (x <= y);

  return 0;
}

