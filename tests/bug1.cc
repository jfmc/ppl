#include <config.h>

#include "ppl_install.hh"
#include <iostream>
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

int
main() {
  ifstream s("bug1.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  Polyhedron x;
  Polyhedron y;

  s >> x;
  s >> y;

  assert(x.OK());
  assert(y.OK());

  // Now see the program explode in unexpected and interesting ways.
  x.convex_hull_assign_and_minimize(y);

}
