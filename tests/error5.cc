#include "ppl_install.hh"
#include <stdexcept>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  Variable x(0);
  Variable y(1);
  Variable z(2);
  GenSys gs;
  gs.insert(vertex(0*x + 1*y +2*z));
  Polyhedron ph(gs);

  set<Variable> to_be_removed;
  to_be_removed.insert(z);

  ph.remove_dimensions(to_be_removed);

  try {
    to_be_removed.insert(x);
    // Here the set `to_be_removed' still contains variable `z'.
    // This variable is now beyond the space dimension,
    // so that a dimension-incompatibility exception is obtained.
    ph.remove_dimensions(to_be_removed);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl;
#endif
    exit(0);
  }
  catch (...) {
    exit(1);
  }

  // Should not get here.
  return 1;
}

