#include "ppl_install.hh"
#include <stdexcept>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  Variable x(0);
  Variable y(1);

  GenSys gs;
  gs.insert(vertex(x + y));
  gs.insert(ray(x + 0*y));
  gs.insert(ray(0*x + y));
  Polyhedron ph(gs);
  LinExpression coeff = x + y + 1;
  try {
    // This is an invalid denominator.
    Integer d = 0;
    ph.assign_variable(x, coeff, d);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_denominator: " << e.what() << endl;
#endif
    exit(0);
  }
  catch (...) {
    exit(1);
  }

  // Should not get here.
  return 1;
}
