#include "ppl.hh"
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
  try {
    // This is an invalid vertex.
    gs.insert(vertex(x + y + z, 0));
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

