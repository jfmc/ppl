
#ifndef _maxmin_hh
#define _maxmin_hh 1

namespace Parma_Polyhedra_Library {

  inline int
  max(int x, int y) {
    return x < y ? y : x;
  }

  inline int
  min(int x, int y) {
    return x < y ? x : y;
  }

  inline int
  max(unsigned int x, unsigned int y) {
    return x < y ? y : x;
  }

  inline int
  min(unsigned int x, unsigned int y) {
    return x < y ? x : y;
  }

  inline int
  max(char x, char y) {
    return x < y ? y : x;
  }

  inline int
  min(char x, char y) {
    return x < y ? x : y;
  }

} // namespace Parma_Polyhedra_Library
#endif
