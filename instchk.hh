
#include <stdint.h>

namespace Parma_Polyhedra_Library {

class Polyhedron {
public:
  static bool valid_instantiation() {
    return true;
  }
  static bool valid_Polyhedra_Powerset_argument() {
    return true;
  }
};

template <typename T>
bool
valid_BD_Shape_argument(void);

template <>
bool
valid_BD_Shape_argument<char>() {
  return true;
}

template <>
bool
valid_BD_Shape_argument<int>() {
  return true;
}

template <>
bool
valid_BD_Shape_argument<int8_t>() {
  return true;
}

template <typename T>
class BD_Shape {
public:
  static bool valid_instantiation() {
    return valid_BD_Shape_argument<T>();
  }
};

template <typename PH>
class Polyhedra_Powerset {
public:
  static bool valid_instantiation() {
    return PH::valid_Polyhedra_Powerset_argument();
  }
};

} // namespace Parma_Polyhedra_Library
