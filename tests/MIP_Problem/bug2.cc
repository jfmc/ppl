#include <iostream>
#include <iomanip>
#include <fenv.h>

int main() {
  // Rounding upward.
  fesetround(FE_UPWARD);
  // fesetround(FE_TONEAREST);
  // fesetround(FE_TOWARDZERO);
  // fesetround(FE_DOWNWARD);

  double a;
  double b;

  a = 1.0;
  std::cout << "a = " << std::setprecision(40) << a << "\n";
  b = 18014398509481984.0;
  std::cout << "b = " << std::setprecision(40) << b << "\n";
  a += b*b;
  std::cout << "a += b*b = " << std::setprecision(40) << a << "\n";
  b = 61699314894975792.0;
  std::cout << "b = " << std::setprecision(40) << b << "\n";
  a += b*b;
  std::cout << "a += b*b = " << std::setprecision(40) << a << "\n";

  return 0;
}
