#ifndef PPL_make_threadable_hh
#define PPL_make_threadable_hh 1

#ifdef PPL_THREAD_SAFE

#include "Init_defs.hh"
#include <utility>
#include <type_traits>

namespace Parma_Polyhedra_Library {

namespace Implementation {

template <typename Function>
class Threadable {
public:
  explicit Threadable(Function f)
    : fun(f) {}

  template <typename... Args>
  typename std::result_of<Function(Args...)>::type
  operator()(Args&&... args) const {
    Thread_Init thread_init;
    return fun(std::forward<Args>(args)...);
  }

private:
  Function fun;
}; // class Threadable

} // namespace Implementation


//! Wraps function \p f to make it PPL-threadable.
/*!
  The resulting PPL-threadable function automatically performs
  PPL-related thread initialization and finalization steps.
  \example
  Suppose that a new thread (using the PPL) has to be created that
  executes function \c foo on arguments \c arg1 and \c arg2. Then
  the following code can be used:
  <code>std::thread t(make_threadable(foo), arg1, arg2);</code>
*/
template <typename Function>
inline Implementation::Threadable<Function>
make_threadable(Function f) {
  return Implementation::Threadable<Function>(f);
};

} // namespace Parma_Polyhedra_Library

#endif // defined(PPL_THREAD_SAFE)

#endif // !defined(PPL_make_threadable_hh)









