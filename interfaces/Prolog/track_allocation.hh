#ifndef _track_allocation_hh
#define _track_allocation_hh

#if TRACK_ALLOCATON

#include <set>
#include <iostream>

namespace Parma_Polyhedra_Library {

class PolyTracker {
public:
  void insert(const void* pp);
  void check(const void* pp);
  void remove(const void* pp);

  PolyTracker();
  ~PolyTracker();

private:
  typedef std::set<const void*, std::less<const void*> > Set;
  Set s;
};

PolyTracker::PolyTracker() {
}

PolyTracker::~PolyTracker() {
  size_t n = s.size();
  if (n > 0) 
    std::cerr << n << " polyhedra leaked!" << std::endl;
}

void
PolyTracker::insert(const void* pp) {
  std::pair<Set::iterator, bool> stat = s.insert(pp);
  if (!stat.second)
    abort();
}

void
PolyTracker::check(const void* pp) {
  if (s.find(pp) == s.end())
    abort();
}

void
PolyTracker::remove(const void* pp) {
  if (s.erase(pp) != 1)
    abort();
}

} // namespace Parma_Polyhedra_Library

static PolyTracker poly_tracker;

#define REGISTER(x) poly_tracker.insert(x)
#define UNREGISTER(x) poly_tracker.remove(x)
#define CHECK(x) poly_tracker.check(x)

#else

#define REGISTER(x)
#define UNREGISTER(x)
#define CHECK(x)

#endif

#endif
