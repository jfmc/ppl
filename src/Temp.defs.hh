#ifndef PPL_Temp_defs_hh
#define PPL_Temp_defs_hh 1

#include "compiler.hh"

namespace Parma_Polyhedra_Library {

template <typename T> struct Slow_Copy : public False { };

}

#if 0
// FIXME: to uncomment as soon as possible
namespace std {
using namespace Parma_Polyhedra_Library;
template <typename T>
inline typename Enable_If<Slow_Copy<T>::value, void>::type
swap(T&, T&) {
  COMPILE_TIME_CHECK(!Slow_Copy<T>::value, "missing swap specialization");
  // This is intentionally written to generate ambiguous overloading
  // or compile time check error.
  // A swap specialization for this type is missing and needed.
}
}
#endif

namespace Parma_Polyhedra_Library {

template <typename T, T>
struct Enable_If_Is {
  typedef void type;
};

template <typename T, typename Enable = void>
struct Has_Assign_Or_Swap : public False { };

template <typename T>
struct Has_Assign_Or_Swap<T, typename Enable_If_Is<void (T::*)(T& x), &T::assign_or_swap>::type> : public True { };


template <typename T>
inline typename Enable_If<Has_Assign_Or_Swap<T>::value, void>::type
assign_or_swap(T& to, T& from) {
  to.assign_or_swap(from);
}

template <typename T>
inline typename Enable_If<!Has_Assign_Or_Swap<T>::value && !Slow_Copy<T>::value, void>::type
assign_or_swap(T& to, T& from) {
  to = from;
}

template <typename T>
inline typename Enable_If<!Has_Assign_Or_Swap<T>::value && Slow_Copy<T>::value, void>::type
assign_or_swap(T& to, T& from) {
  std::swap(to, from);
}

template <typename T>
class Temp_Item {
private:
  T item_;
  Temp_Item* next;
  static Temp_Item* list;
  Temp_Item(const Temp_Item&);
  Temp_Item& operator=(const Temp_Item&);
public:
  Temp_Item()
    : item_() {
  }
  T& item() {
    return item_;
  }
  static Temp_Item& get() {
    if (list) {
      Temp_Item* p = list;
      list = list->next;
      return *p;
    }
    else
      return *new Temp_Item();
  }
  static void release(Temp_Item& p) {
    p.next = list;
    list = &p;
  }
};

template <typename T>
Temp_Item<T>* Temp_Item<T>::list = 0;


template <typename T>
class Temp_Holder {
private:
  Temp_Item<T>& hold;
public:
  Temp_Holder(Temp_Item<T>& obj)
    : hold(obj) {
  }
  ~Temp_Holder() {
    Temp_Item<T>::release(hold);
  }
  T& item() {
    return hold.item();
  }
};

template <typename T>
struct Null_Holder {
private:
  T item_;
public:
  Null_Holder() {
  }
  T item() {
    return item_;
  }
};

template <typename T>
struct Temp_List {
  typedef T& type;
  typedef Temp_Holder<T> holder_type;
  static holder_type get_holder() {
    return Temp_Holder<T>(Temp_Item<T>::get());
  }
};

template <typename T>
struct Temp_Local {
  typedef T type;
  typedef Null_Holder<T> holder_type;
  static holder_type get_holder() {
    return Null_Holder<T>();
  }
};

template <typename T, typename Enable = void>
struct Dirty_Temp;

template <typename T>
struct Dirty_Temp<T, typename Enable_If<Slow_Copy<T>::value, void>::type> : public Temp_List<T> { };
template <typename T>
struct Dirty_Temp<T, typename Enable_If<!Slow_Copy<T>::value, void>::type> : public Temp_Local<T> { };


#define DIRTY_TEMP(T, id)						\
  typename Dirty_Temp<T>::holder_type holder ## id =			\
    Dirty_Temp<T>::get_holder();					\
  typename Dirty_Temp<T>::type id = holder ## id.item()

}


#endif // !defined(PPL_Temp_defs_hh)
