/* Declarations of global objects.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_globals_defs_hh
#define PPL_globals_defs_hh 1

#include "globals.types.hh"
#include "Coefficient.defs.hh"
#include "C_Integer.hh"
#include "meta_programming.hh"
#include <exception>

namespace Parma_Polyhedra_Library {

//! Returns a value that does not designate a valid dimension.
dimension_type
not_a_dimension();

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A node of the list of available coefficients.
/*! \ingroup PPL_CXX_interface */
// FIXME: rewrite the comment.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Coefficient_free_list_element {
private:
  Coefficient i;
  Coefficient_free_list_element* p;

public:
  Coefficient_free_list_element()
    : i() {
  }

  Coefficient& integer() {
    return i;
  }

  Coefficient_free_list_element*& next() {
    return p;
  }
};

extern Coefficient_free_list_element* Coefficient_free_list_first;

inline Coefficient&
get_tmp_Coefficient() {
  Coefficient* p;
  if (Coefficient_free_list_first != 0) {
    p = &Coefficient_free_list_first->integer();
    Coefficient_free_list_first = Coefficient_free_list_first->next();
  }
  else
    p = reinterpret_cast<Coefficient*>(new Coefficient_free_list_element());
  return *p;
}

inline void
release_tmp_Coefficient(Coefficient& i) {
  Coefficient_free_list_element& e
    = reinterpret_cast<Coefficient_free_list_element&>(i);
  e.next() = Coefficient_free_list_first;
  Coefficient_free_list_first = &e;
}

class Temp_Coefficient_Holder {
private:
  Coefficient& hold;

public:
  Temp_Coefficient_Holder(Coefficient& i)
    : hold(i) {
  }
  ~Temp_Coefficient_Holder() {
    release_tmp_Coefficient(hold);
  }
};

#if 1
#define TEMP_INTEGER(id) \
Coefficient& id = get_tmp_Coefficient(); \
Temp_Coefficient_Holder temp_Coefficient_holder_ ## id = (id)
#else
#define TEMP_INTEGER(id) static Coefficient id
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Speculative allocation function.
/*!
  \return
  The actual capacity to be allocated.

  \param requested_size
  The number of elements we need.

  \param maximum_size
  The maximum number of elements to be allocated. It is assumed
  to be no less than \p requested_size.

  Computes a capacity given a requested size.
  Allows for speculative allocation aimed at reducing the number of
  reallocations enough to guarantee amortized constant insertion time
  for our vector-like data structures. In all cases, the speculative
  allocation will not exceed \p maximum_size.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
dimension_type
compute_capacity(dimension_type requested_size,
		 dimension_type maximum_size);

// FIXME!!!
dimension_type
compute_capacity(dimension_type requested_size);

//! User objects the PPL can throw.
/*! \ingroup PPL_CXX_interface
  This abstract base class should be instantiated by those users
  willing to provide a polynomial upper bound to the time spent
  by any invocation of a library operator.
*/
class Throwable {
public:
  //! Throws the user defined exception object.
  virtual void throw_me() const = 0;

  //! Virtual destructor.
  virtual ~Throwable();
};

/*! \brief
  A pointer to an exception object.

  \ingroup PPL_CXX_interface
  This pointer, which is initialized to zero, is repeatedly checked
  along any super-linear (i.e., computationally expensive) computation
  path in the library.
  When it is found nonzero the exception it points to is thrown.
  In other words, making this pointer point to an exception (and
  leaving it in this state) ensures that the library will return
  control to the client application, possibly by throwing the given
  exception, within a time that is a linear function of the size
  of the representation of the biggest object (powerset of polyhedra,
  polyhedron, system of constraints or generators) on which the library
  is operating upon.

  \note
  The only sensible way to assign to this pointer is from within a
  signal handler or from a parallel thread.  For this reason, the
  library, apart from ensuring that the pointer is initially set to zero,
  never assigns to it.  In particular, it does not zero it again when
  the exception is thrown: it is the client's responsibility to do so.
*/
extern const Throwable* volatile abandon_expensive_computations;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  If the pointer abandon_expensive_computations is found
  to be nonzero, the exception it points to is thrown.

  \relates Throwable
*/
#endif
void
maybe_abandon();

//! A tag class.
/*! \ingroup PPL_CXX_interface
  Tag class to differentiate the C_Polyhedron and NNC_Polyhedron
  constructors that build a polyhedron out of a bounding box.
*/
struct From_Bounding_Box {
};

//! A tag class.
/*! \ingroup PPL_CXX_interface
  Tag class to make the Grid covering box constructor unique.
*/
struct From_Covering_Box {
};

//! A tag class.
/*! \ingroup PPL_CXX_interface
  Tag class to distinguish those constructors that recycle the data
  structures of their arguments, instead of taking a copy.
*/
struct Recycle_Input {
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  If \f$g\f$ is the GCD of \p x and \p y, the values of \p x and \p y
  divided by \f$g\f$ are assigned to \p nx and \p ny, respectively.

  \note
  \p x and \p nx may be the same object and likewise for
  \p y and \p ny.  Any other aliasing results in undefined behavior.
*/
#endif
void
normalize2(Coefficient_traits::const_reference x,
	   Coefficient_traits::const_reference y,
	   Coefficient& nx, Coefficient& ny);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns a mask for the lowest \p n bits,
#endif
template <typename T>
T low_bits_mask(unsigned n);

// Turn s into a string: PPL_STR(x + y) => "x + y".
#define PPL_STR(s) #s
// Turn the expansion of s into a string: PPL_XSTR(x) => "x expanded".
#define PPL_XSTR(s) PPL_STR(s)

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define PPL_OUTPUT_DECLARATIONS						\
  /*! \brief Writes to \c std::cerr an ASCII representation of \p *this. */ \
  void ascii_dump() const;						\
  /*! \brief Writes to \p s an ASCII representation of \p *this. */	\
  void ascii_dump(std::ostream& s) const;				\
  /*! \brief Prints \p *this to \c std::cerr using \c operator<<. */	\
  void print() const;
#else
#define PPL_OUTPUT_DECLARATIONS					\
  void ascii_dump() const;					\
  void ascii_dump(std::ostream& s) const;			\
  void print() const;
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

#define PPL_OUTPUT_DEFINITIONS(class_name)			\
  void								\
  Parma_Polyhedra_Library::class_name::ascii_dump() const {	\
    ascii_dump(std::cerr);					\
  }								\
								\
  void								\
  Parma_Polyhedra_Library::class_name::print() const {		\
    using namespace IO_Operators;				\
    std::cerr << *this;						\
  }

#define PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(class_name)			\
  void									\
  Parma_Polyhedra_Library::class_name::ascii_dump() const {		\
    ascii_dump(std::cerr);						\
  }									\
									\
  void									\
  Parma_Polyhedra_Library::class_name::print() const {			\
    std::cerr << "No user level output operator defined "		\
	      << "for class " PPL_XSTR(class_name) << "." << std::endl; \
  }

#define PPL_OUTPUT_TEMPLATE_DEFINITIONS(type_symbol, class_prefix)	\
  template <typename type_symbol>					\
  void									\
  class_prefix::ascii_dump() const {					\
    ascii_dump(std::cerr);						\
  }									\
									\
  template <typename type_symbol>					\
  void									\
  class_prefix::print() const {						\
    using namespace IO_Operators;					\
    std::cerr << *this;							\
  }

// FIXME: The class_prefix has changed from
//        PPL_OUTPUT_TEMPLATE_DEFINITIONS, to work around `,'.
//        Perhaps PPL_OUTPUT_TEMPLATE_DEFINITIONS should be changed to
//        match this.
#define PPL_OUTPUT_2_PARAM_TEMPLATE_DEFINITIONS(type_symbol1,		\
						type_symbol2,		\
						class_prefix)		\
  template <typename type_symbol1, typename type_symbol2>		\
  void									\
  class_prefix<type_symbol1, type_symbol2>::ascii_dump() const {	\
    ascii_dump(std::cerr);						\
  }									\
									\
  template <typename type_symbol1, typename type_symbol2>		\
  void									\
  class_prefix<type_symbol1, type_symbol2>::print() const {		\
    using namespace IO_Operators;					\
    std::cerr << *this;							\
  }

#define PPL_OUTPUT_TEMPLATE_DEFINITIONS_ASCII_ONLY(type_symbol, class_prefix) \
  template <typename type_symbol>					\
  void									\
  class_prefix::ascii_dump() const {					\
    ascii_dump(std::cerr);						\
  }									\
									\
  template <typename type_symbol>					\
  void									\
  class_prefix::print() const {						\
    std::cerr << "No user level output operator defined "		\
	      << "for " PPL_XSTR(class_prefix) << "." << std::endl;	\
  }

template <typename T, long long v, typename Enable = void>
struct Fit : public False {
};

template <typename T, long long v>
struct Fit<T, v, typename Enable_If<C_Integer<T>::value>::type>  {
  enum {
    value = (v >= static_cast<long long>(C_Integer<T>::min)
             && v <= static_cast<long long>(C_Integer<T>::max))
  };
};

template <typename T, long long v>
struct TConstant {
  static const T value = v;
};


template <typename T, long long v>
const T TConstant<T, v>::value;

template <typename T, long long v, bool prefer_signed = true,
	  typename Enable = void>
struct Constant_ : public TConstant<T, v> {
};

template <typename T, long long v, bool prefer_signed>
struct Constant_<T, v, prefer_signed,
		 typename Enable_If<(Fit<typename C_Integer<T>::smaller_signed_type, v>::value
				     && (prefer_signed ||
					 !Fit<typename C_Integer<T>::smaller_unsigned_type, v>::value))>::type>
  : public Constant_<typename C_Integer<T>::smaller_signed_type, v, prefer_signed> {
};

template <typename T, long long v, bool prefer_signed>
struct Constant_<T, v, prefer_signed,
		 typename Enable_If<(Fit<typename C_Integer<T>::smaller_unsigned_type, v>::value
				     && (!prefer_signed ||
					 !Fit<typename C_Integer<T>::smaller_signed_type, v>::value))>::type>
  : public Constant_<typename C_Integer<T>::smaller_unsigned_type, v, prefer_signed> {
};

template <long long v, bool prefer_signed = true>
struct Constant : public Constant_<long long, v, prefer_signed> {
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Extract the numerator and denominator components of \p from.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline void
numer_denom(const Checked_Number<T, Policy>& from,
	    Coefficient& num, Coefficient& den);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Divides \p x by \p y into \p to, rounding the result towards plus infinity.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline void
div_round_up(Checked_Number<T, Policy>& to,
	     Coefficient_traits::const_reference x,
	     Coefficient_traits::const_reference y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the minimum between \p x and \p y.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename N>
inline void
min_assign(N& x, const N& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the maximum between \p x and \p y.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename N>
inline void
max_assign(N& x, const N& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is an even number.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline bool
is_even(const Checked_Number<T, Policy>& x);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \f$x = -y\f$.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T, typename Policy>
inline bool
is_additive_inverse(const Checked_Number<T, Policy>& x,
		    const Checked_Number<T, Policy>& y);

} // namespace Parma_Polyhedra_Library

#include "globals.inlines.hh"

#endif // !defined(PPL_globals_defs_hh)
