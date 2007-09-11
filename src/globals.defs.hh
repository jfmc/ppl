/* Declarations of global objects.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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
#include "Slow_Copy.hh"
#include "Temp.defs.hh"
#include <exception>
#include <gmpxx.h>

namespace Parma_Polyhedra_Library {

//! Returns a value that does not designate a valid dimension.
dimension_type
not_a_dimension();

template <typename T>
inline typename Enable_If<Slow_Copy<T>::value, void>::type
swap(T&, T&) {
  COMPILE_TIME_CHECK(!Slow_Copy<T>::value, "missing swap specialization");
  // This is intentionally written to generate ambiguous overloading
  // or compile time check error.
  // A swap specialization for this type is missing and needed.
}

// FIXME: write a comment for this.
#define TEMP_INTEGER(id) DIRTY_TEMP0(Coefficient, id)

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
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
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
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)

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
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T, typename Policy>
void
numer_denom(const Checked_Number<T, Policy>& from,
	    Coefficient& num, Coefficient& den);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Divides \p x by \p y into \p to, rounding the result towards plus infinity.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T, typename Policy>
void
div_round_up(Checked_Number<T, Policy>& to,
	     Coefficient_traits::const_reference x,
	     Coefficient_traits::const_reference y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the minimum between \p x and \p y.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename N>
void
min_assign(N& x, const N& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Assigns to \p x the maximum between \p x and \p y.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename N>
void
max_assign(N& x, const N& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is an even number.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T, typename Policy>
bool
is_even(const Checked_Number<T, Policy>& x);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \f$x = -y\f$.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T, typename Policy>
bool
is_additive_inverse(const Checked_Number<T, Policy>& x,
		    const Checked_Number<T, Policy>& y);


template <typename T, typename Enable = void>
struct Has_OK : public False { };

template <typename T>
struct Has_OK<T, typename Enable_If_Is<bool (T::*)() const, &T::OK>::type>
  : public True {
};

template <typename T>
inline typename Enable_If<Has_OK<T>::value, bool>::type
f_OK(const T& to) {
  return to.OK();
}

#define FOK(T) inline bool f_OK(const T&) { return true; }

FOK(char)
FOK(signed char)
FOK(unsigned char)
FOK(signed short)
FOK(unsigned short)
FOK(signed int)
FOK(unsigned int)
FOK(signed long)
FOK(unsigned long)
FOK(signed long long)
FOK(unsigned long long)
FOK(float)
FOK(double)
FOK(long double)
FOK(mpz_class)
FOK(mpq_class)

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x is in canonical form.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
bool
is_canonical(const mpq_class& x);

} // namespace Parma_Polyhedra_Library

#include "globals.inlines.hh"

#endif // !defined(PPL_globals_defs_hh)
