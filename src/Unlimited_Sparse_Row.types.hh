/* Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Unlimited_Sparse_Row_types_hh
#define PPL_Unlimited_Sparse_Row_types_hh 1

#include "Unlimited_Sparse_Row_Over_Linear_Sequence.types.hh"
#include "Unlimited_Sparse_Row_Over_CO_Tree.types.hh"

namespace Parma_Polyhedra_Library {

// PPL_SPARSE_BACKEND_SLOW_INSERTIONS: the backend is slow on insertions, even
//                                     when using an iterator as hint.
// PPL_SPARSE_BACKEND_SLOW_RANDOM_READS: the backend is slow on reads, when
//                                       not using an iterator as hint.
// PPL_SPARSE_BACKEND_SLOW_RANDOM_WRITES: the backend is slow on insertions,
//                                        when not using an iterator as hint.


#ifndef USE_PPL_SPARSE_BACKEND_STD_LIST
#ifndef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
#ifndef USE_PPL_SPARSE_BACKEND_STD_VECTOR
#ifndef USE_PPL_SPARSE_BACKEND_CO_TREE

// No sparse backend defined, assuming Std_List backend
#define USE_PPL_SPARSE_BACKEND_STD_LIST

#endif // !defined(USE_PPL_SPARSE_BACKEND_CO_TREE)
#endif // !defined(USE_PPL_SPARSE_BACKEND_STD_VECTOR)
#endif // !defined(USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST)
#endif // !defined(USE_PPL_SPARSE_BACKEND_STD_LIST)


#ifdef USE_PPL_SPARSE_BACKEND_STD_LIST

#define PPL_SPARSE_BACKEND_SLOW_RANDOM_READS
#define PPL_SPARSE_BACKEND_SLOW_RANDOM_WRITES

// If other options are specified, ignore them.
#undef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST
#undef USE_PPL_SPARSE_BACKEND_STD_VECTOR
#undef USE_PPL_SPARSE_BACKEND_CO_TREE
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CUSTOM_SLIST

#define PPL_SPARSE_BACKEND_SLOW_RANDOM_READS
#define PPL_SPARSE_BACKEND_SLOW_RANDOM_WRITES

// If other options are specified, ignore them.
#undef USE_PPL_SPARSE_BACKEND_STD_VECTOR
#undef USE_PPL_SPARSE_BACKEND_CO_TREE
#endif

#ifdef USE_PPL_SPARSE_BACKEND_STD_VECTOR
#define PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES

#define PPL_SPARSE_BACKEND_SLOW_RANDOM_WRITES
#define PPL_SPARSE_BACKEND_SLOW_INSERTIONS

// If other options are specified, ignore them.
#undef USE_PPL_SPARSE_BACKEND_CO_TREE
#endif

#ifdef USE_PPL_SPARSE_BACKEND_CO_TREE

typedef Unlimited_Sparse_Row_Over_CO_Tree Unlimited_Sparse_Row;

#else

typedef Unlimited_Sparse_Row_Over_Linear_Sequence Unlimited_Sparse_Row;

#endif // defined(USE_PPL_SPARSE_BACKEND_CO_TREE)


} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Unlimited_Sparse_Row_types_hh)
