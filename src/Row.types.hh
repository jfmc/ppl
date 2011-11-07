/* Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Row_types_hh
#define PPL_Row_types_hh 1

// This is needed for PPL_USE_SPARSE_MATRIX.
#include "globals.defs.hh"

#if PPL_USE_SPARSE_MATRIX
#include "Sparse_Row.types.hh"
#else
#include "Dense_Row.types.hh"
#endif

namespace Parma_Polyhedra_Library {

#if PPL_USE_SPARSE_MATRIX
typedef Sparse_Row Row;
#else
typedef Dense_Row Row;
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Row_types_hh)
