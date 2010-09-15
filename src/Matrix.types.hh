/* Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Matrix_types_hh
#define PPL_Matrix_types_hh 1

// This is needed for USE_PPL_SPARSE_MATRIX.
#include "globals.defs.hh"

#if USE_PPL_SPARSE_MATRIX
#include "Sparse_Matrix.types.hh"
#else
#include "Dense_Matrix.types.hh"
#endif

namespace Parma_Polyhedra_Library {

#if USE_PPL_SPARSE_MATRIX
typedef Sparse_Matrix Matrix;
#else
typedef Dense_Matrix Matrix;
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Matrix_types_hh)
