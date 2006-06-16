/* Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Direct_Product_types_hh
#define PPL_Direct_Product_types_hh 1

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
class Direct_Product;

template <typename D1, typename D2>
class Open_Product;

typedef Open_Product<Grid, C_Polyhedron> Grid_C_Polyhedron;
typedef Open_Product<Grid, NNC_Polyhedron> Grid_NNC_Polyhedron;
typedef Grid_NNC_Polyhedron Grid_Polyhedron;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Direct_Product_types_hh)
