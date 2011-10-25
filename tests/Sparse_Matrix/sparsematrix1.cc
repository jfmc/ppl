/* Test the Sparse_Matrix class.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#include "ppl_test.hh"

#if USE_PPL_SPARSE_MATRIX

#include <vector>
#include <algorithm>
#include <set>

namespace {

bool
test01() {

  Sparse_Matrix matrix(3, 4);

  if (matrix.num_rows() != 3)
    return false;

  if (matrix.num_columns() != 4)
    return false;

  matrix.resize(5, Row_Flags());

  if (matrix.num_rows() != 5)
    return false;

  if (matrix.num_columns() != 5)
    return false;

  matrix.add_zero_rows_and_columns(2, 3, Row_Flags());

  if (matrix.num_rows() != 7)
    return false;

  if (matrix.num_columns() != 8)
    return false;

  return true;
}

bool
test02() {

  Sparse_Matrix matrix(3, 6);

  // ( 0  1  2  3  4  0 )
  // ( 0  6  7  8  9 10 )
  // ( 0 11 12 13 14 15 )

  matrix[0][0] = 0;
  matrix[0][1] = 1;
  matrix[0][2] = 2;
  matrix[0][3] = 3;
  matrix[0][4] = 4;
  matrix[0][5] = 0;
  matrix[1][0] = 0;
  matrix[1][1] = 6;
  matrix[1][2] = 7;
  matrix[1][3] = 8;
  matrix[1][4] = 9;
  matrix[1][5] = 10;
  matrix[2][0] = 0;
  matrix[2][1] = 11;
  matrix[2][2] = 12;
  matrix[2][3] = 13;
  matrix[2][4] = 14;
  matrix[2][5] = 15;

  std::vector<dimension_type> cycles;

  // Swap the second and the fourth column
  cycles.push_back(1);
  cycles.push_back(3);
  cycles.push_back(0);

  // Cycle the third, fifth and sixth column
  cycles.push_back(2);
  cycles.push_back(4);
  cycles.push_back(5);
  cycles.push_back(0);

  matrix.permute_columns(cycles);


  // Expected result:
  //
  // ( 0  3  0  1  2  4 )
  // ( 0  8 10  6  7  9 )
  // ( 0 13 15 11 12 14 )

  if (matrix[0][0] != 0) return false;
  if (matrix[0][1] != 3) return false;
  if (matrix[0][2] != 0) return false;
  if (matrix[0][3] != 1) return false;
  if (matrix[0][4] != 2) return false;
  if (matrix[0][5] != 4) return false;
  if (matrix[1][0] != 0) return false;
  if (matrix[1][1] != 8) return false;
  if (matrix[1][2] != 10) return false;
  if (matrix[1][3] != 6) return false;
  if (matrix[1][4] != 7) return false;
  if (matrix[1][5] != 9) return false;
  if (matrix[2][0] != 0) return false;
  if (matrix[2][1] != 13) return false;
  if (matrix[2][2] != 15) return false;
  if (matrix[2][3] != 11) return false;
  if (matrix[2][4] != 12) return false;
  if (matrix[2][5] != 14) return false;

  return true;
}

bool
test03() {
  Sparse_Matrix large(2,5);
  Sparse_Matrix tall(6,3);

  large[0][4] = 10;
  tall[4][0] = 20;

  std::swap(large, tall);

  if (large.num_columns() != 3)
    return false;

  if (large.num_rows() != 6)
    return false;

  if (tall.num_columns() != 5)
    return false;

  if (tall.num_rows() != 2)
    return false;

  if (tall[0][4] != 10)
    return false;

  if (large[4][0] != 20)
    return false;

  return true;
}

bool
test04() {

  Sparse_Matrix x(2, 3);

  // ( 1 2 3 )
  // ( 4 5 6 )

  x[0][0] = 1;
  x[0][1] = 2;
  x[0][2] = 3;
  x[1][0] = 4;
  x[1][1] = 5;
  x[1][2] = 6;

  Sparse_Matrix::iterator itr = x.begin();

  // First row

  if (itr == x.end())
    return false;

  if ((*itr)[0] != 1)
    return false;
  if ((*itr)[1] != 2)
    return false;
  if ((*itr)[2] != 3)
    return false;

  ++itr;

  // Second row

  if (itr == x.end())
    return false;

  if ((*itr)[0] != 4)
    return false;
  if ((*itr)[1] != 5)
    return false;
  if ((*itr)[2] != 6)
    return false;

  ++itr;

  if (itr != x.end())
    return false;

  return true;
}

bool
test05() {

  Sparse_Matrix matrix(3, 5);

  // (  1  2  3  4  5 )
  // (  6  7  8  9 10 )
  // ( 11 12 13 14 15 )

  matrix[0][0] = 1;
  matrix[0][1] = 2;
  matrix[0][2] = 3;
  matrix[0][3] = 4;
  matrix[0][4] = 5;
  matrix[1][0] = 6;
  matrix[1][1] = 7;
  matrix[1][2] = 8;
  matrix[1][3] = 9;
  matrix[1][4] = 10;
  matrix[2][0] = 11;
  matrix[2][1] = 12;
  matrix[2][2] = 13;
  matrix[2][3] = 14;
  matrix[2][4] = 15;

  matrix.add_row(matrix[1]);

  // (  1  2  3  4  5 )
  // (  6  7  8  9 10 )
  // ( 11 12 13 14 15 )
  // (  6  7  8  9 10 )

  if (matrix[0][0] != 1) return false;
  if (matrix[0][1] != 2) return false;
  if (matrix[0][2] != 3) return false;
  if (matrix[0][3] != 4) return false;
  if (matrix[0][4] != 5) return false;
  if (matrix[1][0] != 6) return false;
  if (matrix[1][1] != 7) return false;
  if (matrix[1][2] != 8) return false;
  if (matrix[1][3] != 9) return false;
  if (matrix[1][4] != 10) return false;
  if (matrix[2][0] != 11) return false;
  if (matrix[2][1] != 12) return false;
  if (matrix[2][2] != 13) return false;
  if (matrix[2][3] != 14) return false;
  if (matrix[2][4] != 15) return false;
  if (matrix[3][0] != 6) return false;
  if (matrix[3][1] != 7) return false;
  if (matrix[3][2] != 8) return false;
  if (matrix[3][3] != 9) return false;
  if (matrix[3][4] != 10) return false;

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
END_MAIN

#else // !USE_PPL_SPARSE_MATRIX

// A dummy test to avoid compiler warnings in BEGIN_MAIN.
bool test01() {
  return true;
}

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN

#endif // !USE_PPL_SPARSE_MATRIX
