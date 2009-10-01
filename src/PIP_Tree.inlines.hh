/* PIP_Tree related class implementation: inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_PIP_Tree_inlines_hh
#define PPL_PIP_Tree_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
PIP_Solution_Node::Rational_Matrix::Rational_Matrix()
  : Matrix(),
    denominator(1) {
}

inline
PIP_Solution_Node::Rational_Matrix::Rational_Matrix(dimension_type n_rows,
                                              dimension_type n_columns,
                                              Row::Flags row_flags)
  : Matrix(n_rows, n_columns, row_flags),
    denominator(1) {
}

inline
PIP_Solution_Node::Rational_Matrix::Rational_Matrix(const Rational_Matrix& y)
  : Matrix(y),
    denominator(y.denominator) {
}

inline bool
PIP_Solution_Node::Rational_Matrix::is_integer() const {
  return denominator == 1;
}

inline const Coefficient&
PIP_Solution_Node::Rational_Matrix::get_denominator() const {
  return denominator;
}

inline dimension_type
PIP_Solution_Node::Rational_Matrix::capacity() const {
  return row_capacity;
}

inline
PIP_Tree_Node::~PIP_Tree_Node() {
}

inline const Constraint_System&
PIP_Tree_Node::constraints() const {
  return constraints_;
}

inline
const PIP_Tree_Node*
PIP_Decision_Node::child_node(bool v) const {
  return v ? true_child : false_child;
}

inline
PIP_Tree_Node*
PIP_Decision_Node::child_node(bool v) {
  return v ? true_child : false_child;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_PIP_Tree_inlines_hh)
