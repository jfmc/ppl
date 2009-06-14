/* PIP_Tree related class implementation: non-inline functions.
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

#include <ppl-config.h>
#include <PIP_Tree.defs.hh>

namespace Parma_Polyhedra_Library {

PIP_Decision_Node::~PIP_Decision_Node() {
  delete if_false;
  delete if_true;
}

const PIP_Solution_Node*
PIP_Tree_Node::as_solution() const {
  return 0;
}
PIP_Solution_Node*
PIP_Tree_Node::as_solution() {
  return 0;
}
const PIP_Decision_Node*
PIP_Tree_Node::as_decision() const {
  return 0;
}
PIP_Decision_Node*
PIP_Tree_Node::as_decision() {
  return 0;
}
const PIP_Solution_Node*
PIP_Solution_Node::as_solution() const {
  return this;
}
PIP_Solution_Node*
PIP_Solution_Node::as_solution() {
  return this;
}
const PIP_Decision_Node*
PIP_Decision_Node::as_decision() const {
  return this;
}
PIP_Decision_Node*
PIP_Decision_Node::as_decision() {
  return this;
}

} // namespace Parma_Polyhedra_Library

