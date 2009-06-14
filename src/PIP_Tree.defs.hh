/* PIP_Tree class declaration.
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

#ifndef PPL_PIP_Tree_defs_hh
#define PPL_PIP_Tree_defs_hh 1

namespace Parma_Polyhedra_Library {

class PIP_Solution_Node;
class PIP_Decision_Node;

class PIP_Tree_Node {
public:
  virtual const PIP_Solution_Node* as_solution() const;
  virtual PIP_Solution_Node* as_solution();
  virtual const PIP_Decision_Node* as_decision() const;
  virtual PIP_Decision_Node* as_decision();
  virtual ~PIP_Tree_Node();
};

class PIP_Solution_Node : public PIP_Tree_Node {
public:
  const PIP_Solution_Node* as_solution() const;
  PIP_Solution_Node* as_solution();
  // get_bindings();
};

class PIP_Decision_Node : public PIP_Tree_Node {
  PIP_Tree_Node* if_false;
  PIP_Tree_Node* if_true;
public:
  ~PIP_Decision_Node();
  const PIP_Decision_Node* as_decision() const;
  PIP_Decision_Node* as_decision();
  const PIP_Tree_Node* if_node(bool v) const;
  PIP_Tree_Node* if_node(bool v);
  // Constraint_System* get_constraints();
};

typedef PIP_Tree_Node* PIP_Tree;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_PIP_Tree_defs_hh)
