/* Widening_Function class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Widening_Function_defs_hh
#define PPL_Widening_Function_defs_hh 1

#include "Widening_Function.types.hh"
#include "ConSys.types.hh"

template <typename PH>
class Parma_Polyhedra_Library::Widening_Function {
public:
  typedef void (PH::* Widening_Method)(const PH&, unsigned*);

  Widening_Function(Widening_Method wm);

  void operator()(PH& x, const PH& y, unsigned* tp = 0) const;

private:
  Widening_Method w_method;
};

template <typename PH>
class Parma_Polyhedra_Library::Limited_Widening_Function {
public:
  typedef void (PH::* Limited_Widening_Method)(const PH&,
					       const ConSys&,
					       unsigned*);

  Limited_Widening_Function(Limited_Widening_Method lwm, const ConSys& cs);

  void operator()(PH& x, const PH& y, unsigned* tp = 0) const;

private:
  Limited_Widening_Method lw_method;
  const ConSys& limiting_cs;
};

#include "Widening_Function.inlines.hh"

#endif // !defined(PPL_Widening_Function_defs_hh)
