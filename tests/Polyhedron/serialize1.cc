/* Test Serializer::serialize() and Serializer::deserialize().
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_test.hh"
using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace IO_Operators;

Constraint_System make_Constraint_System(int level) {
  Coefficient z;
  Constraint_System cs;
  for(int i = 0; i < level; ++i) {
    mpz_random(z.get_mpz_t(), level);
    cs.insert(Variable(i) <= z);
    mpz_random(z.get_mpz_t(), -level);
    cs.insert(Variable(i) >= z);
  }
  return cs;
}

bool
test01() {
  C_Polyhedron ph(EMPTY), ph1;
  Serializer s;
  s.open("serialize_deserialize_empty_C_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_empty_C_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_empty_C_Polyhedron.dat.gz",out9);
  s << ph;
  s >> ph1;
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_empty_C_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_empty_C_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

bool
test02(){
  C_Polyhedron ph(UNIVERSE), ph1, ph2, ph3;
  Serializer s;
  s.open("serialize_deserialize_universe_C_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_C_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_universe_C_Polyhedron.dat.gz",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_C_Polyhedron.dat.gz", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_universe_C_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_C_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

bool
test03() {
  C_Polyhedron ph(make_Constraint_System(3)), ph1, ph2, ph3;
  ph.generators();
  Serializer s;
  s.open("serialize_deserialize_C_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_C_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_C_Polyhedron.dat.gz",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_C_Polyhedron.dat.gz", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_C_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_C_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

bool test04() {
  NNC_Polyhedron ph(EMPTY), ph1, ph2, ph3;
  Serializer s;
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat.gz",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat.gz", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_empty_NNC_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

bool test05() {
  NNC_Polyhedron ph(UNIVERSE), ph1, ph2, ph3;
  Serializer s;
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat.gz",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat.gz", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_universe_NNC_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

bool test06() {
  NNC_Polyhedron ph(make_Constraint_System(3)), ph1, ph2, ph3;
  ph.generators();
  Serializer s;
  s.open("serialize_deserialize_NNC_Polyhedron.dat",out);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_NNC_Polyhedron.dat", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_NNC_Polyhedron.dat.gz",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_NNC_Polyhedron.dat.gz", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  s.open("serialize_deserialize_NNC_Polyhedron.dat.bz2",out9);
  s.serialize(ph);
  s.close();
  s.open("serialize_deserialize_NNC_Polyhedron.dat.bz2", in);
  s.deserialize(ph1);
  s.close();
  if(!(ph == ph1))
    return false;
  return true;
}

BEGIN_MAIN
DO_TEST(test01);
DO_TEST(test02);
DO_TEST(test03);
DO_TEST(test04);
DO_TEST(test05);
DO_TEST(test06);
END_MAIN
