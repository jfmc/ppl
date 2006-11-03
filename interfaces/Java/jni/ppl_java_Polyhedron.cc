/* PPL Java interface Polyhedron routines implementation.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_java_Polyhedron.h"
#include "ppl_java_common.hh"
using namespace Parma_Polyhedra_Library;

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1empty
(JNIEnv* env, jobject j_polyhedron) {
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c  = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_empty();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1universe
(JNIEnv* env, jobject j_polyhedron) {
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_universe();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1topologically_1closed
(JNIEnv* env, jobject j_polyhedron){
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_topologically_closed();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1bounded
(JNIEnv* env, jobject j_polyhedron){
  jlong ptr = get_ptr(env, j_polyhedron);
  Polyhedron* c = reinterpret_cast<Polyhedron*>(ptr);
  return c->is_bounded();
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_is_1disjoint_1from
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->is_disjoint_from(*polyhedron);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_bounds_1from_1above
(JNIEnv* env, jobject j_this_polyhedron, jobject java_le) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_polyhedron->bounds_from_above(le);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_bounds_1from_1below
(JNIEnv* env, jobject j_this_polyhedron, jobject java_le) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Linear_Expression le = build_linear_expression(env, java_le);
  return this_polyhedron->bounds_from_below(le);
}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_contains
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->contains(*polyhedron);

}

JNIEXPORT jboolean JNICALL Java_ppl_1java_Polyhedron_strictly_1contains
(JNIEnv* env, jobject j_this_polyhedron, jobject j_polyhedron) {
  jlong this_ptr = get_ptr(env, j_this_polyhedron);
  jlong polyhedron_ptr = get_ptr(env, j_this_polyhedron);
  Polyhedron* this_polyhedron = reinterpret_cast<Polyhedron*>(this_ptr);
  Polyhedron* polyhedron = reinterpret_cast<Polyhedron*>(polyhedron_ptr);
  return this_polyhedron->strictly_contains(*polyhedron);
}
