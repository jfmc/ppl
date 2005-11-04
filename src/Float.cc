/* IEC 559 floating point format related functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>
#include "Float.defs.hh"

namespace Parma_Polyhedra_Library {

const float32_t TFloat<float32_t>::POS_MAX;
const float32_t TFloat<float32_t>::NEG_MAX;

const float64_t TFloat<float64_t>::POS_MAX;
const float64_t TFloat<float64_t>::NEG_MAX;

#ifdef FLOAT96_TYPE

const uint64_t TFloat<float96_t>::LSP_INF;
const uint64_t TFloat<float96_t>::LSP_ZERO;
const uint64_t TFloat<float96_t>::LSP_DMAX;
const uint64_t TFloat<float96_t>::LSP_NMAX;
const float96_t TFloat<float96_t>::POS_MAX;
const float96_t TFloat<float96_t>::NEG_MAX;

#endif

#ifdef FLOAT128_TYPE

const uint64_t TFloat<float128_t>::MSP_SGN_MASK;
const uint64_t TFloat<float128_t>::MSP_POS_INF;
const uint64_t TFloat<float128_t>::MSP_NEG_INF;
const uint64_t TFloat<float128_t>::MSP_POS_ZERO;
const uint64_t TFloat<float128_t>::MSP_NEG_ZERO;
const uint64_t TFloat<float128_t>::LSP_INF;
const uint64_t TFloat<float128_t>::LSP_ZERO;
const uint64_t TFloat<float128_t>::LSP_MAX;
const float128_t TFloat<float128_t>::POS_MAX;
const float128_t TFloat<float128_t>::NEG_MAX;

#endif

} // Parma_Polyhedra_Library
