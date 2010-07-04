/* Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Proxy_types_hh
#define PPL_Proxy_types_hh 1

namespace Parma_Polyhedra_Library {

template <typename Target, typename Underlying>
struct Underlying_To_Exposed;

template <typename Target, typename Exposed>
struct Exposed_To_Underlying;

template <typename Target>
class Proxy;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Proxy_types_hh)
