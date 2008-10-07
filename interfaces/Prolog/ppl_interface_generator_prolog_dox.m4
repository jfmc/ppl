m4_define(`dnl', `m4_dnl')`'dnl
m4_divert(-1)

dnl This m4 file generates the file Prolog_interface.dox

dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

m4_define(`m4_documentation_generation', `')

m4_divert`'dnl
m4_include(`ppl_prolog_sysindep_dox')
m4_divert(-1)`'dnl

dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_prolog_procedure_generators.m4')
m4_define(`m4_interface_classes_names', `Polyhedron@Pointset_Powerset_C_Polyhedron@Pointset_Powerset_NNC_Polyhedron')
m4_define(`m4_cplusplus_classes_names', `Polyhedron@Pointset_Powerset<C_Polyhedron>@Pointset_Powerset<NNC_Polyhedron>')
m4_include(`ppl_interface_generator_prolog_dox_code.m4')

m4_divert`'dnl
dnl m4_include(`ppl_prolog_sysindep_dox')
m4_divert(-1)`'dnl
m4_init_class_definitions

dnl -----------------------------------------------------------------
dnl The Polyhedron Domain.
dnl -----------------------------------------------------------------
m4_divert`'dnl
\anchor Polyhedron_predicates
<H1>Predicates for C Polyhedra</H1>
Here we provide a short description for each of the predicates
available for the domain of C polyhedra.
Note that predicates for other domains will follow a similar pattern.

<H2>Constructor, copy, conversion and destructor predicates</H2>

dnl -----------------------------------------------------------------
dnl The constructor predicates.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_build_doc', `m4_indir(`$1_build_doc')', `')
')
m4_divert`'dnl
<H3>Constructor predicates for C polyhedra</H3>
The constructor predicates build a C polyhedron from a specification
and binds the given variable to a handle for future referencing.
The specification can be:
- the number of space dimensions and an atom indicating if it
  is to be the universe or empty element.
- a representation for the particular class of semantic geometric
  descriptors to which the element being built belongs.
  For example, a C polyhedron can be built from a list of
  non-strict inequality or equality constraints or a list of equality
  congruences or a list of generators that contains no closure points.
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl The copy and conversion constructor predicates.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_convert_doc', `m4_indir(`$1_convert_doc')', `')
')
m4_divert`'dnl
<H3>Predicates that build new C polyhedra by copying or converting from
  other semantic geometric descriptions</H3>
  Besides the constructors listed above, the library also
  provides:
- copy constructors that will copy an element belonging
  to the same class of semantic geometric descriptions
- conversion operators that build a new semantic geometric
  description starting from a \b friend;
  that is, a semantic geometric description in different class
  (e.g., ppl_new_Grid_from_C_Polyhedron,
  ppl_new_C_Polyhedron_from_BD_Shape_mpq_class, etc.).<BR>

  The copy and conversion predicates have two versions,
  one with arity 2 for the source and target handles and one
  with an extra argument denoting the maximum complexity to be used
  in the conversion;
  this complexity argument is ignored when the
  the friend and the element being built are in the same class.
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl The destructor predicate
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_destruct_doc', `m4_indir(`$1_destruct_doc')', `')
')
m4_divert`'dnl
<H3>Destructor predicate</H3>
Below is the destructor predicate for the Polyhedron domain.
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that do not modify the polyhedron.
m4_divert`'dnl
<H2>Predicates that do not change the polyhedron</H2>
m4_divert(-1)
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_testpoly_doc', `m4_indir(`$1_testpoly_doc')', `')
')
m4_divert`'dnl
<H3>Test Predicates</H3>
These predicates test the polyhedron for different properties
and succeed or fail depending on the outcome.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that get information about the polyhedron.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_constpoly_doc', `m4_indir(`$1_constpoly_doc')', `')
')
m4_divert`'dnl
<H3>Predicates that return information about the polyhedron</H3>
These predicates will obtain more detailed information about the polyhedron
unifying some of their arguments with the results.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Space dimension preserving predicates that may modify the polyhedron.
m4_divert`'dnl
<H2>Space-dimension preserving predicates that may change the polyhedron</H2>
These predicates may modify the polyhedron referred to
by the handle in first argument;
the (dimension of the) vector space in which it is embedded is unchanged.
m4_divert(-1)

dnl Predicates that may change the polyhedron by adding
dnl to the polyhedron's constraint or generator descriptions.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_addto_doc', `m4_indir(`$1_addto_doc')', `')
')
m4_divert`'dnl
<H3>Predicates that may change the polyhedron by adding
to the polyhedron's constraint or generator descriptions</H3>
Note that there are two forms of these predicates
differentiated in the names by the
words "add" or "refine with";
see Section \extref{Generic_Operations_on_Semantic_Geometric_Descriptors, "Generic Operations on Semantic Geometric Descriptors"} in the
main \extref{preamble, PPL user manual}
for the differences in the semantics and therefore, the expected behavior,
between these forms.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that transform a polyhedron.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_trans_doc', `m4_indir(`$1_trans_doc')', `')
')
m4_divert`'dnl
<H3>Predicates that transform a polyhedron</H3>
These predicates enable transformations
such as taking the topological closure (which for the domain of
C polyhedron is the identity transformation),
unconstraining a specified dimension
as explained in the
main \extref{preamble, PPL user manual} in Section
\extref{Cylindrification, "Cylindrification Operator"}
and several different image and preimage affine transfer relations;
for details of the latter see Sections
\extref{Images_and_Preimages_of_Affine_Transfer_Relations, "Images and Preimages of Affine Transfer Relations"}
and
\extref{Generalized_Affine_Relations, "Generalized Affine Relations"}.
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that act as binary operators.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_binop_doc', `m4_indir(`$1_binop_doc')', `')
')
m4_divert`'dnl
<H2>Predicates whose results depend on more than one polyhedron</H2>
These predicates include the binary operators which will assign to the
polyhedron referred to by the first argument
its combination with the polyhedron referred to by the second argument
as described in the
main \extref{preamble, PPL user manual} in Sections
\extref{Intersection_and_Convex_Polyhedral_Hull, "Intersection and Convex Polyhedral Hull"}
and
\extref{Convex_Polyhedral_Difference, "Convex Polyhedral Difference"};
and a linear partitioning operator described below.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates for widening and extrapolation.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_widen_doc', `m4_indir(`$1_widen_doc')', `')
')
m4_divert`'dnl
<H3>Predicates for widening and extrapolation</H3>
In addition to the above binary operators, there are also a number
of widening, extrapolation and narrowing operators
as described in the
main \extref{preamble, PPL user manual} in Sections
\extref{Widening_Operators, "Widening Operators"},
\extref{Widening_with_Tokens, "Widening with Tokens"} and
\extref{Extrapolation_Operators, "Extrapolation Operators"}.
Note that for all these widening and extrapolation predicates
to behave as specified
the polyhedron referred to by the second argument has to be
contained in (or equal to) the polyhedron referred to by the first argument.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that may modify the dimension of the vector space.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_varspace_doc', `m4_indir(`$1_varspace_doc')', `')
')
m4_divert`'dnl
<H2>Predicates that may modify the vector space of the polyhedron</H2>
These predicates enable the modification of the vector space
of the polyhedron referred to in the first argument.
Detailed descriptions of these can be found in the
main \extref{preamble, PPL user manual} in Sections
\extref{Concatenating_Polyhedra, "Concatenating Polyhedra"},
\extref{Adding_New_Dimensions_to_the_Vector_Space, "Adding New Dimensions to the Vector Space"},
\extref{Removing_Dimensions_from_the_Vector_Space, "Removing Dimensions from the Vector Space"},
\extref{Mapping_the_Dimensions_of_the_Vector_Space, "Mapping the Dimensions of the Vector Space"},
\extref{expand_space_dimension, "Expanding One Dimension of the Vector Space to Multiple Dimensions"}
and
\extref{Folding_Multiple_Dimensions_of_the_Vector_Space_into_One_Dimension, "Folding Multiple Dimensions of the Vector Space into One Dimension"}.

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicate that dumps.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_dump_doc', `m4_indir(`$1_dump_doc')', `')
')
m4_divert`'dnl
<H2>Ascii dump predicate</H2>

m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Define the class Pointset_Powerset_C_Polyhedron to be documented.
m4_divert`'dnl
\anchor other_domains
<H1>Ad hoc Predicates for Other Domains</H1>
<H2>Extra Predicates Specifically for the Pointset_Powerset Domains</H2>
  The powerset domains can be instantiated by taking as a base domain
  any fixed semantic geometric description
  (C and NNC polyhedra, BD and octagonal shapes, boxes and grids).
  An element of the powerset domain represents a disjunctive collection
  of base objects (its disjuncts), all having the same space dimension.

  Besides the predicates that are available in all semantic geometric
  descriptions (whose documentation is not repeated here),
  the powerset domain also provides several ad hoc functions.
  In particular, the iterator types allow for the examination and
  manipulation of the collection of disjuncts.

  Below is a specification of extra predicates provided for the
  Pointset_Powerset<C_Polyhedron> domain.
  Note that predicates for other Pointset_Powerset domains
  will follow a similar pattern.

dnl Predicates for pointset powerset iterators.
  <H2>Predicates for pointset powerset iterators.</H2>
m4_divert(-1)
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_pps_iter_doc', `m4_indir(`$1_pps_iter_doc')', `')
')
m4_divert`'dnl
m4_ifdef(m4_interface_class`'2,
`m4_patsubst(m4_one_class_code(2), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl Predicates that may change a pointset powerset.
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_pps_change_doc', `m4_indir(`$1_pps_change_doc')', `')
')
m4_divert`'dnl
  <H2>Other Ad Hoc Predicates for the Pointset Powerset.</H2>

m4_ifdef(m4_interface_class`'2,
`m4_patsubst(m4_one_class_code(2), @COMMA@, `,')')`'dnl
m4_popdef(`m4_extension')
m4_include(`ppl_prolog_sysdep_dox')
m4_popdef(`m4_interface_classes_names')
m4_popdef(`m4_cplusplus_classes_names')

dnl
dnl End of file generation.
