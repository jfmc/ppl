/*
This file provides Prolog predicates that interface with the
Parma Polyhedron Library.

ppl_new_polyhedron(-Address, +Int)

   Creates a new universe polyhedron with Int dimensions
   with reference Address.
   Thus the query
      |?- ppl_new_polyhedron(X, 3).
   creates a universe polyhedron with 3 dimensions with
   reference address in X.

ppl_new_empty_polyhedron(-Address, +Int))

   Creates a new empty polyhedron with Int dimensions.
   Thus the query
      |?- ppl_new_empty_polyhedron(X, 3).
   creates an empty polyhedron with 3 dimensions with
   reference address in X.

ppl_copy_polyhedron(+Address1, -Address2))

   Copies the polyhedron referenced by Address1 to one
   referenced by Address2.
   Thus the query
      |?- ppl_new_polyhedron(X1, 3),
          ppl_copy_polyhedron(X1, A2).
   creates two universe polyhedra with 3 dimensions
   referenced by X1 and X2.

ppl_delete_polyhedron(+Address)

   Deletes the polyhedron referenced by Address.

ppl_space_dimension(+Address, -Int)

   There must be a polyhedron P referenced by Address.
   Returns in Int the number of dimensions of the vector space of P.

ppl_insert_constraint(+Address, +Constr).

   Adds the constraint Constr to the polyhedron referenced by Address.
   Constr is a term accepted by the grammar below.

   Constr -->   Expr = Expr    equation
              | Expr =< Expr   nonstrict inequation
              | Expr >= Expr   nonstrict inequation

   Expr   -->   '$VAR'(N)      variable 
                               (N an integer, 0 =< N < Dimension of P)
              | number         floating point or integer
              | + Expr         unary plus
              | - Expr         unary minus
              | Expr + Expr    addition
              | Expr - Expr    subtraction
              | Expr * Expr    multiplication
              | Expr / Expr    division

    Thus after the query
      |?- ppl_new_polyhedron(X, 3),
	  ppl_check_empty(X),
	  ppl_insert_constraint(X, 4*'$VAR'(0)+'$VAR'(1)-2*'$VAR'(2) >= 5).
    the polyhedron referenced by X is defined to be the set of points
    in the 3-dimensional vector space satisfying the constraint
    4x + y - 2z >= 5.

ppl_insert_generator(+Address, +Gen)

   Adds the generator Gen to the polyhedra P referenced by Address.
   Gen is a term accepted by the grammar below.

   Gen   -->   vertex(Expr)      vertex
             | vertex(Expr,Int)  vertex 
                                 (Int is the denominator so that the
                                 vertex is defined by Expr/Int)
             | ray(Expr)         ray
             | line(Expr)        line

   Thus after the query
     |?- ppl_new_polyhedron(X, 3),
	 ppl_insert_generator(X, vertex(-100*'$VAR'(0) - 5*'$VAR'(1),8)).
    the polyhedron referenced by X is defined to be single vertex 
    (-12.5, -0.625, 0)^T in the 3-dimensional vector space.
 
ppl_insert_constraints(+Address, +ConstrList)

   Adds the constraints in list ConstrList to the polyhedra 
   referenced by Address.

ppl_insert_generators(+Address, +GenList)

   Adds the generators in list GenList to the polyhedra 
   referenced by Address.

ppl_remove_dimensions(+Address, +VarList)

   The dimensions corresponding to the variables in list
   VarList are removed from the polyhedron P referenced by Address.
   Each variable must have the form '$VAR'(N) where N is a non-negative 
   integer =< the dimensions of P.
   There must be no repetitions in the list.

ppl_remove_higher_dimensions(+Address, +Int)

   The dimensions higher than Int are removed from the
   polyhedron P referenced by Address.

ppl_add_dimensions_and_project(+Address, +Int)

   Adds Int new dimensions and embeds the old polyheron
   referenced by Address in the new space.

ppl_add_dimensions_and_embed(+Address, +Int)

   Adds Int new dimensions and does not embed the old polyheron
   referenced by Address in the new space.

ppl_intersection_assign(+Address1, +Address2)

   Computes the intersection of the polyhedra referenced by
   Address1 and Address2 and  and places the result at Address1.

ppl_convex_hull_assign(+Address1, +Address2)

   Computes the convex hull of the polyhedra referenced by
   Address1 and Address2 and places the result at Address1.

ppl_widening_assign(+Address1, +Address2)

   Computes the widening between the polyhedra referenced by Address1
   and Address2 and places the result at Address1.

ppl_check_empty(+Address)

   Succeeds if and only if the polyhedron referenced by Address is empty.

*/

foreign_resource(ppl_sicstus,
[
	init(ppl_init),
	deinit(ppl_deinit),
	ppl_new_polyhedron,
	ppl_new_empty_polyhedron,
	ppl_copy_polyhedron,
	ppl_delete_polyhedron,
	ppl_space_dimension,
	ppl_insert_constraint,
	ppl_insert_generator,
	ppl_remove_dimensions,
	ppl_remove_higher_dimensions,
	ppl_add_dimensions_and_embed,
	ppl_add_dimensions_and_project,
	ppl_check_empty,
	ppl_get_constraints,
	ppl_get_generators,
	ppl_intersection_assign,
	ppl_convex_hull_assign,
	ppl_widening_assign
]).

foreign(ppl_new_polyhedron,
	c,
	ppl_new_polyhedron([-address], +integer)).
foreign(ppl_new_empty_polyhedron,
	c,
	ppl_new_empty_polyhedron([-address], +integer)).
foreign(ppl_copy_polyhedron,
	c,
	ppl_copy_polyhedron(+address, [-address])).
foreign(ppl_delete_polyhedron,
	c,
	ppl_delete_polyhedron(+address)).
foreign(ppl_space_dimension,
	c,
	ppl_space_dimension(+address, [-integer])).
foreign(ppl_insert_constraint,
	c,
	ppl_insert_constraint(+address, +term)).
foreign(ppl_insert_generator,
	c,
	ppl_insert_generator(+address, +term)).
foreign(ppl_remove_dimensions,
	c,
	ppl_remove_dimensions(+address, +term)).
foreign(ppl_remove_higher_dimensions,
	c,
	ppl_remove_higher_dimensions(+address, +integer)).
foreign(ppl_add_dimensions_and_project,
	c,
	ppl_add_dimensions_and_project(+address, +integer)).
foreign(ppl_add_dimensions_and_embed,
	c,
	ppl_add_dimensions_and_embed(+address, +integer)).
foreign(ppl_check_empty,
	c,
	ppl_check_empty(+address, [-integer])).
foreign(ppl_get_constraints,
	c,
	ppl_get_constraints(+address, -term)).
foreign(ppl_get_generators,
	c,
	ppl_get_generators(+address, -term)).
foreign(ppl_intersection_assign,
	c,
	ppl_intersection_assign(+address, +address)).
foreign(ppl_convex_hull_assign,
	c,
	ppl_convex_hull_assign(+address, +address)).
foreign(ppl_widening_assign,
	c,
	ppl_widening_assign(+address, +address)).

:- load_foreign_resource(ppl_sicstus).

ppl_check_empty(Polyhedron) :-
  ppl_check_empty(Polyhedron, 1).

ppl_insert_constraints(_Polyhedron, []).
ppl_insert_constraints(Polyhedron, [C|Constraints]) :-
  ppl_insert_constraint(Polyhedron, C),
  ppl_insert_constraints(Polyhedron, Constraints).

ppl_insert_generators(_Polyhedron, []).
ppl_insert_generators(Polyhedron, [G|Generators]) :-
  ppl_insert_generator(Polyhedron, G),
  ppl_insert_generators(Polyhedron, Generators).
