ppl_insert_constraints(_Polyhedron, []).
ppl_insert_constraints(Polyhedron, [C|Constraints]) :-
  ppl_insert_constraint(Polyhedron, C),
  ppl_insert_constraints(Polyhedron, Constraints).

ppl_insert_generators(_Polyhedron, []).
ppl_insert_generators(Polyhedron, [G|Generators]) :-
  ppl_insert_generator(Polyhedron, G),
  ppl_insert_generators(Polyhedron, Generators).
