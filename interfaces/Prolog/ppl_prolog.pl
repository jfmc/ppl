ppl_add_constraints(_Polyhedron, []).
ppl_add_constraints(Polyhedron, [C|Constraints]) :-
  ppl_add_constraint(Polyhedron, C),
  ppl_add_constraints(Polyhedron, Constraints).

ppl_add_generators(_Polyhedron, []).
ppl_add_generators(Polyhedron, [G|Generators]) :-
  ppl_add_generator(Polyhedron, G),
  ppl_add_generators(Polyhedron, Generators).
