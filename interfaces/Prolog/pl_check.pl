all_check :-
incl_C,
incl_NNC,
strict_incl_C,
strict_incl_NNC,
equals_C,
equals_NNC,
not_equals_NNC,
copy_C_C,
copy_NNC_NNC,
copy_C_NNC,
get_cons_C,
get_gens_C,
get_cons_NNC,
get_gens_NNC,
space_C,
space_NNC,
inters_assign,
inters_assign_min,
polyhull_assign,
polyhull_assign_min,
polydiff_assign,
polydiff_assign_min,
widen_C,
lim_widen_C,
widen_NNC,
lim_widen_NNC,
add_con,
add_gen,
add_cons,
add_gens,
add_cons_min,
add_gens_min,
add_dim_cons,
remove_dim,
remove_high_dim,
affine,
affine_pre,
rel_cons,
rel_gens,
checks,
bounded_cons,
bounded_gens,
project,
embed.

incl_C :-
ppl_new_Polyhedron_from_dimension(c, 3, P1),
ppl_new_Polyhedron_from_dimension(c, 3, P2),
ppl_Polyhedron_contains_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

incl_NNC :-
ppl_new_NNC_Polyhedron_from_dimension(3, P1),
ppl_new_NNC_Polyhedron_from_dimension(3, P2),
ppl_Polyhedron_contains_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

strict_incl_C :-
ppl_new_Polyhedron_from_dimension(c, 3, P1),
ppl_Polyhedron_check_universe(P1),
ppl_new_Polyhedron_empty_from_dimension(c, 3, P2),
ppl_Polyhedron_check_empty(P2),
ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

strict_incl_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P2),
ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

equals_C :-
ppl_new_Polyhedron_from_dimension(c, 3, P1),
ppl_new_Polyhedron_from_dimension(c, 3, P2),
ppl_Polyhedron_equals_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

equals_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
ppl_new_Polyhedron_from_dimension(nnc, 3, P2),
ppl_Polyhedron_equals_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

not_equals_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P2),
\+ ppl_Polyhedron_equals_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

copy_C_C :-
ppl_new_Polyhedron_from_dimension(c, 3, P1),
ppl_new_Polyhedron_from_Polyhedron(c, P1, c, P2),
ppl_Polyhedron_equals_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

copy_NNC_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
ppl_new_Polyhedron_from_Polyhedron(nnc, P1, nnc, P2),
ppl_Polyhedron_equals_Polyhedron(P1, P2),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

copy_C_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
ppl_new_Polyhedron_from_Polyhedron(nnc, P1, c, P2),
ppl_new_Polyhedron_from_Polyhedron(c, P2, nnc, P1a),
ppl_Polyhedron_equals_Polyhedron(P1, P1a),
ppl_new_Polyhedron_from_Polyhedron(nnc, P1a, c, P2a),
ppl_Polyhedron_equals_Polyhedron(P2, P2a),
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P1a),
ppl_delete_Polyhedron(P2),
ppl_delete_Polyhedron(P2a),
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_ConSys(nnc, [3 >= A, 4 > A, 4*A + B - 2*C >= 5], P3),
ppl_new_Polyhedron_from_Polyhedron(nnc, P3, c, P4),
ppl_Polyhedron_get_constraints(P4, CS),
CS = [4*A + 1*B + -2*C >= 5, -1*A >= -3],
ppl_delete_Polyhedron(P3),
ppl_delete_Polyhedron(P4).

get_cons_C :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_ConSys(c, [3 >= A, 4*A + B - 2*C >= 5], P),
ppl_Polyhedron_get_constraints(P, CS),
CS = [4*A + 1*B + -2*C >= 5, -1*A >= -3],
ppl_delete_Polyhedron(P).

get_gens_C :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_GenSys(c, [point(1*A + 1*B + 1*C, 1),
                                  point(1*A + 1*B + 1*C, 1)], P),
ppl_Polyhedron_get_generators(P, GS),
GS = [point(1*A + 1*B + 1*C), point(1*A + 1*B + 1*C)],
ppl_delete_Polyhedron(P).

get_cons_NNC :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_ConSys(nnc, [3 > A, 4*A + B - 2*C >= 5], P),
ppl_Polyhedron_get_constraints(P, CS),
CS = [4*A + 1*B + -2*C >= 5, -1*A > -3],
ppl_delete_Polyhedron(P).

get_gens_NNC :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_GenSys(nnc, [
            point(1*A + 1*B + 1*C, 1),
            closure_point(1*A + 1*B + 1*C, 1)
                                   ], P),
ppl_Polyhedron_get_generators(P, GS),
GS = [point(1*A + 1*B + 1*C), closure_point(1*A + 1*B + 1*C)],
ppl_delete_Polyhedron(P).

space_C :-
ppl_new_Polyhedron_from_dimension(c, 3, P),
ppl_Polyhedron_space_dimension(P, N),
N = 3,
ppl_delete_Polyhedron(P).

space_NNC :-
ppl_new_Polyhedron_from_dimension(nnc, 3, P),
ppl_Polyhedron_space_dimension(P, N),
N = 3,
ppl_delete_Polyhedron(P).

inters_assign :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(nnc, [point(0), point(B), 
                                    point(A), point(A, 2)], P1),
ppl_new_Polyhedron_from_GenSys(nnc, [point(0), point(A), 
                                    point(A + B), point(A, 2)], P2),
ppl_Polyhedron_intersection_assign(P1, P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A+ -1*B >= 0, 1*B >= 0, -1*A + -1*B >= -1],
GS = [point(1*A + 1*B, 2), closure_point(1*A + 1*B, 2), point(1*A),
      closure_point(1*A), point(0), closure_point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

inters_assign_min :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(nnc, [point(0), point(B), 
                                    point(A), point(A, 2)], P1),
ppl_new_Polyhedron_from_GenSys(nnc, [point(0), point(A), point(A + B)], P2),
ppl_Polyhedron_intersection_assign_and_minimize(P1, P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A + -1*B >=0, 1*B >= 0, -1*A + -1*B >= -1],
GS = [point(1*A + 1*B, 2), closure_point(1*A + 1*B, 2), 
      point(1*A), closure_point(1*A), point(0), closure_point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

polyhull_assign :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(B),
                                  point(A), point(A,2)], P1),
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(A), point(A + B),
                                  point(A, 2)], P2),
ppl_Polyhedron_poly_hull_assign(P1, P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A >= 0, 1*B >= 0, -1*B >= -1, -1*A >= -1],
GS = [point(1*A + 1*B), point(1*A, 2), point(1*A), point(1*B), point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

polyhull_assign_min :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(B), point(A),
                                  point(A, 2)], P1),
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(A), point(A + B)], P2),
ppl_Polyhedron_poly_hull_assign_and_minimize(P1,P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A >= 0, 1*B >= 0, -1*B >= -1, -1*A >= -1],
GS = [point(1*A + 1*B), point(1*A), point(1*B), point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

polydiff_assign :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(B), point(A),
                                  point(A,2)],P1),
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(A), point(A + B),
                                  point(A,2)],P2),
ppl_Polyhedron_poly_difference_assign(P1, P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A >= 0, 1*B >= 0, -1*A + -1*B >= -1],
GS = [point(1*A + 1*B, 2), point(1*A), point(1*B), point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

polydiff_assign_min :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(B),
                                  point(A), point(A,2)], P1),
ppl_new_Polyhedron_from_GenSys(c, [point(0), point(A), point(A + B)], P2),
ppl_Polyhedron_poly_difference_assign_and_minimize(P1, P2),
ppl_Polyhedron_get_generators(P1, GS),
ppl_Polyhedron_get_constraints(P1, CS),
CS = [1*A >= 0, 1*B >= 0, -1*A + -1*B >= -1],
GS = [point(1*A), point(1*B), point(0)],
ppl_delete_Polyhedron(P1),
ppl_delete_Polyhedron(P2).

widen_C :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 2, P),
ppl_new_Polyhedron_from_ConSys(c, [A >= 1, B >= 0], Q),
ppl_Polyhedron_widening_assign(P, Q),
ppl_Polyhedron_get_constraints(P, CP),
ppl_Polyhedron_get_constraints(Q, CQ), 
CP = [],
CQ = [1*A >= 1, 1*B >= 0],
ppl_delete_Polyhedron(P),
ppl_delete_Polyhedron(Q).

lim_widen_C :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 2, P),
ppl_new_Polyhedron_from_ConSys(c, [A >= 1, B >= 0], Q),
ppl_Polyhedron_add_constraints_and_minimize(Q, 
     [A >= 1, B >= 0]),
ppl_Polyhedron_limited_widening_assign(P, Q,
     [A >= 2, B >= 1]),
ppl_Polyhedron_get_constraints(P, CS),
CS = [1*A >= 2, 1*B >= 1],
ppl_delete_Polyhedron(P),
ppl_delete_Polyhedron(Q).

widen_NNC :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_new_Polyhedron_from_ConSys(nnc, [A >= 1, B >= 0], Q),
ppl_Polyhedron_widening_assign(P, Q),
ppl_Polyhedron_get_constraints(P, CP),
ppl_Polyhedron_get_constraints(Q, CQ),
CP = [],
CQ = [1*A >= 1, 1*B >= 0],
ppl_delete_Polyhedron(P),
ppl_delete_Polyhedron(Q).

lim_widen_NNC :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_new_Polyhedron_from_ConSys(nnc, [A >= 1, B >= 0], Q),
ppl_Polyhedron_add_constraints_and_minimize(Q, 
     [A >= 1, B >= 0]),
ppl_Polyhedron_limited_widening_assign(P, Q,
     [A >= 2, B >= 1]),
ppl_Polyhedron_get_constraints(P, CS),
CS = [1*A >= 2, 1*B >= 1],
ppl_delete_Polyhedron(P),
ppl_delete_Polyhedron(Q).

add_con :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 2, P),
ppl_Polyhedron_add_constraint(P, 
     A - B >= 1), 
ppl_Polyhedron_get_constraints(P, CS),
CS = [1*A + -1*B >= 1],
ppl_delete_Polyhedron(P).

add_gen :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 2, P),
ppl_Polyhedron_add_generator(P, 
     point(1*A + 1*B, 1)),
ppl_Polyhedron_get_generators(P, GS),
GS = [point(1*A + 1*B), point(0), line(1*A), line(1*B)],
ppl_delete_Polyhedron(P).

add_cons :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_dimension(c, 3, P),
ppl_Polyhedron_add_constraints(P, 
     [A >= 1, B >= 0, 4*A + B - 2*C >= 5]),
ppl_Polyhedron_get_constraints(P, CS),
CS = [4*A + 1*B + -2*C >= 5, 1*A >= 1, 1*B >= 0],
ppl_delete_Polyhedron(P).

add_gens :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
ppl_Polyhedron_add_generators(P, 
     [point(1*A + 1*B + 1*C, 1), ray(1*A), ray(2*A),
      point(1*A + 1*B + 1*C, 1),
      point(-100*A - 5*B, 8)]),
ppl_Polyhedron_get_generators(P, GS), 
GS = [ray(2*A), point(1*A + 1*B + 1 *C), ray(1*A), point(-100*A + -5*B, 8)],
ppl_delete_Polyhedron(P).

add_cons_min :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 2, P),
ppl_Polyhedron_add_constraints_and_minimize(P, 
     [A >= 1, B >= 0]),
ppl_Polyhedron_get_constraints(P, CS), 
CS = [1*A >= 1, 1*B >= 0],
ppl_delete_Polyhedron(P).

add_gens_min :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
ppl_Polyhedron_add_generators_and_minimize(P, 
     [point(1*A + 1*B + 1*C, 1),
      ray(1*A), ray(2*A), point(1*A + 1*B + 1*C, 1)]),
ppl_Polyhedron_get_generators(P, GS), 
GS = [point(1*A + 1*B + 1*C), ray(1*A)],
ppl_delete_Polyhedron(P).

add_dim_cons :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
D = '$VAR'(3), E = '$VAR'(4), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_dimensions_and_constraints(P, 
     [A > 1, B >= 0, C >= 0]),
ppl_Polyhedron_get_constraints(P, CS), 
CS = [1*C > 1, 1*D >= 0, 1*E >= 0],
ppl_delete_Polyhedron(P).

remove_dim :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_dimension(nnc, 3, P),
ppl_Polyhedron_add_constraints(P, 
     [A > 1, B >= 0, C >= 0]),
ppl_Polyhedron_remove_dimensions(P,[B]),
ppl_Polyhedron_get_constraints(P, CS),
CS = [1*A > 1, 1*B >= 0],
ppl_delete_Polyhedron(P).

remove_high_dim :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
ppl_new_Polyhedron_from_dimension(nnc, 3, P),
ppl_Polyhedron_add_constraints(P, 
     [A > 1, B >= 0, C >= 0]),
ppl_Polyhedron_get_constraints(P, CS1),
ppl_Polyhedron_remove_higher_dimensions(P, 1),
ppl_Polyhedron_get_constraints(P, CS2), 
CS1 = [1*A > 1, 1*B >= 0, 1*C >= 0],
CS2 = [1*A > 1],
ppl_delete_Polyhedron(P).

affine :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_constraint(P, 
     A - B = 1),
ppl_Polyhedron_get_constraints(P, CS),
ppl_Polyhedron_affine_image(P, A, A + 1, 1),
ppl_Polyhedron_get_constraints(P, CS1),
CS = [1*A + -1*B = 1],
CS1 = [1*A + -1*B = 2],
ppl_delete_Polyhedron(P).

affine_pre :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_constraint(P, 
     A + B >= 10),
ppl_Polyhedron_get_constraints(P, CS),
ppl_Polyhedron_affine_preimage(P, A, A + 1, 1),
ppl_Polyhedron_get_constraints(P, CS1),
CS = [1*A + 1*B >= 10],
CS1 = [1*A + 1*B >= 9],
ppl_delete_Polyhedron(P).

rel_cons :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(c, 3, P),
ppl_Polyhedron_add_constraints_and_minimize(P, 
     [A >= 1, B >= 0]),
R = [is_disjoint],
ppl_Polyhedron_relation_with_constraint(P, A = 0, R),
ppl_delete_Polyhedron(P).

rel_gens :-
A = '$VAR'(0), B = '$VAR'(1),  C = '$VAR'(2),
ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P),
ppl_Polyhedron_add_generators_and_minimize(P, 
     [point(1*A + 1*B + 1*C, 1)]),
ppl_Polyhedron_relation_with_generator(P, point(1*A), R),
R = [],
ppl_delete_Polyhedron(P).

checks :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
ppl_new_Polyhedron_from_dimension(nnc, 3, P),
ppl_Polyhedron_check_universe(P),
ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P1),
ppl_Polyhedron_check_empty(P1),
ppl_Polyhedron_add_generators_and_minimize(P1, 
     [point(1*A + 1*B + 1*C, 1)]),
ppl_Polyhedron_is_bounded(P1),
ppl_delete_Polyhedron(P),
ppl_delete_Polyhedron(P1).

bounded_cons :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_constraints_and_minimize(P, 
     [A >= 0, B >= 0, 1 >= A, B =< 1]),
ppl_Polyhedron_is_bounded(P),
ppl_delete_Polyhedron(P).

bounded_gens :-
ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P),
ppl_Polyhedron_add_generators_and_minimize(P, 
     [point(1*'$VAR'(0) + 1*'$VAR'(1) + 1*'$VAR'(2), 1),
      point(2*'$VAR'(0) + 2*'$VAR'(1) + 2*'$VAR'(2), 1)]),
ppl_Polyhedron_is_bounded(P),
ppl_delete_Polyhedron(P).

project :-
A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), D = '$VAR'(3), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_constraints_and_minimize(P, 
     [A >= 1, B >= 0]),
ppl_Polyhedron_add_dimensions_and_project(P, 2),
ppl_Polyhedron_get_constraints(P, CS),
CS = [1*A >= 1, 1*B >= 0, 1*C = 0, 1*D = 0],
ppl_delete_Polyhedron(P).

embed :-
A = '$VAR'(0), B = '$VAR'(1), 
ppl_new_Polyhedron_from_dimension(nnc, 2, P),
ppl_Polyhedron_add_constraints_and_minimize(P, 
     [A >= 1, B >= 0]),
ppl_Polyhedron_add_dimensions_and_embed(P, 2),
ppl_Polyhedron_get_constraints(P,CS), 
CS = [1*A >= 1, 1*B >= 0],
ppl_delete_Polyhedron(P).
