
:- initialization(main).

eat_eol.

main :-
    ppl_initialize,
    common_main,
    ppl_finalize,
    halt.
