
:- initialization(main).
:- set_prolog_flag(language, iso).

eat_eol.

main :-
    set_prolog_flag(language, iso),
    ppl_init,
    common_main.
