
:- set_prolog_flag(language, iso).

:- ensure_loaded('ppl_sicstus.pl').
:- ensure_loaded('../clpq').

eat_eol :-
	get_code(user_input, _EOL).

main :-
    set_prolog_flag(language, iso),	% FIXME: this is not ISO Prolog
    nofileerrors,			% FIXME: this is not ISO Prolog
    common_main.
