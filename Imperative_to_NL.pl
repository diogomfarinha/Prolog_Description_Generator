:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').

procedure_description(print(X),['prints',X,'on the console']).
procedure_description(print_line(X),['prints',X,'on the console and breaks line']).
