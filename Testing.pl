:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Test_Programs.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Imperative_to_NL.pl').
:- ensure_loaded('Natural_Language_Generation.pl').

%Testing only.
%Loads all modules

%Test prolog_to_imperative
test1:-
    prolog_to_imperative(displayStatus/0),
    prolog_to_imperative(displayStudents/0),
    prolog_to_imperative(inputPassword/0),
    prolog_to_imperative(inputCredentials/0),
    prolog_to_imperative(printList/1),
    prolog_to_nl(biggerThan/2).

%Test prolog_to_nl
test2:-
    prolog_to_nl(displayStatus/0),
    prolog_to_nl(displayStudents/0),
    prolog_to_nl(inputPassword/0),
    prolog_to_nl(inputCredentials/0),
    prolog_to_nl(printList/1),
    prolog_to_nl(biggerThan/2).

%Results chosen for peer evaluation
results:-
    prolog_to_nl(displayStatus/0),
    prolog_to_nl(displayStudents/0),
    prolog_to_nl(displayPeople/0),
    prolog_to_nl(inputPassword/0),
    prolog_to_nl(inputCredentials/0),
    prolog_to_nl(printList/1),
    prolog_to_nl(biggerThan/2).


