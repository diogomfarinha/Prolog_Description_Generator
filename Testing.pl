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
    prolog_to_imperative(displayAge/0),
    prolog_to_imperative(displayOld/0),
    prolog_to_imperative(displayStudents/0),
    prolog_to_imperative(inputPassword/0),
    prolog_to_imperative(inputCredentials/0),
    prolog_to_imperative(printList/1),
    prolog_to_imperative(printList2/2),
    prolog_to_imperative(printList3/3),
    prolog_to_imperative(findElements/2),
    prolog_to_imperative(biggerThan/2),
    prolog_to_imperative(printStudentsTwice/0),
    prolog_to_imperative(displayPeople/0).

%Test prolog_to_nl
test2:-
    prolog_to_nl(displayStatus/0),
    prolog_to_nl(displayAge/0),
    prolog_to_nl(displayOld/0),
    prolog_to_nl(displayStudents/0),
    prolog_to_nl(inputPassword/0),
    prolog_to_nl(inputCredentials/0),
    prolog_to_nl(printList/1),
    prolog_to_nl(printList2/2),
    prolog_to_nl(printList3/3),
    prolog_to_nl(findElements/2),
    prolog_to_nl(biggerThan/2),
    prolog_to_nl(printStudentsTwice/0),
    prolog_to_nl(displayPeople/0).

%Results chosen for peer evaluation
results:-
    tell('results.txt'),
    prolog_to_nl(displayStatus/0),nl,
    prolog_to_nl(displayStudents/0),nl,
    prolog_to_nl(displayPeople/0),nl,
    prolog_to_nl(inputPassword/0),nl,
    prolog_to_nl(inputCredentials/0),nl,
    prolog_to_nl(printList/1),nl,
    prolog_to_nl(biggerThan/2),
    told.


