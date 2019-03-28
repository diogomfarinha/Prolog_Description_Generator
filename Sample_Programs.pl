%Sample programs to be analysed

p(displayStatus).
p(inputPassword).

status(anna, online).
status(john, online).
status(michael, offline).
status(maria, offline).
status(paul, online).

displayStatus:-
    status(Person,Status),
    write(Person),write(' is '),write(Status),nl,
    fail.
displayStatus.

age(anna,24).
age(john,27).
age(michael,54).

country(anna,usa).
country(john,portugal).
country(michael,spain).

inputPassword:-
    repeat,
    write('Insert password:'),nl,
    read(Password),
    processPassword(Password).
processPassword(Password):-
    Password=11037,
    write('PASSWORD ACCEPTED').

remove_from_list(X,[X|Rest],List):-
    remove_from_list(X,Rest,List).
remove_from_list(X,[Head|Rest],[Head|List]):-
    X\=Head,
    remove_from_list(X,Rest,List).
remove_from_list(_,[],[]).


removeOddNumbers([N|Rest],[N|List]):-
    0 is N mod 2,
    removeOddNumbers(Rest,List),!.
removeOddNumbers([N|Rest],List):-
    \+(0 is N mod 2),
    removeOddNumbers(Rest,List),!.
removeOddNumbers([],[]).

student(rui).

print_all_students():-
    student(X),
    write(X),nl,
    fail.
print_all_students().

printList([Head|Rest]):-
	write(Head),nl,
	printList(Rest).
printList([]).

printList2([Head|Rest],2):-
	write(Head),write(2),nl,
	printList2(Rest,2).
printList2([],_).

testfail(P):-
    status(_,_),
    write(P),
    fail.
testfail(P):-
    status(_,_),
    write(P),
    fail.
testfail(P):-
    status(_,_),
    write(P),
    fail.
testfail(_).

testfail2(_):-
    write('Go'),nl,
    status(anna,Y),
    write(Y),
    fail.

doubleFail:-
    status(X,_),
    write(X),
    status(Y,_),
    write(Y),
    fail.

test1:-
    test2.
test2:-
    test3.
test3:-
    write(stuff).

testRepeat:-
    repeat,
    write(yeah),
    write(woo).
testRepeat:-
    repeat,
    write(woo),
    write(yeah).
testRepeat:-
    repeat,
    write(woo),
    write(yeah).

testComp:-
    repeat,
    status(X,_),
    write(X),
    fail.

teste123:-
    status(anna,P),
    write(P),
    doStuff,
    fail.
doStuff.

%1] Este é apenas para veres um programa simples que causa uma repetição do tamanho da lista L, sem recorrer a factos
print_list(L):-
    member(X, L),
    write(X), nl,
    fail.
print_list(_).
 
%2] Este é semelhante mas tem uma filtragem
print_positive_numbers(L):-
    member(X, L),
    positive_number(X),
    write(X), nl,
    fail.
print_positive_numbers(_).
 
positive_number(X):-
    number(X), X > 0.
 
%3] Este contem um filtro definido de maneira mais difícil
print_selected_values(L):-
    member(X, L),
    selected_value(X),
    write(X), nl,
    fail.
print_selected_values(_).
 
selected_value(Y):- member(Y, [a, b]).
selected_value(7).
selected_value(0).
                                                    
%4] Este mostra o exemplo de um programa que pode originar mais do que uma descrição, ambas razoáveis (uma dela mantém a relação de composicinalidade do código original, outra não mantém)
print_values_and_their_relatives:-
    value(X),
    write(X), write('\'s'), tab(1), write(relatives), nl,
    print_relatives(X),
    fail.
print_values_and_their_relatives.
 
print_relatives(X):-
    relative(X, R),
    write(R), nl,
    fail.
print_relatives(_).
 
value(1).
value(2).
value(3).
 
relative(1, -1).
relative(1, 0).
relative(1, 2).
 
relative(2, 4).
relative(2, 8).
 
relative(3, a).
