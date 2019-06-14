:- ensure_loaded('Sample_Facts.pl').
%Sample programs to be analysed

%Fail loop
displayStatus:-
    status(Person,Status),
    write(Person),write(' is '),write(Status),nl,
    fail.
displayStatus.

%Double fail loop
displayAge:-
    status(Person,Status),
    age(Person,Age),
    write(Person),write(' is '),write(Age),write(' years old and '),write(Status),nl,
    fail.
displayAge.

%Double fail loop with if condition
displayOld:-
    status(Person,Status),
    age(Person,Age),
    Age>80,
    write(Person),write(' is '),write(Age),write(' years old and '),write(Status),nl,
    fail.
displayOld.

%Triple fail loop
displayStudents:-
    university(U),
    department(U,D),
    student(D,S),
    write(S),nl,
    fail.
displayStudents.

%Repeat loop
inputPassword:-
    repeat,
    write('Insert password:'),nl,
    read(Password),
    processPassword(Password).
processPassword(Password):-
    Password=11037,
    write('PASSWORD ACCEPTED').

%Double repeat loop
inputCredentials:-
    repeat,
    write('Username:'),nl,
    read(Name),
    name(Name),
    write('Password:'),nl,
    read(P),
    password(Name,P),
    write('Credentials accepted').

%Recursive list iteration
printList([Head|Rest]):-
	write(Head),nl,
	printList(Rest).
printList([]).

%Recursive list iteration with 2 arguments
printList2([Head|Rest],X):-
	write(Head),write(' '),write(X),nl,
	printList2(Rest,X).
printList2([],_).

%Recursive list iteration with 3 arguments
printList3(X,[Head|Rest],Y):-
	write(X),write(' '),write(Head),write(' '),write(Y),nl,
	printList3(X,Rest,Y).
printList3(_,[],_).

%Recursive list iteration with if-else clauses
findElements(N,[Head|Rest]):-
    Head==N,
    write('Found it! '),
    findElements(N,Rest).
findElements(N,[Head|Rest]):-
    Head\=N,
    write('Nope '),
    findElements(N,Rest).
findElements(_,[]).

%Recursive list iteration with if-else clauses
biggerThan(N,[Head|Rest]):-
    Head>N,
    write(Head),write(' is bigger than '),write(N),nl,
    biggerThan(N,Rest).
biggerThan(N,[Head|Rest]):-
    Head=<N,
    write(Head),write(' is not bigger than '),write(N),nl,
    biggerThan(N,Rest).
biggerThan(_,[]).

%Sucessive fail loops.
printStudentsTwice:-
    student(_,S),
    write(S),nl,
    fail.
printStudentsTwice:-
    student(_,S),
    write(S),nl,
    fail.
printStudentsTwice.

%Successive fail loops
displayPeople:-
    gender(Person,female),
    write(Person),nl,
    fail.
displayPeople:-
    gender(Person,male),
    write(Person),nl,
    fail.
displayPeople.



 