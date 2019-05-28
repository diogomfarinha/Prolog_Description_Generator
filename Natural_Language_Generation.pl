:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Vocabulary.pl').

%Natural language descriptions of procedures
%Prepared descriptions in both present tense and infinitive
procedure_description(Predicate,present,Desc):-
    Predicate=..[Name|Args],
    Name=print,
    pretty_variables(Args,PrettyArgs),
    atom_concat('prints ',PrettyArgs,Atom1),
    atom_concat(Atom1,' on the console',Desc).
procedure_description(Predicate,infinitive,Desc):-
    Predicate=..[Name|Args],
    Name=print,
    pretty_variables(Args,PrettyArgs),
    atom_concat('print ',PrettyArgs,Atom1),
    atom_concat(Atom1,' on the console',Desc).
procedure_description(Predicate,present,Desc):-
    Predicate=..[Name|Args],
    Name=print_line,
    pretty_variables(Args,PrettyArgs),
    atom_concat('prints ',PrettyArgs,Atom1),
    atom_concat(Atom1,' on the console and breaks line',Desc).
procedure_description(Predicate,infinitive,Desc):-
    Predicate=..[Name|Args],
    Name=print_line,
    pretty_variables(Args,PrettyArgs),
    atom_concat('print ',PrettyArgs,Atom1),
    atom_concat(Atom1,' on the console and break line',Desc).
procedure_description(Predicate,present,Desc):-
    Predicate=..[Name|Args],
    Name=read,
    pretty_variables(Args,PrettyArgs),
    atom_concat('prompts the user for ',PrettyArgs,Desc).
procedure_description(Predicate,infinitive,Desc):-
    Predicate=..[Name|Args],
    Name=read,
    pretty_variables(Args,PrettyArgs),
    atom_concat('prompt the user for ',PrettyArgs,Desc).
%Generate description in present tense 
procedure_description(Predicate,present,Desc):-
    generate_description(Predicate,present,Desc),!.
%Generate description in infinitive
procedure_description(Predicate,infinitive,Desc):-
    generate_description(Predicate,infinitive,Desc),!.

%Automatic generation of natural language descriptions
generate_description(Predicate,present,Desc):-
    Predicate=..[Name|Args],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    conjugate_verbs(Words,present,Conjugated),!,
    pretty_variables(Args,PrettyArgs),
    append(Conjugated,[PrettyArgs],DescList),
    atomic_list_concat(DescList,' ', Desc).
generate_description(Predicate,infinitive,Desc):-
    Predicate=..[Name|Args],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    pretty_variables(Args,PrettyArgs),
    append(Words,[PrettyArgs],DescList),
    atomic_list_concat(DescList,' ', Desc).

%Separate words in list of characters according to common programming conventions
separate_words(CharList,Words):-
    separate_words(CharList,[],Words),!.
separate_words([C|Rest],Current,[Word|Words]):-
    C='_',
    atom_chars(Word,Current),
    separate_words(Rest,[],Words).
separate_words([C|Rest],[],Words):-
    char_type(C,digit),
    separate_words(Rest,[],Words).
separate_words([C|Rest],Current,[Word|Words]):-
    char_type(C,digit),
    atom_chars(Word,Current),
    separate_words(Rest,[],Words).
separate_words([C|Rest],Current,[Word|Words]):-
    upper_lower(C, Lower),%upcase_atom is true for both lower and upper case
    atom_chars(Word,Current),
    char_code(Atom,Lower),
    separate_words(Rest,[Atom],Words).
separate_words([C|Rest],Current,Words):-
    append(Current,[C],NewCurrent),
    separate_words(Rest,NewCurrent,Words).
separate_words([],Current,[Word]):-
    atom_chars(Word,Current).

%Conjugate verbs in list
conjugate_verbs([Verb|Rest],present,[Conjugated|ConjugatedRest]):-
    verb(Verb),
    present_tense(Verb,Conjugated),
    conjugate_verbs(Rest,present,ConjugatedRest).
conjugate_verbs([Word|Rest],Tense,[Word|ConjugatedRest]):-
    conjugate_verbs(Rest,Tense,ConjugatedRest).
conjugate_verbs([],_,[]).

%Make variable listing more readable for natural language
pretty_variables([Var],Var).
pretty_variables([Head,Tail],Text):-
    atom_concat(Head, ' and ', Atom1),
    atom_concat(Atom1,Tail,Text).
pretty_variables([Head|Rest],Text):-
    pretty_variables(Rest,TextRest),
    atom_concat(Head,', ',Atom1),
    atom_concat(Atom1,TextRest,Text).

%Process condition into natural language
condition_description(A=B,Desc):-
    atom_concat(A,' is equal to ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A==B,Desc):-
    atom_concat(A,' is equal to ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A\=B,Desc):-
    atom_concat(A,' is different than ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A>B,Desc):-
    atom_concat(A,' is bigger than ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A<B,Desc):-
    atom_concat(A,' is less than ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A=<B,Desc):-
    atom_concat(A,' is equal or less than ',Atom1),
    atom_concat(Atom1,B,Desc).
condition_description(A>=B,Desc):-
    atom_concat(A,' is bigger or equal to ',Atom1),
    atom_concat(Atom1,B,Desc).

%Capitalize atom (Make first character uppercase)
capitalize(Atom,Capitalized):-
    atom_chars(Atom,[C|Rest]),
    upcase_atom(C, Up),
    atom_chars(Capitalized,[Up|Rest]).
