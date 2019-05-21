:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Vocabulary.pl').

%Natural language descriptions of procedures
%Prepared descriptions in both present tense and infinitive
procedure_description(print(X),present,['prints',X,'on the console']).
procedure_description(print(X),infinitive,['print',X,'on the console']).
procedure_description(print_line(X),present,['prints',X,'on the console and breaks line']).
procedure_description(print_line(X),infinitive,['prints',X,'on the console and breaks line']).
%Generate description in present tense 
procedure_description(Predicate,present,Desc):-
    generate_description(Predicate,present,Desc).
%Generate description in infinitive
procedure_description(Predicate,infinitive,Desc):-
    generate_description(Predicate,infinitive,Desc).

%Automatic generation of natural language descriptions
generate_description(Predicate,present,Desc):-
    Predicate=..[Name|Args],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    conjugate_verbs(Words,present,Conjugated),!,
    append(Conjugated,Args,Desc).
generate_description(Predicate,infinitive,Desc):-
    Predicate=..[Name|Args],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    append(Words,Args,Desc).

%Separate words in list of characters according to common programming conventions
separate_words(CharList,Words):-
    separate_words(CharList,[],Words),!.
separate_words([C|Rest],Current,[Word|Words]):-
    C='_',
    atom_chars(Word,Current),
    separate_words(Rest,[],Words).
separate_words([C|Rest],Current,[Word|Words]):-
    upper_lower(C, Lower),
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



