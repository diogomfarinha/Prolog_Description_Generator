:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Vocabulary.pl').

%Natural language descriptions of procedures
%Prepared descriptions in both present tense and infinitive
procedure_description(Predicate,Tense,Desc):-
    Predicate=..[Name|Args],
    Name=print,
    remove_quotes_in_args(Args,CleanArgs),
    atomic_list_concat(CleanArgs,'', AtomArgs),
    conjugate(print,Tense,Conjugated),
    atom_concat(Conjugated,' \"',Atom1),
    atom_concat(Atom1,AtomArgs,Atom2),
    atom_concat(Atom2,'\" on the console',Desc).
procedure_description(Predicate,Tense,Desc):-
    Predicate=..[Name|Args],
    Name=print_line,
    remove_quotes_in_args(Args,CleanArgs),
    atomic_list_concat(CleanArgs,'', AtomArgs),
    conjugate(print,Tense,Conjugated),
    atom_concat(Conjugated,' \"',Atom1),
    atom_concat(Atom1,AtomArgs,Atom2),
    atom_concat(Atom2,'\" on the console',Desc).
procedure_description(read(_),Tense,Conjugation):-
    conjugate(read,Tense,Conjugation),!.
%Generate description in present tense 
procedure_description(Predicate,present,Desc):-
    generate_description(Predicate,present,Desc),!.
%Generate description in infinitive
procedure_description(Predicate,infinitive,Desc):-
    generate_description(Predicate,infinitive,Desc),!.

%Automatic generation of natural language descriptions.
%Handle special case of 0 arguments made pretty for imperative descriptions
generate_description(Predicate,Conjugation,Desc):-
    Predicate=..[Name|['']],
    NewPred=..[Name|[]],
    generate_description(NewPred,Conjugation,Atom),
    atom_length(Atom, Length),
    Len is Length-1,
    sub_atom(Atom,0,Len,_,Desc).%Remove useless space since there are no arguments
%Predicate name is a noun equal to Variable name
generate_description(Predicate,_,Arg):-
    Predicate=..[Name|[Arg]],
    atom_chars(Name, CharList),
    separate_words(CharList,[Word]),
    string_lower(Arg,Word).
%Predicate name contains variable name
generate_description(Predicate,Tense,Desc):-
    Predicate=..[Name|[Arg]],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    string_lower(Arg,Word),
    remove_quotation_marks(Word,NewWord),
    delete(Words,NewWord,NewWords),
    conjugate_verbs(NewWords,Tense,Conjugated),!,
    append(Conjugated,[Arg],DescList),
    atomic_list_concat(DescList,' ', Desc).
generate_description(Predicate,Tense,Desc):-
    Predicate=..[Name|Args],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),
    conjugate_verbs(Words,Tense,Conjugated),!,
    pretty_enumeration(Args,PrettyArgs),
    append(Conjugated,[PrettyArgs],DescList),
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
conjugate_verbs([Verb|Rest],Tense,[Conjugated|ConjugatedRest]):-
    verb(Verb),
    conjugate(Verb,Tense,Conjugated),
    conjugate_verbs(Rest,Tense,ConjugatedRest).
conjugate_verbs([Word|Rest],Tense,[Word|ConjugatedRest]):-
    conjugate_verbs(Rest,Tense,ConjugatedRest).
conjugate_verbs([],_,[]).

%Make variable listing more readable for natural language
pretty_enumeration([],'').
pretty_enumeration([Var],Var).
pretty_enumeration([Head,Tail],Text):-
    atom_concat(Head, ' and ', Atom1),
    atom_concat(Atom1,Tail,Text).
pretty_enumeration([Head|Rest],Text):-
    pretty_enumeration(Rest,TextRest),
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

%Remove quotation marks from atoms in list
remove_quotes_in_args([Atom|Rest],[Clean|CleanRest]):-
    remove_quotation_marks(Atom,Clean),
    remove_quotes_in_args(Rest,CleanRest).
remove_quotes_in_args([],[]).

%Remove quotation marks from atom
remove_quotation_marks(Atom,Removed):-
    atom_chars(Atom,Chars),
    remove_quotation_chars(Chars,CleanChars),
    atom_chars(Removed,CleanChars).

%Remove quotation mark characters
remove_quotation_chars(['/','"'|Rest],Removed):-
    remove_quotation_chars(Rest,Removed).
remove_quotation_chars(['"'|Rest],Removed):-
    remove_quotation_chars(Rest,Removed).
remove_quotation_chars([C|Rest],[C|Removed]):-
    remove_quotation_chars(Rest,Removed).
remove_quotation_chars([],[]).

%Get first noun in list of words
get_first_noun([Noun|_],Noun):-
    noun(Noun).
get_first_noun([_|Rest],Noun):-
    get_first_noun(Rest,Noun).
