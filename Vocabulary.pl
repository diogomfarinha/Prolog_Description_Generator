%Verbs
verb('process').
verb('remove').
verb('input').
verb('print').
verb('read').

%Present tenses
present_tense('process','processes').
present_tense('remove','removes').
present_tense('input','inputs').
present_tense('print','prints').
present_tense('read','reads').

%Conjugation of verbs
conjugate(Verb,present,Present):-
    verb(Verb),
    present_tense(Verb,Present),!.
conjugate(Verb,infinitive,Verb):-
    verb(Verb),!.

%Nouns
noun('password').
noun('student').
noun('name').
noun('age').
noun('status').

%Phrase linking adverbs
phrase_linker('then').
phrase_linker('afterwards').
phrase_linker('next').
phrase_linker('subsequently').
phrase_linker('consequently').