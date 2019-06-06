:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Natural_Language_Generation.pl').
:- ensure_loaded('Vocabulary.pl').

%Converts Prolog predicates into imperative-style descriptions and then
%into natural language descriptions.
%Receives predicates in format Name/Arity
prolog_to_nl(Name/0):-
    prolog_to_imperative_info(Name/0,_,Body),!,
    delete_tags_from_list(Body,[Clause|CleanBody]),
    add_clause_phrase_linkers(CleanBody,[],LinkedBody),!,
    process_body([Clause|LinkedBody],ProcessedBody),!,
    process_loop_conjunctions(ProcessedBody,ProcessedLoops),!,
    filter_subjects(ProcessedLoops,FilteredSubjects),!,
    process_punctuation(FilteredSubjects,ProcessedPunctuation),!,
    process_conjunctions(ProcessedPunctuation,ProcessedConjunctions),!,
    process_subjects(Name,ProcessedConjunctions,ProcessedSubjects),!,
    process_phrases(ProcessedSubjects,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_header(Head,Header),
    delete_tags_from_list(Body,[Clause|CleanBody]),
    add_clause_phrase_linkers(CleanBody,[],LinkedBody),!,
    process_body([Clause|LinkedBody],ProcessedBody),!,
    process_loop_conjunctions(ProcessedBody,ProcessedLoops),!,
    filter_subjects(ProcessedLoops,FilteredSubjects),!,
    process_punctuation(FilteredSubjects,ProcessedPunctuation),!,
    process_conjunctions(ProcessedPunctuation,ProcessedConjunctions),!,
    process_subjects(Name,ProcessedConjunctions,ProcessedSubjects),!,
    process_phrases([Header|ProcessedSubjects],Text),!,
    nl,nl,
    write(Text).

%Developer version
prolog_to_nl_dev(Name/0):-
    prolog_to_imperative_info(Name/0,_,Body),!,
    nl,nl,
    write(body:Body),nl,
    delete_tags_from_list(Body,[Clause|CleanBody]),
    writeq(clean:[Clause|CleanBody]),nl,
    add_clause_phrase_linkers(CleanBody,[],LinkedBody),!,
    writeq(linked:LinkedBody),nl,
    process_body([Clause|LinkedBody],ProcessedBody),!,
    writeq(body:ProcessedBody),nl,
    process_loop_conjunctions(ProcessedBody,ProcessedLoops),!,
    writeq(loop:ProcessedLoops),nl,
    filter_subjects(ProcessedLoops,FilteredSubjects),!,
    writeq(filtered:FilteredSubjects),nl,
    process_punctuation(FilteredSubjects,ProcessedPunctuation),!,
    writeq(punctuation:ProcessedPunctuation),nl,
    process_conjunctions(ProcessedPunctuation,ProcessedConjunctions),!,
    writeq(conjunctions:ProcessedConjunctions),nl,
    process_subjects(Name,ProcessedConjunctions,ProcessedSubjects),!,
    writeq(subjects:ProcessedSubjects),nl,
    process_phrases(ProcessedSubjects,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl_dev(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    nl,nl,
    write(body:Body),nl,
    process_header(Head,Header),
    writeq(header:Header),nl,
    delete_tags_from_list(Body,[Clause|CleanBody]),
    writeq(clean:[Clause|CleanBody]),nl,
    add_clause_phrase_linkers(CleanBody,[],LinkedBody),!,
    writeq(linked:LinkedBody),nl,
    process_body([Clause|LinkedBody],ProcessedBody),!,
    writeq(body:ProcessedBody),nl,
    process_loop_conjunctions(ProcessedBody,ProcessedLoops),!,
    writeq(loop:ProcessedLoops),nl,
    filter_subjects(ProcessedLoops,FilteredSubjects),!,
    writeq(filtered:FilteredSubjects),nl,
    process_punctuation(FilteredSubjects,ProcessedPunctuation),!,
    writeq(punctuation:ProcessedPunctuation),nl,
    process_conjunctions(ProcessedPunctuation,ProcessedConjunctions),!,
    writeq(conjunctions:ProcessedConjunctions),nl,
    process_subjects(Name,ProcessedConjunctions,ProcessedSubjects),!,
    writeq(subjects:ProcessedSubjects),nl,
    process_phrases([Header|ProcessedSubjects],Text),!,
    nl,nl,
    write(Text).

%Delete tags relevant to imperative style descriptions
delete_tags_from_list([List|Rest],[Clean|CleanRest]):-
    delete_tags(List,Clean),
    delete_tags_from_list(Rest,CleanRest).
delete_tags_from_list([],[]).
delete_tags(List,Clean_List):-
    delete(List,tag:indent,List2),
    delete(List2,tag:unindent,List3),
    delete(List3,tag:write_unindent,Clean_List).

%Add phrase linkers to unite program clauses
add_clause_phrase_linkers([],_,[]).
add_clause_phrase_linkers([Clause|Rest],LinkersUsed,[[Linker,tag:comma|Clause]|ProcessedRest]):-
    choose_linker(LinkersUsed,Linker,NewLinkersUsed),!,
    add_clause_phrase_linkers(Rest,NewLinkersUsed,ProcessedRest).  

%Processes body of imperative-style description into natural language
process_body([Head|Rest],[Processed|ProcessedRest]):-
    process_snippet(Head,Processed),
    process_body(Rest,ProcessedRest).
process_body([],[]).

%Processes individial code snippets of imperative-style description into natural language
process_snippet([for(Variable:Predicate)|Rest],[Desc,tag:loop_conjunction,tag:subject|DescRest]):-
    atom(Variable),
    atom_concat('for each ',Variable,Atom1),
    atom_concat(Atom1,' that satisfies ',Atom2),
    term_string(Predicate,PredString),
    atom_concat(Atom2,PredString,Desc),
    process_snippet(Rest,DescRest).
process_snippet([for(Variables:Predicate)|Rest],[Desc,tag:loop_conjunction,tag:subject|DescRest]):-
    Variables=..[_|Vars],
    pretty_enumeration(Vars,PrettyVars),
    atom_concat('for each ',PrettyVars,Atom1),
    atom_concat(Atom1,' that satisfy ',Atom2),
    term_string(Predicate,PredString),
    atom_concat(Atom2,PredString,Desc),
    process_snippet(Rest,DescRest).
process_snippet([for(Iteration)|Rest],[Desc,tag:subject|DescRest]):-
    atomic_list_concat(List,' ',Iteration),
    delete(List,in,[Head,Tail]),
    atom_concat('for each ',Head,Atom1),
    atom_concat(Atom1,' that belongs to ',Atom2),
    atom_concat(Atom2,Tail,Atom3),
    atom_concat(Atom3,',',Desc),
    process_snippet(Rest,DescRest).
process_snippet([do|Rest],[tag:subject|DescRest]):-
    process_snippet(Rest,DescRest).
process_snippet([while(not(Pred))|Rest],[tag:end_of_phrase,'if',Desc,tag:end_of_phrase|DescRest]):-
    Pred=..[Name|Args],
    atomic_list_concat(Args,', ', AtomArgs),
    atom_concat(Name,' ',Atom1),
    atom_concat(Atom1,AtomArgs,Atom2),
    atom_concat(Atom2,' exists, it stops, otherwise it repeats the same process',Desc),
    process_snippet(Rest,DescRest).
process_snippet([while(not_successful(Pred))|Rest],[tag:end_of_phrase,'if',tag:subject,Desc,tag:end_of_phrase|DescRest]):-
    procedure_description(Pred,infinitive,ProdDesc),
    atom_concat('manages to successfully ',ProdDesc,Atom1),
    atom_concat(Atom1,', it stops, otherwise, it repeats the same process',Desc),
    process_snippet(Rest,DescRest).
process_snippet([if(Condition)|Rest],[Desc,tag:subject|DescRest]):-
    condition_description(Condition,ProcessedCon),
    atom_concat('if ',ProcessedCon,Atom1),
    atom_concat(Atom1,' then',Desc),
    process_snippet(Rest,DescRest).
process_snippet([tag:X|Rest],[tag:X|DescRest]):-
    process_snippet(Rest,DescRest).
process_snippet([else|Rest],[tag:comma,'otherwise',tag:subject|DescRest]):-
    process_snippet(Rest,DescRest).
process_snippet([Pred|Rest],[Desc,tag:conjunction,'breaks line',tag:conjunction|DescRest]):-
    compound(Pred),
    Pred=..[Name|_],
    Name=print_line,
    procedure_description(Pred,present,Desc),!,
    process_snippet(Rest,DescRest).
process_snippet([Pred|Rest],[FullDesc,tag:conjunction|DescRest]):-
    compound(Pred),
    Pred=..[Name|Args],
    Name=read,
    procedure_description(Pred,present,Desc),!,
    get_context(Args,Rest,Context),!,
    pretty_enumeration(Args,PrettyArgs),
    atomic_list_concat([Desc,Context,PrettyArgs,'from user input'],' ',FullDesc),
    process_snippet(Rest,DescRest).
process_snippet([Pred|Rest],[Desc,tag:conjunction|DescRest]):-
    compound(Pred),
    procedure_description(Pred,present,Desc),!,
    process_snippet(Rest,DescRest).
process_snippet([Head|Rest],[Head|DescRest]):-
    process_snippet(Rest,DescRest).
process_snippet([],[]).

%Get context from nouns in following predicates in list
get_context(Args,[tag:_|Rest],Context):-
    get_context(Args,Rest,Context).
get_context(Args,[if(_)|Rest],Context):-
    get_context(Args,Rest,Context).
get_context(Args,[Head|Rest],Context):-
    atom(Head),
    get_context(Args,Rest,Context).
get_context(Args,[for(_:Predicate)|_],Context):-
    get_context_in_predicate(Args,Predicate,Context).
get_context(Args,[while(not(Predicate))|_],Context):-
    get_context_in_predicate(Args,Predicate,Context).
get_context(Args,[while(not_successful(Predicate))|_],Context):-
    get_context_in_predicate(Args,Predicate,Context).
get_context(Args,[Predicate|_],Context):-
    get_context_in_predicate(Args,Predicate,Context).
get_context(Args,[_|Rest],Context):-
    get_context(Args,Rest,Context).
get_context(_,[],'').

%Get context from predicate
get_context_in_predicate(Args,Pred,Context):-
    Pred=..[Name|PredArgs],
    intersection(Args,PredArgs,Int),
    Int\=[],
    atom_chars(Name, CharList),
    separate_words(CharList,Words),!,
    get_first_noun(Words,Context).

%Process header of natural language description
process_header(Head,[Header]):-
    Head=..[Name|Args],
    atom_concat(Name,' receives ',Atom),
    pretty_enumeration(Args,PrettyArgs),
    atom_concat(Atom,PrettyArgs,Header).

%Process loop conjunction tags in list of phrases
process_loop_conjunctions([List|Rest],[Processed|ProcessedRest]):-
    process_loop_conjunctions(List,[],Processed),
    process_loop_conjunctions(Rest,ProcessedRest).
process_loop_conjunctions([],[]).

%Process loop conjunction tags in phrase to unite multiple loops coherently
%Desc,tag:loop_conjunction,tag:subject
process_loop_conjunctions([Desc,tag:loop_conjunction,Tag|Rest],List,Processed):-
    append(List,[Desc,Tag],NewList),
    process_loop_conjunctions(Rest,NewList,Processed).
process_loop_conjunctions([Head|Rest],[],[Head|Processed]):-
    process_loop_conjunctions(Rest,[],Processed).
process_loop_conjunctions([Head|Rest],List,Processed):-
    pretty_loop_conjunctions(List,Pretty),
    append(Pretty,[Head],NewList),
    process_loop_conjunctions(Rest,[],ProcessedRest),
    append(NewList,ProcessedRest,Processed).
process_loop_conjunctions([],List,Pretty):-
    pretty_loop_conjunctions(List,Pretty).
    
%Pretty loop conjunction descriptions
pretty_loop_conjunctions([Desc,Tag],[NewDesc,Tag]):-
    atom_concat(Desc,',',NewDesc).
pretty_loop_conjunctions([Desc1,Tag1,Desc2,Tag2],[NewDesc1,Tag1,NewDesc2,Tag2]):-
    atom_concat(Desc1,' and',NewDesc1),
    atom_concat(Desc2,',',NewDesc2).
pretty_loop_conjunctions([Desc,Tag|Rest],[NewDesc,Tag|PrettyRest]):-
    atom_concat(Desc,',',NewDesc),
    pretty_loop_conjunctions(Rest,PrettyRest).
pretty_loop_conjunctions([],[]).

%Filter redudant subject tags
filter_subjects([List|Rest],[Filtered|FilteredRest]):-
    remove_extra_subjects(List,Filtered),
    filter_subjects(Rest,FilteredRest).
filter_subjects([],[]).

%Remove extra subject tags in phrase
remove_extra_subjects([tag:subject,tag:subject|Rest],Filtered):-
    remove_extra_subjects([tag:subject|Rest],Filtered).
remove_extra_subjects([tag:subject,X,tag:subject|Rest],[X|Filtered]):-
    remove_extra_subjects([tag:subject|Rest],Filtered).
remove_extra_subjects([Head|Rest],[Head|Filtered]):-
    remove_extra_subjects(Rest,Filtered).
remove_extra_subjects([],[]).

%Process end_of_phrase tags and capitalize appropriately all lists in list
process_punctuation([List],Capitalized):-
    split_phrases(List,Phrases),!,
    add_final_phrase_linkers(Phrases,[],LinkedPhrases),!,
    capitalize_phrases(LinkedPhrases,Capitalized).
process_punctuation([List|Rest],Processed):-
    split_phrases(List,Phrases),!,
    add_phrase_linkers(Phrases,[],LinkedPhrases),!,
    capitalize_phrases(LinkedPhrases,Capitalized),
    process_punctuation(Rest,PhrasesRest),
    append(Capitalized,PhrasesRest,Processed).
process_punctuation([],[]).

%Process end_of_phrase tags and capitalize appropriately
split_phrases(List,Phrases):-
    split_phrases(List,[],Phrases).
split_phrases([tag:end_of_phrase|Rest],List,[List|SplitRest]):-
    split_phrases(Rest,[],SplitRest).
split_phrases([Head,tag:end_of_phrase],List,[NewList]):-
    append(List,[Head],NewList).
split_phrases([Head|Rest],List,Split):-
    append(List,[Head],NewList),
    split_phrases(Rest,NewList,Split).
split_phrases([],List,[List]).

%Add linking adverbs to phrases
add_phrase_linkers([],_,[]).
add_phrase_linkers([Phrase|Rest],LinkersUsed,[[Linker,tag:comma,'it'|Phrase]|ProcessedRest]):-
    \+member(tag:subject,Phrase),
    \+member('if',Phrase),
    choose_linker(LinkersUsed,Linker,NewLinkersUsed),!,
    add_phrase_linkers(Rest,NewLinkersUsed,ProcessedRest).
add_phrase_linkers([Head|Rest],LinkersUsed,[Head|ProcessedRest]):-
    add_phrase_linkers(Rest,LinkersUsed,ProcessedRest).

%Add linking adverbs to phrases of the program's last clause
add_final_phrase_linkers([],_,[]).
add_final_phrase_linkers([Phrase],_,[['finally, it'|Phrase]]):-
    \+member(tag:subject,Phrase),
    \+member('if',Phrase).
add_final_phrase_linkers([Phrase|Rest],LinkersUsed,[[Linker,tag:comma,'it'|Phrase]|ProcessedRest]):-
    \+member(tag:subject,Phrase),
    \+member('if',Phrase),
    choose_linker(LinkersUsed,Linker,NewLinkersUsed),!,
    add_final_phrase_linkers(Rest,NewLinkersUsed,ProcessedRest).
add_final_phrase_linkers([Head|Rest],LinkersUsed,[Head|ProcessedRest]):-
    add_final_phrase_linkers(Rest,LinkersUsed,ProcessedRest). 

%Choose linking adverb based on previously used adverbs
choose_linker(Used,Linker,[Linker|Used]):-
    phrase_linker(Linker),
    \+member(Linker,Used).
choose_linker(_,Linker,[Linker]):-
    phrase_linker(Linker).

%Upper case the first character of phrases except for subjects
capitalize_phrases([[tag:subject|Rest]|RestList],[[tag:subject|Rest]|CapitalizedRest]):-
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([[Head|Rest]|RestList],[[Capitalized|Rest]|CapitalizedRest]):-
    capitalize(Head,Capitalized),
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([[]|RestList],CapitalizedRest):-
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([],[]).

%Process conjunction tags in list of phrases
process_conjunctions([List|Rest],[Processed|ProcessedRest]):-
    process_conjunctions(List,[],Processed),
    process_conjunctions(Rest,ProcessedRest).
process_conjunctions([],[]).

%Process conjunction tags in phrase to unite text excerpts coherently
process_conjunctions([X,tag:conjunction,tag:comma|Rest],List,[Pretty,NewX|Processed]):-
    atom_concat(X,',',NewX),
    pretty_enumeration(List,Pretty),
    process_conjunctions(Rest,[],Processed).
process_conjunctions([X,tag:conjunction,tag:comma|Rest],[],[NewX|Processed]):-
    atom_concat(X,',',NewX),
    process_conjunctions(Rest,[],Processed).
process_conjunctions([X,tag:conjunction|Rest],List,Processed):-
    append(List,[X],NewList),
    process_conjunctions(Rest,NewList,Processed).
process_conjunctions([X,tag:comma|Rest],[],[NewHead|Processed]):-
    atom_concat(X,',',NewHead),
    process_conjunctions(Rest,[],Processed).
process_conjunctions([X,tag:comma|Rest],List,[Pretty,NewHead|Processed]):-
    atom_concat(X,',',NewHead),
    pretty_enumeration(List,Pretty),
    process_conjunctions(Rest,[],Processed).
process_conjunctions([Head|Rest],[],[Head|Processed]):-
    process_conjunctions(Rest,[],Processed).
process_conjunctions([Head|Rest],List,[Pretty,Head|Processed]):-
    pretty_enumeration(List,Pretty),
    process_conjunctions(Rest,[],Processed).
process_conjunctions([tag:conjunction],List,[Pretty]):-
    pretty_enumeration(List,Pretty).
process_conjunctions([tag:conjunction],[],[]).
process_conjunctions([],[],[]).
process_conjunctions([],List,[Pretty]):-
    pretty_enumeration(List,Pretty).

%Process subject tags in lists
process_subjects(Name,[List|Rest],[Processed|ProcessedRest]):-
    process_subject(Name,List,Processed,0),
    process_subjects(Name,Rest,ProcessedRest).
process_subjects(_,[],[]).
process_subject(Name,[tag:subject|Rest],[Name|ProcessedRest],N):-
    0 is mod(N,2),
    Up is N+1,
    process_subject(Name,Rest,ProcessedRest,Up).
process_subject(Name,[tag:subject|Rest],[it|ProcessedRest],N):-
    Up is N+1,
    process_subject(Name,Rest,ProcessedRest,Up).
process_subject(Name,[Head|Rest],[Head|ProcessedRest],N):-
    process_subject(Name,Rest,ProcessedRest,N).
process_subject(_,[],[],_).

%Process lists of atoms into phrases
process_phrases([List|Rest],Text):-
    atomic_list_concat(List, ' ', Atom1),
    atom_concat(Atom1,'. ',Phrase),
    process_phrases(Rest,Phrases),
    atom_concat(Phrase,Phrases,Text).
process_phrases([],'').
    

    