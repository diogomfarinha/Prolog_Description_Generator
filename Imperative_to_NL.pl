:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Natural_Language_Generation.pl').

%Converts Prolog predicates into imperative-style descriptions and then
%into natural language descriptions.
%Receives predicates in format Name/Arity
prolog_to_nl(Name/0):-
    prolog_to_imperative_info(Name/0,_,Body),!,
    delete_tags_from_list(Body,Clean_Body),
    process_body(Clean_Body,Processed),!,
    filter_subjects(Processed,Filtered),!,
    process_punctuation(Filtered,Processed2),!,
    process_subjects(Name,Processed2,Processed3),!,
    process_phrases(Processed3,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_header(Head,Header),
    delete_tags_from_list(Body,Clean_Body),
    process_body(Clean_Body,Processed),!,
    filter_subjects(Processed,Filtered),!,
    process_punctuation(Filtered,Processed2),!,
    process_subjects(Name,Processed2,Processed3),!,
    process_phrases([Header|Processed3],Text),!,
    nl,nl,
    write(Text).

%Developer version
prolog_to_nl_dev(Name/0):-
    prolog_to_imperative_info(Name/0,_,Body),!,
    nl,nl,
    write(body:Body),nl,
    delete_tags_from_list(Body,Clean_Body),
    writeq(clean:Clean_Body),nl,
    process_body(Clean_Body,Processed),!,
    writeq(body:Processed),nl,
    filter_subjects(Processed,Filtered),!,
    writeq(filtered:Filtered),nl,
    process_punctuation(Filtered,Processed2),!,
    writeq(punctuation:Processed2),nl,
    process_subjects(Name,Processed2,Processed3),!,
    writeq(subjects:Processed3),nl,
    process_phrases(Processed3,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl_dev(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    nl,nl,
    write(body:Body),nl,
    process_header(Head,Header),
    writeq(header:Header),nl,
    delete_tags_from_list(Body,Clean_Body),
    writeq(clean:Clean_Body),nl,
    process_body(Clean_Body,Processed),!,
    writeq(body:Processed),nl,
    filter_subjects(Processed,Filtered),!,
    writeq(filtered:Filtered),nl,
    process_punctuation(Filtered,Processed2),!,
    writeq(punctuation:Processed2),nl,
    process_subjects(Name,Processed2,Processed3),!,
    writeq(subjects:Processed3),nl,
    process_phrases([Header|Processed3],Text),!,
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

%Processes body of imperative-style description into natural language
process_body([Head|Rest],[Processed|ProcessedRest]):-
    process_clause(Head,Processed),
    process_clause(Rest,ProcessedRest).

%Processes individial clauses of imperative-style description into natural language
process_clause([for(Variables:Predicate)|Rest],[Desc,tag:subject|DescRest]):-
    Variables=..[_|Vars],
    pretty_variables(Vars,PrettyVars),
    atom_concat('for each ',PrettyVars,Atom1),
    atom_concat(Atom1,' that satisfy ',Atom2),
    term_string(Predicate,PredString),
    atom_concat(Atom2,PredString,Atom3),
    atom_concat(Atom3,',',Desc),
    process_clause(Rest,DescRest).
process_clause([for(Iteration)|Rest],[Desc,tag:subject|DescRest]):-
    atomic_list_concat(List,' ',Iteration),
    delete(List,in,[Head,Tail]),
    atom_concat('for each ',Head,Atom1),
    atom_concat(Atom1,' that belongs to ',Atom2),
    atom_concat(Atom2,Tail,Atom3),
    atom_concat(Atom3,',',Desc),
    process_clause(Rest,DescRest).
process_clause([do|Rest],[tag:subject|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([while(not(Pred))|Rest],[tag:end_of_phrase,'if',Desc|DescRest]):-
    Pred=..[Name|Args],
    atomic_list_concat(Args,', ', AtomArgs),
    atom_concat(Name,' ',Atom1),
    atom_concat(Atom1,AtomArgs,Atom2),
    atom_concat(Atom2,' exists, it stops, otherwise it repeats the same process',Desc),
    process_clause(Rest,DescRest).
process_clause([while(not_successful(Pred))|Rest],[tag:end_of_phrase,'if',tag:subject,Desc,tag:end_of_phrase|DescRest]):-
    procedure_description(Pred,infinitive,ProdDesc),
    atom_concat('manages to successfully ',ProdDesc,Atom1),
    atom_concat(Atom1,', it stops, otherwise, it repeats the same process',Desc),
    process_clause(Rest,DescRest).
process_clause([if(Condition)|Rest],[Desc,tag:subject|DescRest]):-
    condition_description(Condition,ProcessedCon),
    atom_concat('if ',ProcessedCon,Atom1),
    atom_concat(Atom1,' then',Desc),
    process_clause(Rest,DescRest).
process_clause([tag:X|Rest],[tag:X|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([else|Rest],[', otherwise,',tag:subject|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([Pred|Rest],[Desc|DescRest]):-
    compound(Pred),
    procedure_description(Pred,present,Desc),!,
    process_clause(Rest,DescRest).
process_clause([Head|Rest],[Head|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([],[]).

%Process header of natural language description
process_header(Head,[Header]):-
    Head=..[Name|Args],
    atom_concat(Name,' receives ',Atom),
    pretty_variables(Args,PrettyArgs),
    atom_concat(Atom,PrettyArgs,Header).

%Filter redudant subject tags
filter_subjects([List|Rest],[Filtered|FilteredRest]):-
    filter_subject(List,Filtered),
    filter_subjects(Rest,FilteredRest).
filter_subjects([],[]).
filter_subject([tag:subject,tag:subject|Rest],Filtered):-
    filter_subject([tag:subject|Rest],Filtered).
filter_subject([tag:subject,X,tag:subject|Rest],[X|Filtered]):-
    filter_subject([tag:subject|Rest],Filtered).
filter_subject([Head|Rest],[Head|Filtered]):-
    filter_subject(Rest,Filtered).
filter_subject([],[]).

%Process end_of_phrase tags and capitalize appropriately all lists in list
process_punctuation([List|Rest],Processed):-
    split_phrases(List,Phrases),!,
    capitalize_phrases(Phrases,Capitalized),
    process_punctuation(Rest,PhrasesRest),
    append(Capitalized,PhrasesRest,Processed).
process_punctuation([],[]).

%Process end_of_phrase tags and capitalize appropriately
split_phrases(List,Phrases):-
    split_phrases(List,[],Phrases).
split_phrases([tag:end_of_phrase|Rest],List,[List|SplitRest]):-
    split_phrases(Rest,[],SplitRest).
split_phrases([Head|Rest],List,Split):-
    append(List,[Head],NewList),
    split_phrases(Rest,NewList,Split).
split_phrases([],List,[List]).

%Upper case the first character of phrases except for subjects
capitalize_phrases([[tag:subject|Rest]|RestList],[[tag:subject|Rest]|CapitalizedRest]):-
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([[Head|Rest]|RestList],[[Capitalized|Rest]|CapitalizedRest]):-
    capitalize(Head,Capitalized),
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([[]|RestList],CapitalizedRest):-
    capitalize_phrases(RestList,CapitalizedRest).
capitalize_phrases([],[]).

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
    

    