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
    process_subjects(Name,Processed,Processed2),!,
    process_phrases(Processed2,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_header(Head,Header),
    delete_tags_from_list(Body,Clean_Body),
    process_body(Clean_Body,Processed),!,
    process_subjects(Name,Processed,Processed2),!,
    process_phrases([Header|Processed2],Text),!,
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
process_clause([while(not(Pred))|Rest],['. If',Desc|DescRest]):-
    Pred=..[Name|Args],
    atomic_list_concat(Args,', ', AtomArgs),
    atom_concat(Name,' ',Atom1),
    atom_concat(Atom1,AtomArgs,Atom2),
    atom_concat(Atom2,' exists, it stops. Otherwise it repeats the same process',Desc),
    process_clause(Rest,DescRest).
process_clause([while(not_successful(Pred))|Rest],['. If',tag:subject,Desc|DescRest]):-
    procedure_description(Pred,infinitive,ProdDesc),
    atom_concat('manages to successfully ',ProdDesc,Atom1),
    atom_concat(Atom1,', it stops. Otherwise it repeats the same process',Desc),
    process_clause(Rest,DescRest).
process_clause([tag:X|Rest],[tag:X|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([if(Condition)|Rest],[Desc|DescRest]):-
    condition_description(Condition,ProcessedCon),
    atom_concat('if ',ProcessedCon,Atom1),
    atom_concat(Atom1,' then',Desc),
    process_clause(Rest,DescRest).
process_clause([else|Rest],[otherwise|DescRest]):-
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

%Process subject tags in lists
process_subjects(Name,[List|Rest],[Processed|ProcessedRest]):-
    process_subject(Name,List,Processed,0),
    process_subjects(Name,Rest,ProcessedRest).
process_subjects(_,[],[]).
process_subject(Name,[tag:subject|Rest],[NameAtom|ProcessedRest],N):-
    0 is mod(N,2),
    Up is N+1,
    atom_concat(Name,',',NameAtom),
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
    atom_concat(Atom1,'. ',Atom2),
    capitalize(Atom2,Phrase),
    process_phrases(Rest,Phrases),
    atom_concat(Phrase,Phrases,Text).
process_phrases([],'').
    

    