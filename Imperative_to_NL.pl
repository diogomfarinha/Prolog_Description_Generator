:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Natural_Language_Generation.pl').

%Converts Prolog predicates into imperative-style descriptions and then
%into natural language descriptions.
%Receives predicates in format Name/Arity
prolog_to_nl(Name/0):-
    prolog_to_imperative_info(Name/0,_,Body),
    nl,nl,
    process_body(Body,Text),!,
    write(Text).
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_header(Head,Header),
    process_body(Body,Text),!,
    nl,nl,
    write(Header),nl,
    write(Text).

%Processes body of imperative-style description into natural language
process_body([Head|Rest],[Processed|ProcessedRest]):-
    process_clause(Head,Processed),
    process_clause(Rest,ProcessedRest).

%Processes individial clauses of imperative-style description into natural language
process_clause([for(Variables:Predicate)|Rest],[Desc|DescRest]):-
    Variables=..[_|Vars],
    pretty_variables(Vars,PrettyVars),
    atom_concat('for each ',PrettyVars,Atom1),
    atom_concat(Atom1,' that satisfy ',Atom2),
    term_string(Predicate,PredString),
    atom_concat(Atom2,PredString,Desc),
    process_clause(Rest,DescRest).
process_clause([for(Iteration)|Rest],[Desc|DescRest]):-
    atomic_list_concat(List,' ',Iteration),
    delete(List,in,[Head,Tail]),
    atom_concat('for each element ',Head,Atom1),
    atom_concat(Atom1,' that belongs to ',Atom2),
    atom_concat(Atom2,Tail,Desc),
    process_clause(Rest,DescRest).
process_clause([tag:X|Rest],[tag:X|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([if(Condition)|Rest],[Desc|DescRest]):-
    condition_description(Condition,ProcessedCon),
    atom_concat('if ',ProcessedCon,Atom1),
    atom_concat(Atom1,' then',Desc),
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
    Head=..[Name|[Arg]],
    atom_concat(Name,' receives 1 argument: ',Atom1),
    atom_concat(Atom1,Arg,Header).
process_header(Head,[Header]):-
    Head=..[Name|Args],
    length(Args,Length),
    atom_concat(Name,' receives ',Atom1),
    atom_concat(Atom1,Length,Atom2),
    atom_concat(Atom2,' argument: ',Atom3),
    pretty_variables(Args,PrettyArgs),
    atom_concat(Atom3,PrettyArgs,Header).



    