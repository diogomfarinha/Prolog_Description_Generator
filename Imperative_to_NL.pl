:- ensure_loaded('Predicate_Library.pl').
:- ensure_loaded('Prolog_to_Imperative.pl').
:- ensure_loaded('Natural_Language_Generation.pl').

%Converts Prolog predicates into imperative-style descriptions and then
%into nautral language descriptions
%Receives predicates in format Name/Arity
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_body(Body,Text),!,
    nl,nl,
    writeq(Head),nl,
    writeq(Body),nl,
    write(Text).
%prolog_to_nl(Name/Arity):-
 %   prolog_to_imperative_info(Name/Arity,Head,Body),
  %  process_header(Head,Header),
   % process_body(Body,Text),
    %nl,
    %write(Header),
    %write(Text).

process_body([Head|Rest],[Processed|ProcessedRest]):-
    process_clause(Head,Processed),
    process_clause(Rest,ProcessedRest).
process_clause([for(Variables:Predicate)|Rest],[Desc|Text]):-
    Variables=..[_|Vars],
    pretty_variables(Vars,PrettyVars),
    atom_concat('for each ',PrettyVars,Atom1),
    atom_concat(Atom1,' that satisfy ',Atom2),
    term_string(Predicate,PredString),
    atom_concat(Atom2,PredString,Desc),
    process_clause(Rest,Text).
process_clause([tag:X|Rest],[tag:X|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([Pred|Rest],[Desc|DescRest]):-
    compound(Pred),
    procedure_description(Pred,present,Desc),!,
    process_clause(Rest,DescRest).
process_clause([Head|Rest],[Head|DescRest]):-
    process_clause(Rest,DescRest).
process_clause([],[]).

    