:- ensure_loaded('Predicate_Library.pl').

%Converts and prints Prolog programs into imperative-style descriptions. Pred is in Name/Arity format
%Java style prints with brackets and Python style prints with colon
%At this time it can only describe Prolog progams from 'Test_Programs.pl', in the future,
%the program file should be added as an argument.
prolog_to_imperative(Pred):-
    prolog_to_imperative(Pred,java).
    %prolog_to_imperative_dev(Pred).
prolog_to_imperative(Name/Arity,Style):-
    get_rules_with_variables('Test_Programs.pl',Name/Arity,[[Head|Rule]|Rules]),
    get_patterns([[Head|Rule]|Rules],Patterns),
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    process_loop_coherence(Filtered_Patterns,Coherent_Patterns),
    patterns_to_text(Coherent_Patterns,Text_List),
    process_ifs_in_list(Text_List,Formatted_Text),!,
    process_writes_in_list(Head,Formatted_Text,Pretty_Text),!,
    process_predicate_head(Head,ProcessedHead),
    nl,
    get_predicate(Name/Arity,Pred),
    listing(Pred),
    print_formatted_description(Style,ProcessedHead,Pretty_Text),!.
%Prolog to imperative/3 has the final description as one of its arguments
prolog_to_imperative(Name/Arity,ProcessedHead,Pretty_Text):-
    get_rules_with_variables('Test_Programs.pl',Name/Arity,[[Head|Rule]|Rules]),
    get_patterns([[Head|Rule]|Rules],Patterns),
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    process_loop_coherence(Filtered_Patterns,Coherent_Patterns),
    patterns_to_text(Coherent_Patterns,Text_List),
    process_ifs_in_list(Text_List,Formatted_Text),!,
    process_writes_in_list(Head,Formatted_Text,Pretty_Text),!,
    process_predicate_head(Head,ProcessedHead),
    nl,
    get_predicate(Name/Arity,Pred),
    listing(Pred),
    print_formatted_description(java,ProcessedHead,Pretty_Text),!.
%Developer version
prolog_to_imperative_dev(Name/Arity):-
    get_rules_with_variables('Test_Programs.pl',Name/Arity,[[Head|Rule]|Rules]),
    write(rules:[[Head|Rule]|Rules]),nl,
    get_patterns([[Head|Rule]|Rules],Patterns),
    write(patterns:Patterns),nl,
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    write(filtered:Filtered_Patterns),nl,
    process_loop_coherence(Filtered_Patterns,Coherent_Patterns),
    write(coherent:Coherent_Patterns),nl,
    patterns_to_text(Coherent_Patterns,Text_List),
    write(text:Text_List),nl,
    process_ifs_in_list(Text_List,Formatted_Text),!,
    write(formatted:Formatted_Text),nl,
    process_writes_in_list(Head,Formatted_Text,Pretty_Text),!,
    write(pretty:Pretty_Text),nl,
    process_predicate_head(Head,ProcessedHead),
    write(head:ProcessedHead),nl,
    nl,
    get_predicate(Name/Arity,Pred),
    listing(Pred),
    print_formatted_description(java,ProcessedHead,Pretty_Text),!.

%Get Prolog programming patterns in list of rules
get_patterns([[Head|Rule]|Rest],Patterns):-
    pattern(Head,Rule,Pattern_from_Rule),!,
    get_patterns(Rest,Patterns_from_Rest),
    append([Pattern_from_Rule],Patterns_from_Rest,Patterns).
get_patterns([],[]).

%Identifies and tags Prolog programming patterns
%fail pattern
pattern(Predicate,[Pred|Body],[tag:for_loop(Args,Pred)|Rest]):-
    predicate_has_more_solutions(Pred),
    list_ends_with(Body,fail),
    Pred=..[_|Args],
    pattern(Predicate,Body,Rest).
%repeat pattern
pattern(_,[repeat|Body],[tag:do_while|Rest]):-
    predicates_can_fail(Body,Rest).
%recursion pattern- iteration through entire list
pattern(Predicate,[Pred|Body],[tag:iter_loop(Head)|Rest]):-
    functor(Predicate,Name,Arity),
    functor(Pred,Name,Arity),
    Predicate=..[Name|Args],
    nth0(Index,Args,[Head|VarRest]),
    Pred=..[Name|Args2],
    nth0(Index,Args2,VarRest),
    length(Args,Length),
    list_of_empty_elements(Length,Index,[],Args3),
    New_Predicate=..[Name|Args3],
    catch(New_Predicate,_,fail),%Prevent errors from calling non-existent predicates
    assert(recursion_argument(Index)),
    pattern(Predicate,Body,Rest).
%if pattern- incomplete, still needs work, only detects math operations
pattern(Predicate,[Pred|Body],[tag:if_clause(Pred)|Rest]):-
    is_math(Pred),
    pattern(Predicate,Body,Rest).
pattern(Predicate,[\+Pred|Body],[tag:if_not(Pred)|Rest]):-
    pattern(Predicate,Body,Rest).
%no patttern
pattern(_,[fail|_],[]).
%pretty predicates with 0 arguments
pattern(Predicate,[Pred|Body],[Pretty|Rest]):-
    Pred=..[Name|[]],
    Name\=nl,
    Pretty=..[Name|['']],
    pattern(Predicate,Body,Rest).
pattern(Predicate,[Pred|Body],[Pred|Rest]):-
    pattern(Predicate,Body,Rest).
pattern(_,[],[]).

%Identifies and tags which predicates from the list can fail
predicates_can_fail([Pred|Body],[tag:canfail(Pred)|Rest]):-
    predicate_can_fail(Pred),
    predicates_can_fail(Body,Rest).
predicates_can_fail([Pred|Body],[Pred|Rest]):-
    predicates_can_fail(Body,Rest).
predicates_can_fail([],[]).

% List is a list with length and all of its elements as variables except
% for element on index (counting starts at 0)
list_of_empty_elements(Length,Index,Element,List):-
  list_of_empty_elements(Length,Index,Element,0,List).
list_of_empty_elements(Length,Index,Element,Count,[_|Rest]):-
    Count<Length,
    Count\=Index,
    Up is Count+1,
    list_of_empty_elements(Length,Index,Element,Up,Rest).
list_of_empty_elements(Length,Index,Element,Index,[Element|Rest]):-
    Up is Index+1,
    list_of_empty_elements(Length,Index,Element,Up,Rest).
list_of_empty_elements(Length,_,_,Length,[]).

%Joins Patterns with iteration tags on the same list so that they all belong to the same 
%iteration cycle (flattens and unindents iteration clauses). 
%Moves iteration tag to Head of list and deletes extra iteration tags.
eliminate_iteration_redundancy(Patterns,Patterns):-
    \+matrix_member(tag:iter_loop(_),Patterns).
eliminate_iteration_redundancy(Patterns,ProcessedPatterns):-
    matrix_member(tag:iter_loop(X),Patterns),
    eliminate_iteration_redundancy(Patterns,PatternsFiltered,IterationPatterns),
    flatten(IterationPatterns,FlattenedPatterns),
    append([[tag:iter_loop(X)|FlattenedPatterns]],PatternsFiltered,ProcessedPatterns).
eliminate_iteration_redundancy([Pattern|Rest],[Pattern|ProcessedRest],IterationClauses):-
    \+member(tag:iter_loop(_),Pattern),
    eliminate_iteration_redundancy(Rest,ProcessedRest,IterationClauses).
eliminate_iteration_redundancy([Pattern|Rest],Patterns,[IterationClause|IterationClauses]):-
    member(tag:iter_loop(_),Pattern),
    delete(Pattern, tag:iter_loop(_), Pattern_without),
    append(Pattern_without,[tag:unindent],IterationClause),
    eliminate_iteration_redundancy(Rest,Patterns,IterationClauses).
eliminate_iteration_redundancy([],[],[]).

%Processes head of predicate for imperative description
process_predicate_head(Pred,Head):-
    Pred=..[Name|[]],
    Head=..[Name|['']].
process_predicate_head(Pred,Pred):-
    \+catch(recursion_argument(_),_,fail).
process_predicate_head(Pred,NewPred):-
    retract(recursion_argument(Index)),
    retractall(recursion_argument(_)),
    Pred=..[Name|Args],
    switch_index(Index,Args,'List',NewArgs),
    NewPred=..[Name|NewArgs].

%Processes coherence in for loops in list
process_loop_coherence([List|Rest],[Coherent|CoherentRest]):-
    coherent_loops(List,Coherent),
    process_loop_coherence(Rest,CoherentRest).
process_loop_coherence([],[]).

%Assures loops are coherent and do not repeat variables 
%Ex: for(U:university(U)){for(S:student(U,S)) instead of for(U:university(U)){for((U,S):student(U,S))
coherent_loops(List,Coherent):-
    coherent_loops(List,Coherent,[]).
coherent_loops([tag:for_loop(List,Pred)|Rest],[tag:for_loop(Subtract,Pred)|CoherentRest],Variables):-
    intersection(List,Variables,Intersection),
    subtract(List,Intersection,Subtract),
    append(Intersection,Subtract,Conjunction),
    coherent_loops(Rest,CoherentRest,Conjunction).
coherent_loops([Head|Rest],[Head|CoherentRest],Variables):-
    coherent_loops(Rest,CoherentRest,Variables).
coherent_loops([],[],_).

%Processes list of processed patterns into list of lists with text and terms
patterns_to_text([Pattern|Rest],[Text|TextRest]):-
    pattern_to_text(Pattern,Text),!,
    patterns_to_text(Rest,TextRest),!.
patterns_to_text([],[]).

%Process processed patterns into text
pattern_to_text([tag:iter_loop(Elements)|Rest],[Text,tag:indent|TextRest]):-
    iter_loop_description(Elements,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:for_loop(Args,Pred)|Rest],[Text,tag:indent|TextRest]):-
    for_loop_description(Args,Pred,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:if_clause(Condition)|Rest],[Text,tag:indent|TextRest]):-
    if_clause_description(Condition,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:if_not(Condition)|Rest],[Text,tag:indent|TextRest]):-
    if_clause_description(not,Condition,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:do_while|Rest],Text):-
    count_member(tag:canfail(_),Rest,Count),!,write(Count),nl,
    do_while_description(Count,DoText),
    repeat_pattern_to_text(Rest,TextRest),
    append(DoText,TextRest,Text).
pattern_to_text([Predicate|Rest],[Predicate|TextRest]):-
    pattern_to_text(Rest,TextRest).
pattern_to_text([],[]).

%Describe a loop for iterating through a list
iter_loop_description(Element,for(Text)):-
    atom_concat(Element,' in List',Text).

%Describe a for loop with arguments and predicate
for_loop_description([X],Predicate,for(X:Predicate)).
for_loop_description(Arguments,Predicate,for(X:Predicate)):-
    delete(Arguments,'_',[X]).
for_loop_description(Arguments,Predicate,for(Term:Predicate)):-
    delete(Arguments,'_',CleanArgs),
    Term=..[''|CleanArgs].

%Describe an if clause with condition
if_clause_description(Condition,if(Condition)).
if_clause_description(not,Condition,if(Text)):-
    term_to_atom(Condition,AtomCon),
    atom_concat('not ', AtomCon, Text).

%Describe a while loop with Variables
do_while_description(0,[]).
do_while_description(Count,[do,tag:indent|Rest]):-
    CountDown is Count-1,
    do_while_description(CountDown,Rest).

%Process predicates in a repeat pattern to text
repeat_pattern_to_text([tag:canfail(Pred)|Rest],[tag:write_unindent,while(not(Pred))|TextRest]):-
    is_fact(Pred),
    repeat_pattern_to_text(Rest,TextRest).
repeat_pattern_to_text([tag:canfail(Pred)|Rest],[tag:write_unindent,while(not_successful(Pred))|TextRest]):-
    repeat_pattern_to_text(Rest,TextRest).
repeat_pattern_to_text([Pred|Rest],[Pred|TextRest]):-
    repeat_pattern_to_text(Rest,TextRest).
repeat_pattern_to_text([],[]).

%Process if clauses in all lists into if-else if applicable 
process_ifs_in_list([Pattern|Rest],[Processed|ProcessedRest]):-
    process_ifs(Pattern,Processed),
    process_ifs_in_list(Rest,ProcessedRest),!.
process_ifs_in_list([],[]).

%Transform if clauses in  list into if-else if applicable 
process_ifs(List,ProcessedList):-
    get_ifs(List,[if(Cond1),if(Cond2)]),
    opposite_conditions(Cond1,Cond2),
    select(if(Cond2), List, else, ProcessedList).
process_ifs(List,List).

%Get ifs from list
get_ifs([if(X)|Rest],[if(X)|GetRest]):-
    get_ifs(Rest,GetRest).
get_ifs([_|Rest],GetRest):-
    get_ifs(Rest,GetRest).
get_ifs([],[]).

%ArgsList is list of all arguments in processed pattern list
get_arguments_list([for(_:Pred)|Rest],ArgsList):-
    Pred=..[_|Args],
    get_arguments_list(Rest,ArgsRest),
    append(Args,ArgsRest,ArgsList).
get_arguments_list([while(not(Pred))|Rest],ArgsList):-
    Pred=..[_|Args],
    get_arguments_list(Rest,ArgsRest),
    append(Args,ArgsRest,ArgsList).
get_arguments_list([while(not_successful(Pred))|Rest],ArgsList):-
    Pred=..[_|Args],
    get_arguments_list(Rest,ArgsRest),
    append(Args,ArgsRest,ArgsList).
get_arguments_list([if(Pred)|Rest],ArgsList):-
    Pred=..[_|Args],
    get_arguments_list(Rest,ArgsRest),
    append(Args,ArgsRest,ArgsList).
get_arguments_list([for(Iteration)|Rest],ArgsList):-
    atomic_list_concat([Var|_],' ',Iteration),
    get_arguments_list(Rest,ArgsRest),
    append([Var],ArgsRest,ArgsList).
get_arguments_list([Head|Rest],ArgsList):-
    Head=..[Name|Args],
    Name\=write,
    Name\=nl,
    Name\=':',
    get_arguments_list(Rest,ArgsRest),
    append(Args,ArgsRest,ArgsList).
get_arguments_list([_|Rest],ArgsList):-
    get_arguments_list(Rest,ArgsList).
get_arguments_list([],[]).

%Process write/1 in all lists into prettier text
process_writes_in_list(Head,[Pattern|Rest],[Processed|ProcessedRest]):-
    get_arguments_list([Head|Pattern],Args),
    process_writes(Pattern,Args,Processed),
    process_writes_in_list(Head,Rest,ProcessedRest),!.
process_writes_in_list(_,[],[]).

%Process all write/1 in list into prettier text
process_writes([write(X)|Rest],Args,[Text|ProcessedRest]):-
    process_writes_in_a_row([write(X)|Rest],Args,[],Text),
    process_writes_skip([write(X)|Rest],Args,ProcessedRest).
process_writes([Pred|Rest],Args,[Pred|ProcessedRest]):-
    process_writes(Rest,Args,ProcessedRest).
process_writes([],_,[]).

%Formats writes in a row into a prettier format
process_writes_in_a_row([write(X)|Rest],Variables,Args,Pretty):-
    member(X,Variables),
    append(Args,[X],NewArgs),
    process_writes_in_a_row(Rest,Variables,NewArgs,Pretty).
process_writes_in_a_row([write(X)|Rest],Variables,Args,Pretty):-
    \+member(X,Variables),
    atom_concat('\"',X,Atom1),
    atom_concat(Atom1,'\"',Atom2),
    append(Args,[Atom2],NewArgs),
    process_writes_in_a_row(Rest,Variables,NewArgs,Pretty).
process_writes_in_a_row([write(X)|Rest],Variables,Args,Pretty):-
    append(Args,[X],NewArgs),
    process_writes_in_a_row(Rest,Variables,NewArgs,Pretty).
process_writes_in_a_row([nl|_],_,Args,Pretty):-
    Pretty=..[print_line|Args].
process_writes_in_a_row(_,_,Args,Pretty):-
    Pretty=..[print|Args].

%Skips writes and nl
process_writes_skip([write(_)|Rest],Args,ProcessedRest):-
    process_writes_skip(Rest,Args,ProcessedRest).
process_writes_skip([nl|Rest],Args,ProcessedRest):-
    process_writes(Rest,Args,ProcessedRest).
process_writes_skip([Pred|Rest],Args,[Pred|ProcessedRest]):-
    process_writes(Rest,Args,ProcessedRest).
process_writes_skip([],_,[]).

%Print imperative style description in formatted style
print_formatted_description(java,Head,Body):-
    write(Head),write('{'),nl,
    print_formatted_predicate(java,Body),
    write('}').
print_formatted_description(python,Head,Body):-
    write(Head),write(':'),nl,
    print_formatted_predicate(python,Body).

%Print predicate and list of formattted rules
print_formatted_predicate(Style,[Rule|Rest]):-
    print_formatted(Style,Rule,5),
    print_formatted_predicate(Style,Rest),!.
print_formatted_predicate(_,[]).
  
%Print formatted list according to the chosen style
print_formatted(java,List,Indentation):-
    print_java_style(List,Indentation,0),!.
print_formatted(python,List,Indentation):-
    print_python_style(List,Indentation),!.

%Writes text in a java-like style of formatting.
print_java_style([Head,tag:indent|Rest],Indentation,BracketCount):-
    tab(Indentation),
    write(Head),write('{'),
    nl,
    NewIndentation is Indentation+5,
    NewCount is BracketCount+1,
    print_java_style(Rest,NewIndentation,NewCount).
print_java_style([tag:indent|Rest],Indentation,BracketCount):-
    NewIndentation is Indentation+5,
    NewCount is BracketCount+1,
    print_java_style(Rest,NewIndentation,NewCount).
print_java_style([tag:write_unindent,Head|Rest],Indentation,BracketCount):-
    NewIndentation is Indentation-5,
    tab(NewIndentation),
    BracketCount>=1,
    write('} '),write(Head),nl,
    NewCount is BracketCount-1,
    print_java_style(Rest,NewIndentation,NewCount).
print_java_style([tag:unindent|Rest],Indentation,BracketCount):-
    NewIndentation is Indentation-5,
    tab(NewIndentation),
    BracketCount>=1,
    write('}'),nl,
    NewCount is BracketCount-1,
    print_java_style(Rest,NewIndentation,NewCount).
print_java_style([tag:unindent|Rest],Indentation,BracketCount):-
    NewIndentation is Indentation-5,
    print_java_style(Rest,NewIndentation,BracketCount).
print_java_style([Head|Rest],Indentation,Count):-
    tab(Indentation),
    write(Head),
    nl,
    print_java_style(Rest,Indentation,Count).
print_java_style([],Indentation,Count):-
    print_brackets(Indentation,Count).

%Closes brackets in java style formatting
print_brackets(_,0).
print_brackets(Indentation,Count):-
    NewIndentation is Indentation-5,
    tab(NewIndentation),
    write('}'),
    nl,
    NewCount is Count-1,
    print_brackets(NewIndentation,NewCount).

%Writes text in a python-like style of formatting
print_python_style([Head,tag:indent|Rest],Indentation):-
    tab(Indentation),
    write(Head),write(':'),
    nl,
    NewIndentation is Indentation+5,
    print_python_style(Rest,NewIndentation).
print_python_style([tag:indent|Rest],Indentation):-
    NewIndentation is Indentation+5,
    print_python_style(Rest,NewIndentation).
print_python_style([tag:write_unindent|Rest],Indentation):-
    NewIndentation is Indentation-5,
    print_python_style(Rest,NewIndentation).
print_python_style([tag:unindent|Rest],Indentation):-
    NewIndentation is Indentation-5,
    print_python_style(Rest,NewIndentation).
print_python_style([Head|Rest],Indentation):-
    tab(Indentation),
    write(Head),
    nl,
    print_python_style(Rest,Indentation).
print_python_style([],_).

