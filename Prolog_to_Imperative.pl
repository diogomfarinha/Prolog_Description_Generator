:- ensure_loaded('Predicate_Library.pl').

%Converts and prints Prolog programs into imperative-style descriptions. Pred is in Name/Arity format
prolog_to_imperative(Pred):-
    prolog_to_imperative_dev(Pred).
prolog_to_imperative_dev(Name/Arity):-
    get_predicate(Name/Arity,Pred),
    write(pred:Pred),nl,
    get_all_rules(Pred,Rules),
    write(rules:Rules),nl,
    get_patterns(Pred,Rules,Patterns),
    write(patterns:Patterns),nl,
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    write(filtered:Filtered_Patterns),nl,
    get_variables_dictionary(Filtered_Patterns,VarsDic),!,
    write(varsdic:VarsDic),nl,
    process_predicate_head(Pred,VarsDic,Head),
    write(head:Head),nl,
    process_patterns_list(Filtered_Patterns,VarsDic,Processed_Patterns),
    write(processed:Processed_Patterns),nl,
    patterns_to_text(Processed_Patterns,Text_List),
    write(text:Text_List),nl,
    process_writes_in_list(Text_List,Pretty_Text),!,
    write(pretty:Pretty_Text),nl,nl,
    write(Head),write('{'),nl,
    print_formatted_predicate(java,Pretty_Text),
    write('}'),!.
prolog_to_imperative(Name/Arity,java):-
    get_predicate(Name/Arity,Pred),
    get_all_rules(Pred,Rules),
    get_patterns(Pred,Rules,Patterns),
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    get_variables_dictionary(Filtered_Patterns,VarsDic),!,
    process_predicate_head(Pred,VarsDic,Head),
    process_patterns_list(Filtered_Patterns,VarsDic,Processed_Patterns),
    patterns_to_text(Processed_Patterns,Text_List),
    process_writes_in_list(Text_List,Pretty_Text),!,
    write(Head),write('{'),nl,
    print_formatted_predicate(java,Pretty_Text),
    write('}'),!.
prolog_to_imperative(Pred/Arity,python):-
    get_predicate(Name/Arity,Pred),
    get_all_rules(Pred,Rules),
    get_patterns(Pred,Rules,Patterns),
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    get_variables_dictionary(Filtered_Patterns,VarsDic),!,
    process_predicate_head(Pred,VarsDic,Head),
    process_patterns_list(Filtered_Patterns,VarsDic,Processed_Patterns),
    patterns_to_text(Processed_Patterns,Text_List),
    process_writes_in_list(Text_List,Pretty_Text),!,
    write(Head),write(':'),nl,
    print_formatted_predicate(python,Coherent_Text),!.

%Converts Prolog programs into imperative-style descriptions
prolog_to_imperative_info(Pred/Arity,ProcessedHeadArgs,Coherent_Text):-
    get_patterns(Pred/Arity,HeadArgs,Patterns),
    eliminate_iteration_redundancy(Patterns,Filtered_Patterns),
    process_patterns_list(HeadArgs,ProcessedHeadArgs,Filtered_Patterns,Processed_Patterns),
    patterns_to_text(Processed_Patterns,Text_List),
    process_writes_in_list(Text_List,Pretty_Text),
    process_iteration_coherence(Pretty_Text,Coherent_Text),!,
    process_predicate_head(Pred,ProcessedHeadArgs,Head),
    write(Head),write('{'),nl,
    print_formatted_predicate(java,Coherent_Text),
    write('}'),!.

%Get Prolog programming patterns in list of rules
get_patterns(Pred,[Rule|Rest],Patterns):-
    pattern(Pred,Rule,Pattern_from_Rule),!,
    get_patterns(Pred,Rest,Patterns_from_Rest),
    append([Pattern_from_Rule],Patterns_from_Rest,Patterns).
get_patterns(_,[],[]).

%Identifies and tags Prolog programming patterns
%fail pattern
pattern(Predicate,[Pred|Body],[tag:for_loop,Pred|Rest]):-
    predicate_has_more_solutions(Pred),
    list_ends_with(Body,fail),
    pattern(Predicate,Body,Rest).
%repeat pattern
pattern(_,[repeat|Body],[tag:repeat_loop|Rest]):-
    predicates_can_fail(Body,Rest).
%recursion pattern- vector input, output by side-effect
pattern(Predicate,[Pred|Body],[tag:iter_loop(Head)|Rest]):-
    functor(Predicate,Name,Arity),
    functor(Pred,Name,Arity),
    Predicate=..[Name|Args],
    list_contains_list_argument(Args,Index),
    nth0(Index,Args,[Head|VarRest]),
    Pred=..[Name|Args2],
    nth0(Index,Args2,VarRest),
    length(Args,Length),
    list_of_empty_elements(Length,Index,[],Args3),
    New_Predicate=..[Name|Args3],
    catch(New_Predicate,_,fail),%Prevent errors from calling non-existent predicates
    assert(recursion_argument(Index)),
    pattern(Predicate,Body,Rest).
%if pattern
pattern(Predicate,[Pred|Body],[tag:if_clause,Pred|Rest]):-
    is_math(Pred),
    pattern(Predicate,Body,Rest).
pattern(Predicate,[Pred|Body],[tag:if_not,Pred|Rest]):-
    is_negation(Pred),
    pattern(Predicate,Body,Rest).
%no patttern
pattern(_,[fail|_],[]).
pattern(Predicate,[Pred|Body],[Pred|Rest]):-
    pattern(Predicate,Body,Rest).
pattern(_,[],[]).

%Identifies and tags which predicates from the list can fail
predicates_can_fail([Pred|Body],[tag:canfail,Pred|Rest]):-
    predicate_can_fail(Pred),
    predicates_can_fail(Body,Rest).
predicates_can_fail([Pred|Body],[Pred|Rest]):-
    predicates_can_fail(Body,Rest).
predicates_can_fail([],[]).

%Finds argument with format [Head|Rest] in list with Index. Prevents bugs member/2 and nth0/3 would otherwise cause when dealing with variables.
list_contains_list_argument(List,Index):-
    list_contains_list_argument(List,Index,0).
list_contains_list_argument([Head|_],Index,Index):-
    \+var(Head),
    Head=[_|_].
list_contains_list_argument([_|Rest],Index,Count):-
    Up is Count+1,
    list_contains_list_argument(Rest,Index,Up).

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

%If list of lists contains iteration tag, insert all of them into main list,
%so that they all belong to the same iteration cycle. Also deletes extra iteration tags
eliminate_iteration_redundancy([Pattern|Rest],[Pattern|ProcessedRest]):- %%%%%%%%%%%%%%%%%%%%%%%%%%%FLATTEN AND UNINDENT
    member(tag:iter_loop(_),Pattern),
    delete_from_matrix(tag:iter_loop(_),Rest,ProcessedRest).
eliminate_iteration_redundancy([Pattern|Rest],[Pattern|ProcessedRest]):-
    \+member(tag:iter_loop(_),Pattern),
    eliminate_iteration_redundancy(Rest,ProcessedRest).
eliminate_iteration_redundancy([],[]).

%Get variables dictionary for all variables in list of patterns
get_variables_dictionary(Patterns,VarsDic):-
    flatten(Patterns, FlatPatterns),
    get_variables_list(FlatPatterns,VarsList),
    list_to_set(VarsList,VarsSet),
    create_variables_dictionary(VarsSet,VarsDic).

%Second argument is list of all variables in 1st argument
get_variables_list([Head|Rest],VarsList):-
    term_variables(Head,Vars),
    get_variables_list(Rest,VarsRest),
    append(Vars,VarsRest,VarsList).
get_variables_list([],[]).

%Creates list of atom:variable pairs from variables in list
create_variables_dictionary(List,Dic):-
    create_variables_dictionary(List,Dic,[]).
create_variables_dictionary([Head|Rest],[Head:Var|Dic],List):-
    generate_variable(Var),
    \+member(Var,List),
    create_variables_dictionary(Rest,Dic,[Var|List]).
create_variables_dictionary([],[],_).

%Head is predicate head in atom format
process_predicate_head(Pred,VarsDic,Head):-
    Pred=..[Name|Args],
    translate_head_args(Args,VarsDic,TransArgs),
    atomic_concat(Name,'(', Atom1),
    atomic_list_concat(TransArgs,',',AtomArgs),
    atomic_concat(Atom1, AtomArgs, Atom2),
    atomic_concat(Atom2,')', Head).

%Translate variables in head of predicate
translate_head_args(HeadArgs,VarsDic,TransArgs):-
    \+catch(recursion_argument(_),_,fail),
    translate_variables(HeadArgs,VarsDic,TransArgs).
translate_head_args(HeadArgs,VarsDic,TransArgs):-
    retract(recursion_argument(Index)),
    switch_index(Index,HeadArgs,list,NewArgs),
    translate_variables(NewArgs,VarsDic,TransArgs).

%Translate variables in list with dictionary
translate_variables([Var|Rest],VarsDic,[Trans|TransRest]):-
    var(Var),
    get_translation(Var,VarsDic,Trans),
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([Head|Rest],VarsDic,[list2|TransRest]):-
    \+var(Head),
    Head=[_|_],
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([Atom|Rest],VarsDic,[Atom|TransRest]):-
    \+var(Atom),
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([],_,[]).

%Translated is Var translated according to the dictionary
get_translation(Var,[Head:Translated|_],Translated):-
    Var==Head.
get_translation(Var,[_|Rest],Translated):-
    get_translation(Var,Rest,Translated).
get_translation(_,[],var).

%Processes patterns in list of patterns
process_patterns_list([Pattern|Rest],VarsDic,[Processed2|Processed_Rest]):-
    process_patterns(Pattern,VarsDic,Processed),!,
    process_recursion(Processed,Processed2),
    process_patterns_list(Rest,VarsDic,Processed_Rest),!.
process_patterns_list([],_,[]).

%Processes pattern list
process_patterns([tag:iter_loop(Var)|Rest],VarsDic,[tag:iter_loop(TransVar)|ProcessedRest]):-
    translate_variables([Var],VarsDic,TransVar),
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:for_loop,Pred|Rest],VarsDic,[tag:for_loop,TransArgs,TransPred|ProcessedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:repeat_loop|Rest],VarsDic,[tag:while_loop,true|ProcessedRest]):-
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:if_clause,Pred|Rest],VarsDic,[tag:if_clause,TransPred|ProcessedRest]):-
    (Pred)=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:if_not,\+Pred|Rest],VarsDic,[tag:if_not,TransPred|ProcessedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([Pred|Rest],VarsDic,[TransPred|ProcessedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([],_,[]).

%Make recursive tag the first member of pattern for coherence
process_recursion(Pattern,[tag:iter_loop(Element)|Pattern_without]):-
    member(tag:iter_loop(Element),Pattern),
    delete(Pattern, tag:iter_loop(Element), Pattern_without).
process_recursion(Pattern,Pattern).

%Processes list of processed patterns into list of lists with text and terms
patterns_to_text([Pattern|Rest],[Text|TextRest]):-
    pattern_to_text(Pattern,Text),!,
    patterns_to_text(Rest,TextRest),!.
patterns_to_text([],[]).

%Process processed patterns into text
pattern_to_text([tag:iter_loop(Elements)|Rest],[Text,tag:indent|TextRest]):-
    iter_loop_description(Elements,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:for_loop,Args,Pred|Rest],[Text,tag:indent|TextRest]):-
    for_loop_description(Args,Pred,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:if_clause,Condition|Rest],[Text,tag:indent|TextRest]):-
    if_clause_description(Condition,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:if_not,Condition|Rest],[Text,tag:indent|TextRest]):-
    if_clause_description(not,Condition,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:while_loop,Condition|Rest],[Text,tag:indent|TextRest]):-
    while_loop_description(Condition,Text),
    repeat_pattern_to_text(Rest,TextRest).
pattern_to_text([Predicate|Rest],[Predicate|TextRest]):-
    pattern_to_text(Rest,TextRest).
pattern_to_text([],[]).

%Describe a loop for iterating through a list
iter_loop_description(Elements,for(Text)):-
    atomic_list_concat(Elements,',', AtomElements),
    atom_concat(AtomElements,' in list',Text).

%Describe a for loop with arguments and predicate
for_loop_description(Arguments,Predicate,for(Text2:Predicate)):-
    atomic_list_concat(Arguments,', ', AtomArgs),
    atom_concat('(',AtomArgs,Text1),
    atom_concat(Text1,')',Text2).

%Describe an if clause with condition
if_clause_description(Condition,if(Condition)).
if_clause_description(not,Condition,if(Text)):-
    term_to_atom(Condition,AtomCon),
    atom_concat('not ', AtomCon, Text).

%Describe a while loop with Variables
while_loop_description(Condition,while(Condition)).

%Process predicates in a repeat pattern to text
repeat_pattern_to_text([tag:canfail,Pred|Rest],[watch_execution(Pred), 'if successful',tag:indent|TextRest]):-
    repeat_pattern_to_text(Rest,TextRest).
repeat_pattern_to_text([Pred|Rest],[Pred|TextRest]):-
    repeat_pattern_to_text(Rest,TextRest).
repeat_pattern_to_text([],[break]).

%Process write/1 in all lists into prettier text
process_writes_in_list([Pattern|Rest],[Processed|ProcessedRest]):-
    process_writes(Pattern,Processed),
    process_writes_in_list(Rest,ProcessedRest),!.
process_writes_in_list([],[]).

%Process all write/1 in list into prettier text
process_writes([write(X)|Rest],[Text|ProcessedRest]):-
    process_writes_in_a_row(Rest,[X],Text),
    process_writes_skip(Rest,ProcessedRest).
process_writes([Pred|Rest],[Pred|ProcessedRest]):-
    process_writes(Rest,ProcessedRest).
process_writes([],[]).

%Formats writes in a row into prettier text
process_writes_in_a_row([write(X)|Rest],Args,Text):-
    append(Args,[X],NewArgs),
    process_writes_in_a_row(Rest,NewArgs,Text).
process_writes_in_a_row([nl|_],Args,Text):-
    atomic_list_concat(Args,'","',ArgsAtom),
    atomic_concat('print_line("', ArgsAtom, Atom1),
    atomic_concat(Atom1, '")', Text).
process_writes_in_a_row(_,Args,Text):-
    atomic_list_concat(Args,'","',ArgsAtom),
    atomic_concat('print("', ArgsAtom, Atom1),
    atomic_concat(Atom1, '")', Text).

%Skips writes and nl
process_writes_skip([write(_)|Rest],ProcessedRest):-
    process_writes_skip(Rest,ProcessedRest).
process_writes_skip([nl|Rest],ProcessedRest):-
    process_writes(Rest,ProcessedRest).
process_writes_skip([Pred|Rest],[Pred|ProcessedRest]):-
    process_writes(Rest,ProcessedRest).
process_writes_skip([],[]).

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
print_java_style([tag:unindent|Rest],Indentation,BracketCount):-
    NewIndentation is Indentation-5,
    tab(NewIndentation),
    BracketCount>=2,
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
print_python_style([tag:unindent|Rest],Indentation):-
    NewIndentation is Indentation-5,
    print_python_style(Rest,NewIndentation).
print_python_style([Head|Rest],Indentation):-
    tab(Indentation),
    write(Head),
    nl,
    print_python_style(Rest,Indentation).
print_python_style([],_).

