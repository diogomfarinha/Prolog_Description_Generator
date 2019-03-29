:- ensure_loaded('Predicate_Library.pl').

%Converts and prints Prolog programs into imperative-style descriptions. Pred is in format Name/Arity
prolog_to_imperative(Pred):-
    get_patterns(Pred,Patterns),
    process_patterns_list(Patterns,Processed_Patterns),
    patterns_to_text(Processed_Patterns,Text_List),
    process_writes_in_list(Text_List,Pretty_Text),
    write(Pred),write('{'),nl,
    print_formatted_predicate(java,Pretty_Text),
    write('}').
prolog_to_imperative(Pred,java):-
    prolog_to_imperative(Pred),!.
prolog_to_imperative(Pred,python):-
    get_patterns(Pred,Patterns),
    process_patterns_list(Patterns,Processed_Patterns),
    patterns_to_text(Processed_Patterns,Text_List),
    process_writes_in_list(Text_List,Pretty_Text),
    write(Pred),write(':'),nl,
    print_formatted_predicate(python,Pretty_Text).

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
% pattern(Predicate,[Pred|Body],[tag:list_recursion|Rest]):-
%     functor(Predicate,Name,1),
%     functor(Pred,Name,1),
%     Predicate=..[Name,[Var1|Var2]],
%     var(Var1),
%     var(Var2),
%     Pred=..[Name,Var3],
%     var(Var3),
%     New_Predicate=..[Name,[]],
%     catch(New_Predicate,_,fail),%Prevent errors from calling non-existant predicates
%     pattern(Predicate,Body,Rest).
%recursion pattern- vector input, output by side-effect -- generalized
pattern(Predicate,[Pred|Body],[tag:list_recursion|Rest]):-
    functor(Predicate,Name,Arity),
    functor(Pred,Name,Arity),
    Predicate=..[Name|Args],
    list_contains_list_argument(Args,Index),
    Pred=..[Name|Args2],
    nth0(Index,Args2,Var3),
    var(Var3),
    length(Args,Length),
    list_of_empty_elements(Length,Index,[],Args3),
    New_Predicate=..[Name|Args3],
    catch(New_Predicate,_,fail),%Prevent errors from calling non-existant predicates
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


%Get Prolog programming patterns in predicate
get_patterns(Name/Arity,Patterns):-
    get_predicate(Name/Arity,Pred),
    get_all_rules_with_term(Pred,List_of_Rules),
    get_patterns_from_rules(List_of_Rules,Patterns),!.

%Get Prolog programming patterns in list of rules
get_patterns_from_rules([[Pred,Rule]|Rest],Patterns):-
    pattern(Pred,Rule,Pattern_from_Rule),!,
    process_recursion(Pattern_from_Rule,Processed_Pattern),!,
    get_patterns_from_rules(Rest,Patterns_from_Rest),
    append([Processed_Pattern],Patterns_from_Rest,Patterns).
get_patterns_from_rules([],[]).

%Process recursion tags into common tags
process_recursion(Pattern,[tag:for_loop,element|Pattern_without]):-
    member(tag:list_recursion,Pattern),
    delete(Pattern, tag:list_recursion, Pattern_without).
process_recursion(Pattern,Pattern).

%Processes patterns in list of patterns
process_patterns_list([Pattern|Rest],[Processed|Processed_Rest]):-
    get_variables_dictionary(Pattern,VarsDic),
    process_patterns(Pattern,VarsDic,Processed),!,
    process_patterns_list(Rest,Processed_Rest),!.
process_patterns_list([],[]).

%VarsDic is a list of atom:variable pairs created from the variables in Preds
get_variables_dictionary(Preds,VarsDic):-
    get_variables_list(Preds,List),
    list_to_set(List,Set),
    create_variables_dictionary(Set,VarsDic,[]),!.

%Second argument is list of all variables in 1st argument
get_variables_list([Head|Rest],VarsList):-
     term_variables(Head,Vars),
     get_variables_list(Rest,VarsRest),
     append(Vars,VarsRest,VarsList).
get_variables_list([],[]).

%Creates list of atom:variable pairs from variables in list
create_variables_dictionary([Head|Rest],[Head:Var|Dic],List):-
    generate_variable(Var),
    \+member(Var,List),
    create_variables_dictionary(Rest,Dic,[Var|List]).
create_variables_dictionary([],[],_).

%Processes pattern list
process_patterns([tag:for_loop,element|Rest],VarsDic,[tag:for_loop,[element],list|ProcessedRest]):-
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:for_loop,Pred|Rest],VarsDic,[tag:for_loop,TransArgs,TransPred|ProcessedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([tag:repeat_loop|Rest],VarsDic,[tag:while_loop,true|ProcessedRest]):-
    process_patterns(Rest,VarsDic,ProcessedRest).
%TODO PROCESS RECURSION PATTERN
process_patterns([Pred|Rest],VarsDic,[TransPred|ProcessedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarsDic,TransArgs),
    TransPred=..[Name|TransArgs],
    process_patterns(Rest,VarsDic,ProcessedRest).
process_patterns([],_,[]).

%Translate variables in list with dictionary
translate_variables([Var|Rest],VarsDic,[Trans|TransRest]):-
    var(Var),
    get_translation(Var,VarsDic,Trans),
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
get_translation(X,[],X).

%Processes list of processed patterns into list of lists with text and terms
patterns_to_text([Pattern|Rest],[Text|TextRest]):-
    pattern_to_text(Pattern,Text),
    patterns_to_text(Rest,TextRest),!.
patterns_to_text([],[]).

%Process processed patterns into text
pattern_to_text([tag:for_loop,Args,Pred|Rest],[Text,tag:indent|TextRest]):-
    for_loop_description(Args,Pred,Text),
    pattern_to_text(Rest,TextRest).
pattern_to_text([tag:while_loop,Condition|Rest],[Text,tag:indent|TextRest]):-
    while_loop_description(Condition,Text),
    repeat_pattern_to_text(Rest,TextRest).
%TODO PROCESS RECURSION PATTERN
pattern_to_text([Predicate|Rest],[Predicate|TextRest]):-
    pattern_to_text(Rest,TextRest).
pattern_to_text([],[]).

%Describe a for loop with Arguments and Predicate
for_loop_description(Arguments,Predicate,Description):-
    atomic_list_concat(Arguments,', ', AtomArgs),
    term_to_atom(Predicate,AtomPred),
    atom_concat('for ', AtomArgs, Text1),
    atom_concat(Text1, ' in ', Text2),
    atom_concat(Text2, AtomPred, Description).

%Describe a while loop with Variables
while_loop_description(Condition,Description):-
    atom_concat('while(', Condition, Text1),
    atom_concat(Text1, ')', Description).

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
    atomic_list_concat(Args,' ',ArgsAtom),
    atomic_concat('print_line(', ArgsAtom, Atom1),
    atomic_concat(Atom1, ')', Text).
process_writes_in_a_row(_,Args,Text):-
    atomic_list_concat(Args,' ',ArgsAtom),
    atomic_concat('print(', ArgsAtom, Atom1),
    atomic_concat(Atom1, ')', Text).

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

%Writes text in a java-like style of formatting
print_java_style([Head,tag:indent|Rest],Indentation,Count):-
    tab(Indentation),
    write(Head),write('{'),
    nl,
    NewIndentation is Indentation+5,
    NewCount is Count+1,
    print_java_style(Rest,NewIndentation,NewCount).
print_java_style([tag:indent|Rest],Indentation,Count):-
    NewIndentation is Indentation+5,
    NewCount is Count+1,
    print_java_style(Rest,NewIndentation,NewCount).
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
print_python_style([Head|Rest],Indentation):-
    tab(Indentation),
    write(Head),
    nl,
    print_python_style(Rest,Indentation).
print_python_style([],_).

