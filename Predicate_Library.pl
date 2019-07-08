%Get predicate with arity
get_predicate(Name/Arity,Pred):-
    functor(Pred,Name,Arity).

%Item is first element of list
list_begins_with([Item|_],Item).

%Item is last element of list
list_ends_with([_|Body],Item):-
    list_ends_with(Body,Item).
list_ends_with([Item],Item).

%Converts conjunction into list
conjunction_to_list((A, B), List):-
    !,
    conjunction_to_list(A, La),
    conjunction_to_list(B, Lb),
    append(La, Lb, List).
conjunction_to_list(T, [T]).

%List has a single boolean
is_list_with_boolean([true]).
is_list_with_boolean([false]).

%Rule is list correspoding to the rule's body
rule(Predicate,Rule):-
    clause(Predicate,Body),
    conjunction_to_list(Body,Rule),
    \+is_list_with_boolean(Rule).

%Predicate is fact
is_fact(Pred):-
    predicate_property(Pred,number_of_rules(0)).

%Counts occurences of element in list
count_member(Element,[Element|Rest],Count_Up):-
    count_member(Element,Rest,Count),
    Count_Up is Count+1.
count_member(Element,[_|Rest],Count):-
    count_member(Element,Rest,Count).
count_member(_,[],0).

%Gets all rules for a predicate in list format. List contains lists with rule term and rule body (in list format)
get_all_rules_with_term(Pred,_):-
    assert(list_of_rules([])),
    rule(Pred,Rule),
    retract(list_of_rules(List)),
    append(List,[[Pred,Rule]],New_List),
    assert(list_of_rules(New_List)),
    fail.
get_all_rules_with_term(_,List):-
    retract(list_of_rules(List)).

%Gets all rules of a predicate in list format. List contains lists with rule bodies
get_all_rules(Pred,Rules):-
    list_of_definition_bodies(Pred,Rules),!.

%Gets list of definition bodies from predicate head
list_of_definition_bodies(Head, Bodies):-
    findall(Head:-Body, rule(Head, Body), Clauses),
    list_of_bodies(Head, Clauses, Bodies).
list_of_bodies(Head, [Head:-B|Clauses], [B|Bodies]):-
    list_of_bodies(Head, Clauses, Bodies).
list_of_bodies(_, [], []).

%Asserts if fact does not exist
assert_if_not_exists(predicate_instantiated(X)):-
    \+catch(predicate_instantiated(_),_,fail),
    assert(predicate_instantiated(X)).
assert_if_not_exists(_).

%Predicate does not have more than one solution
predicate_does_not_have_more_solutions(age(_,_)).

%Predicate has more than one solution
predicate_has_more_solutions(Pred):-
    predicate_has_more_solutions(Pred,[]).

%Predicate has more than one solution. Contains list of iterated predicates to prevent infite loops with recursive predicates
predicate_has_more_solutions(Pred,_):-
    predicate_does_not_have_more_solutions(Pred),!,fail.
predicate_has_more_solutions(Pred,_):-
    is_fact(Pred).
predicate_has_more_solutions(member(_,_),_).
predicate_has_more_solutions(Pred,List_of_Predicates):-
    \+predicate_property(Pred,built_in),
    \+predicate_property(Pred,imported_from(lists)),
    \+member(Pred,List_of_Predicates),
    get_all_rules(Pred,List_of_Rules),
    list_has_more_solutions(List_of_Rules,[Pred|List_of_Predicates]),!.

%Rule contains a predicate that has more than one solution
rule_has_more_solutions([Head|_],List_of_Predicates):-
    predicate_has_more_solutions(Head,List_of_Predicates).
rule_has_more_solutions([_|Rest],List_of_Predicates):-
    rule_has_more_solutions(Rest,List_of_Predicates).

%List has a rule that has predicate that has more than one solution
list_has_more_solutions([Rule|_],List_of_Predicates):-
    rule_has_more_solutions(Rule,List_of_Predicates).
list_has_more_solutions([_|Rest],List_of_Predicates):-
    list_has_more_solutions(Rest,List_of_Predicates).

%Generates variable names ex: x;...;s;x1;x2;
generate_variable(x).
generate_variable(y).
generate_variable(z).
generate_variable(w).
generate_variable(i).
generate_variable(j).
generate_variable(k).
generate_variable(p).
generate_variable(l).
generate_variable(u).
generate_variable(t).
generate_variable(r).
generate_variable(q).
generate_variable(f).
generate_variable(d).
generate_variable(s).
generate_variable(Variable):-
    generate_variable(X),
    atom_chars(X,[AtomX]),
    generate_number(N),
    atom_number(AtomN,N),
    atom_concat(AtomX,AtomN,Variable).

%Generates numbers from 0 onwards 
generate_number(0).
generate_number(Number):-
    generate_number(N),
    Number is N+1.

%Second list contains arguments from all Predicates in List
get_all_args([Pred|Rest],[Args|ArgsRest]):-
    Pred=..[_|Args],
    get_all_args(Rest,ArgsRest).
get_all_args([],[]).

%Predicate is a math equation
is_math(_=_).
is_math(_==_).
is_math(_\=_).
is_math(_<_).
is_math(_>_).
is_math(_=<_).
is_math(_>=_).

%Predicate is negated
is_negation(\+_).

%Predicate cannot fail when called
predicate_cannot_fail(write(_)).
predicate_cannot_fail(nl).

%Predicate can fail when called
predicate_can_fail(Pred):-
    predicate_can_fail(Pred,[]).

%Predicate can fail when called. Contains list of iterated predicates to prevent infite loops with recursive predicates
predicate_can_fail(Pred,_):-
    predicate_cannot_fail(Pred),!,fail.
predicate_can_fail(Pred,_):-
    is_math(Pred).
predicate_can_fail(Pred,_):-
    is_negation(Pred).
predicate_can_fail(Pred,_):-
    is_fact(Pred). 
predicate_can_fail(Pred,_):-
    predicate_property(Pred,imported_from(lists)).
predicate_can_fail(Pred,List_of_Predicates):-
    \+predicate_property(Pred,built_in),
    \+member(Pred,List_of_Predicates),
    get_all_rules(Pred,List_of_Rules),
    list_can_fail(List_of_Rules,[Pred|List_of_Predicates]),!.

%Rule contains a predicate that can fail
rule_can_fail([Head|_],List_of_Predicates):-
    predicate_can_fail(Head,List_of_Predicates).
rule_can_fail([_|Rest],List_of_Predicates):-
    rule_can_fail(Rest,List_of_Predicates).

%List has a rule that has predicate that can fail
list_can_fail([Rule|_],List_of_Predicates):-
    rule_can_fail(Rule,List_of_Predicates).
list_can_fail([_|Rest],List_of_Predicates):-
    list_can_fail(Rest,List_of_Predicates).

%Switch list element on index
switch_index(Index,List,Element,NewList):-
    switch_index(0,Index,List,Element,NewList).
switch_index(Count,Index,[Head|Rest],Element,[Head|NewRest]):-
    Count\=Index,
    CountUp is Count+1,
    switch_index(CountUp,Index,Rest,Element,NewRest).
switch_index(Index,Index,[_|Rest],Element,[Element|Rest]).
switch_index(_,_,[],_,[]).

%Get element from list of lists according to indexes
get_element_from_matrix(Index1,Index2,List_of_Lists,Element):-
    nth0(Index1,List_of_Lists,List),
    nth0(Index2,List,Element).

%Delete element from all lists in list of lists
delete_from_matrix(Element,[Head|Rest],[Clean|CleanRest]):-
    delete(Head,Element,Clean),
    delete_from_matrix(Element,Rest,CleanRest).
delete_from_matrix(_,[],[]).

%Adds element as Head of every list in list of lists
add_to_matrix([Head|Rest],Element,[[Element|Head],NewRest]):-
    add_to_matrix(Rest,Element,NewRest).
add_to_matrix([],_,[]).

%Element belongs to a  list in list of lists
matrix_member(Element,[List|_]):-
    member(Element,List).
matrix_member(Element,[List|Rest]):-
    \+member(Element,List),
    matrix_member(Element,Rest).

%Disjunction between conditions encompasses all possible cases
opposite_conditions(X=Y,X\=Y).
opposite_conditions(X=Y,Y\=X).
opposite_conditions(X\=Y,X=Y).
opposite_conditions(X\=Y,Y=X).
opposite_conditions(X==Y,X\=Y).
opposite_conditions(X==Y,Y\=X).
opposite_conditions(X\=Y,X==Y).
opposite_conditions(X\=Y,Y==X).
opposite_conditions(X<Y,X>=Y).
opposite_conditions(X>=Y,X<Y).
opposite_conditions(X>Y,X=<Y).
opposite_conditions(X=<Y,X>Y).

%List is list of rules in File that belong to Pred. Pred is in format Name/Arity. Throws error if Pred does not exist in file.
get_rules_with_variables(File,Pred,List):-
    get_all_rules_with_variables(File,Pred,List),
    List=[],
    throw('Predicate does not exist in file.').
get_rules_with_variables(File,Pred,List):-
    get_all_rules_with_variables(File,Pred,List).

%List is list of rules in File that belong to Pred. Pred is in format Name/Arity
get_all_rules_with_variables(File,Pred,List):-
    open(File, read, Stream),
    read_and_process_terms(Stream,Pred,List),
    close(Stream),!.

%Reads and processes term from stream
read_and_process_terms(Stream,Pred,List):-
    read_term(Stream,Term, [variable_names(VarNames)]),
    process_term(Stream,Pred,List,Term,VarNames).

%Processes Term. List contains translated heands and bodies of terms whose name and arity match the given predicate
process_term(_,_,[],end_of_file,_).
process_term(Stream,Name/Arity,[[Pred|TranslatedBody]|Rest],Head:-Body,VarNames):-
    Head=..[Name|Args],
    length(Args,Length),
    Length=Arity,
    translate_variables(Args,VarNames,Translated),
    Pred=..[Name|Translated],
    conjunction_to_list(Body,BodyList),
    translate_predicates(BodyList,VarNames,TranslatedBody),
    read_and_process_terms(Stream,Name/Arity,Rest).
process_term(Stream,Pred,List,_,_):-
    read_and_process_terms(Stream,Pred,List).

%Translate variables of predicates in list
translate_predicates([Pred|Rest],VarNames,[Translated|TranslatedRest]):-
    Pred=..[Name|Args],
    translate_variables(Args,VarNames,TranslatedArgs),
    Translated=..[Name|TranslatedArgs],
    translate_predicates(Rest,VarNames,TranslatedRest).
translate_predicates([],_,[]).

%Translate variables in list with dictionary
translate_variables([Var|Rest],VarsDic,[Trans|TransRest]):-
    var(Var),
    get_translation(Var,VarsDic,Trans),
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([[X|Y]|Rest],VarsDic,[[TransX|TransY]|TransRest]):-
    get_translation(X,VarsDic,TransX),
    get_translation(Y,VarsDic,TransY),
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([Atom|Rest],VarsDic,[Atom|TransRest]):-
    \+var(Atom),
    translate_variables(Rest,VarsDic,TransRest).
translate_variables([],_,[]).

%Translated is Var translated according to the dictionary
get_translation(Var,[Translated=Head|_],Translated):-
    Var==Head. % Must use == to compare non-instantiated variables
get_translation(Var,[_|Rest],Translated):-
    get_translation(Var,Rest,Translated).
get_translation(_,[],'_').



