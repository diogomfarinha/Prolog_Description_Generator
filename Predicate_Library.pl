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

%Returns rule from Predicate in list format
rule(Predicate,Rule):-
    clause(Predicate,Body),
    conjunction_to_list(Body,Rule),
    \+is_list_with_boolean(Rule).

%Predicate is fact
is_fact(Pred):-
    predicate_property(Pred,number_of_rules(0)).

%Counts occurences of element in list
count_member(Element,[Element|Rest],Count_Up):-
    Count_Up is Count+1,
    count_member(Rest,Element,Count).
count_member(Element,[Head|Rest],Count):-
    Head\=Element,
    count_member(Rest,Element,Count).
count_member(_,[],0).

%Gets all rules for a predicate in list format. List contains lists of predicates, each list represents a different rule.
get_rules(Pred,_):-
    assert(rules(Pred,[])),
    rule(Pred,Rule),
    retract(rules(Pred,List_of_Rules)),
    append(List_of_Rules,[Rule],New_List_of_Rules),
    assert(rules(Pred,New_List_of_Rules)),
    fail.
get_rules(Pred,List_of_Rules):-
    retract(rules(Pred,List_of_Rules)).

%Predicate has more than one solution
predicate_has_more_solutions(Pred):-
    is_fact(Pred).
predicate_has_more_solutions(member(X,List)):-
    var(X),
    length(List,Length),
    Length>1.
predicate_has_more_solutions(member(X,List)):-
    count_member(X,List,Count),
    Count>1.
predicate_has_more_solutions(Pred):-
    \+predicate_property(Pred,built_in),
    \+predicate_property(Pred,imported_from(lists)),
    get_rules(Pred,List_of_Rules),
    list_has_more_solutions(List_of_Rules,[Pred]),!.

%Predicate has more than one solution. Contains list of iterated predicates to prevent infite loops with recursive predicates
predicate_has_more_solutions(Pred,_):-
    is_fact(Pred).
predicate_has_more_solutions(member(X,List),_):-
    var(X),
    length(List,Length),
    Length>1.
predicate_has_more_solutions(member(X,List),_):-
    count_member(X,List,Count),
    Count>1.
predicate_has_more_solutions(Pred,List_of_Predicates):-
    \+predicate_property(Pred,built_in),
    \+predicate_property(Pred,imported_from(lists)),
    \+member(Pred,List_of_Predicates),
    get_rules(Pred,List_of_Rules),
    append(List_of_Predicates,[Pred],New_List_of_Predicates),
    list_has_more_solutions(List_of_Rules,New_List_of_Predicates),!.

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
is_math(\+_).
is_math(_=_).
is_math(_==_).
is_math(_\=_).
is_math(_<_).
is_math(_>_).
is_math(_=<_).
is_math(_>=_).

%Predicate can fail when called
predicate_can_fail(Pred):-
    is_math(Pred).
predicate_can_fail(Pred):-
    is_fact(Pred).
predicate_can_fail(member(_,_)).
predicate_can_fail(Pred):-
    \+predicate_property(Pred,built_in),
    \+predicate_property(Pred,imported_from(lists)),
    get_rules(Pred,List_of_Rules),
    list_can_fail(List_of_Rules,[Pred]),!.

%Predicate can fail when called. Contains list of iterated predicates to prevent infite loops with recursive predicates
predicate_can_fail(Pred,_):-
    is_math(Pred).
predicate_can_fail(member(_,_),_).
predicate_can_fail(Pred,List_of_Predicates):-
    \+predicate_property(Pred,built_in),
    \+predicate_property(Pred,imported_from(lists)),
    \+member(Pred,List_of_Predicates),
    get_rules(Pred,List_of_Rules),
    append(List_of_Predicates,[Pred],New_List_of_Predicates),
    list_can_fail(List_of_Rules,New_List_of_Predicates),!.

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

