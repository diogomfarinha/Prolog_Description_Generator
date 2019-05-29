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
    process_loop_conjunctions(Processed,Loop),!,
    filter_subjects(Loop,Filtered),!,
    process_punctuation(Filtered,Processed2),!,
    process_conjunctions(Processed2,Processed3),!,
    process_subjects(Name,Processed3,Processed4),!,
    process_phrases(Processed4,Text),!,
    nl,nl,
    write(Text).
prolog_to_nl(Name/Arity):-
    prolog_to_imperative_info(Name/Arity,Head,Body),
    process_header(Head,Header),
    delete_tags_from_list(Body,Clean_Body),
    process_body(Clean_Body,Processed),!,
    process_loop_conjunctions(Processed,Loop),!,
    filter_subjects(Loop,Filtered),!,
    process_punctuation(Filtered,Processed2),!,
    process_conjunctions(Processed2,Processed3),!,
    process_subjects(Name,Processed3,Processed4),!,
    process_phrases([Header|Processed4],Text),!,
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
    process_loop_conjunctions(Processed,Loop),!,
    writeq(loop:Loop),nl,
    filter_subjects(Loop,Filtered),!,
    writeq(filtered:Filtered),nl,
    process_punctuation(Filtered,Processed2),!,
    writeq(punctuation:Processed2),nl,
    process_conjunctions(Processed2,Processed3),!,
    writeq(conjunctions:Processed3),nl,
    process_subjects(Name,Processed3,Processed4),!,
    writeq(subjects:Processed4),nl,
    process_phrases(Processed4,Text),!,
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
    process_loop_conjunctions(Processed,Loop),!,
    writeq(loop:Loop),nl,
    filter_subjects(Loop,Filtered),!,
    writeq(filtered:Filtered),nl,
    process_punctuation(Filtered,Processed2),!,
    writeq(punctuation:Processed2),nl,
    process_conjunctions(Processed2,Processed3),!,
    writeq(conjuntions:Processed3),nl,
    process_subjects(Name,Processed3,Processed4),!,
    writeq(subjects:Processed4),nl,
    process_phrases([Header|Processed4],Text),!,
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
    process_snippet(Head,Processed),
    process_snippet(Rest,ProcessedRest).

%Processes individial code snippets of imperative-style description into natural language
process_snippet([for(Variables:Predicate)|Rest],[Desc,tag:loop_conjunction,tag:subject|DescRest]):-
    Variables=..[_|[Var]],
    atom_concat('for each ',Var,Atom1),
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
process_snippet([Pred|Rest],[Desc,tag:conjunction|DescRest]):-
    compound(Pred),
    procedure_description(Pred,present,Desc),!,
    process_snippet(Rest,DescRest).
process_snippet([Head|Rest],[Head|DescRest]):-
    process_snippet(Rest,DescRest).
process_snippet([],[]).

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
    

    