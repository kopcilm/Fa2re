%automaton(+Word, +TransitionFunction, +StartStates, +EndStates)
%
%True iff the finite automaton induced by TransitionFunction accept Word
%
%Word ................. list of atoms
%TransitionFunction ... list of triplets, represented by list [StateFrom, Symbol, StateTo] 
%                       where Symbol is either an atom or predicate lambda() representing lambda transition
%StartStates .......... list of start states
%EndStates ............ list of end states
%
%States are represented as atoms.

lambda().

automaton(Word, TransitionFunction, StartStates, EndStates):-
    once(automaton_(Word, TransitionFunction, StartStates, EndStates)).

automaton_([], TransitionFunction, States, EndStates):-
    lambda_closure(States, TransitionFunction, LambdaClosure),
    member(S, LambdaClosure),
    member(S, EndStates).

automaton_([Symbol|Word], TransitionFunction, States, EndStates):-
    lambda_closure(States, TransitionFunction, LambdaClosure),
    setof(NS, 
    (
        member(S, LambdaClosure),
        member([S, Symbol, NS], TransitionFunction)
    ),
    NewStates),
    automaton_(Word, TransitionFunction, NewStates, EndStates).
    
%lambda_closure(+States, +TransitionFunction, -LambdaClosure)
%
%States ............... list of states
%TransitionFunction ... list of triplets, represented by list [StateFrom, Symbol, StateTo] 
%                       where Symbol is either an atom or predicate lambda() representing lambda transition
%LambdaClosure ........ list of states

lambda_closure(States, TransitionFunction, LambdaClosure):-
    setof(NS, 
        (
            member(S, States),
            member([S, lambda(), NS], TransitionFunction),
            not(member(NS, States))
        ),
        NewStates)
    -> (
        append(States, NewStates, States2),
        lambda_closure(States2, TransitionFunction, LambdaClosure)
    );(
       LambdaClosure = States
    ).


%regular_expression(+Word, +RegExp)
%
%True iff the regular expression given by the RegExp matches the Word
%
%Word ................ list of atoms
%RegExp ............. expression consisting of operators(*, :, +, (, )) and atoms

:- op(250, yf,  *). %iteration
:- op(200, xfy, :). %concatenation
:- op(150, xfy, +). %alternation

regular_expression(Word, RegExp):-
    once(regular_expression_(Word, RegExp, [])).

regular_expression_(Text, RegExp, UnmatchedText):-
    RegExp = lambda() -> (
        UnmatchedText = Text
    );
    atom(RegExp) -> (
        Text = [T|Ts],
        RegExp = T, 
        UnmatchedText = Ts
    );
    RegExp =.. [*, P] -> (
        UnmatchedText = Text;
        (
            regular_expression_(Text, P, UT), 
            regular_expression_(UT, RegExp, UnmatchedText)
        ) 
    );
    RegExp =.. [:, P1, P2] -> (
        regular_expression_(Text, P1, UT1), 
        regular_expression_(UT1, P2, UnmatchedText)    
    );
    RegExp =.. [+, P1, P2] -> (
        regular_expression_(Text, P1, UnmatchedText); 
        regular_expression_(Text, P2, UnmatchedText)     
    ).



%automaton_regexp(+TransitionFunction, +StartStates, +EndStates, -RegularExpression)
%
%converts automaton to regular expression
%
%TransitionFunction ... list of triplets, represented by list [StateFrom, Symbol, StateTo] 
%                       where Symbol is either an atom or predicate lambda() representing lambda transition
%StartStates .......... list of start states
%EndStates ............ list of end states
%RegularExpression .... expression consisting of operators(*, :, +, (, )) and atoms

automaton_regexp(TransitionFunction, StartStates, EndStates, RegularExpression):-
    edges_regexps(TransitionFunction, RegExpEdges),
    one_start_one_end(StartStates, EndStates, StartEndEdges),
    append(RegExpEdges, StartEndEdges, Edges),
    once(eliminate_states(Edges, RegularExpression)).

%eliminate_states(+States, -RegExp)
%
%eliminate states 1 by 1  until only [start(), RE, end()] is left, then returns the regular expression

eliminate_states([[start(), RE, end()]], RE).
eliminate_states(Edges, RE):-
    member(Edge,Edges),
    (Edge=[_, _, State]; Edge=[State, _, _]),
    atom(State),
    create_edges(State, Edges, CreatedEdges), 
    delete_state(State, Edges, EdgesAfterDelete),
    append(CreatedEdges, EdgesAfterDelete, EdgesWithDuplicity), 
    edges_regexps(EdgesWithDuplicity, NewEdges), 
    eliminate_states(NewEdges, RE).


%one_start_one_end(+StartStates, +EndStates, -Edges)
%
%creates new start and end state

start().
end().

one_start_one_end(StartStates, EndStates, Edges):-
    bagof([start(), lambda(), S], member(S, StartStates), StartEdges),
    bagof([S, lambda(), end()], member(S, EndStates), EndEdges),
    append(StartEdges, EndEdges, Edges).
    
    

%edges_regexps(+Edges,-RegExpEdges)
%
%takes automaton edges and return  edges with regexp values 

edges_regexps(Edges, RegExpEdges):-
    bagof(RegExpEdge, edges_regexps_(Edges, RegExpEdge), RegExpEdges). 

edges_regexps_(Edges, RegExpEdge):-
    simplify_edges(Edges, SEdges),
    member([X,Y], SEdges),
    bagof([X,V,Y], member([X,V,Y], Edges), E),
    same_edges_to_regexp(E, RegExpEdge).



%same_edges_to_regexp(+Edges,-RegExpEdge)
%
%takes automaton edges from state 'X' to state 'Y' and returns one edge from 'X' to 'Y' with value being regexp

same_edges_to_regexp([Edge], Edge):-!.
same_edges_to_regexp([[S1, RegExp2,S2], Edge|Edges], [S1, ((RegExp2) + (RegExp1)),S2]):-
    same_edges_to_regexp([Edge|Edges], [S1, RegExp1,S2]).


%simplify_edges(+Edges, -SimplifiedEdges)
%
%returns edges without the values, without duplicity
%
%Edges ................ list of triplets, represented by list [StateFrom, Symbol, StateTo]
%SimplifiedEdges ...... list of tuples, represented by list [StateFrom, StateTo]

simplify_edges(Edges, SimplifiedEdges):-
    simplify_edges_(Edges, SEdges), 
    sort(SEdges, SimplifiedEdges).

simplify_edges_([], []).
simplify_edges_([[X,_,Y]|Edges], [[X,Y]|SEdges]):- 
    simplify_edges_(Edges, SEdges).


%create_edges(+State, +Edges, -NewEdges)
%
%returns new Edges, formed by eliminating State

create_edges(State, Edges, NewEdges):-
    bagof(NE, create_edge(State, Edges, NE), NewEdges).

create_edge(State, Edges, NewEdge):-
    member([State, Loop, State], Edges) -> 
    (
        member([State, RE2, B], Edges),
        member([A, RE1, State], Edges),
        A\=State, B\=State,
        ((RE1=lambda(),RE2=lambda())->
        NewEdge=[A, ((Loop)*), B];
        RE1=lambda()-> 
        NewEdge=[A, (((Loop)*):(RE2)), B];
        RE2=lambda()->
        NewEdge=[A, ((RE1):((Loop)*)), B];
        NewEdge=[A, ((RE1):((Loop)*):(RE2)), B])
    );(
        member([A, RE1, State], Edges),
        member([State, RE2, B], Edges),
        ((RE1=lambda(),RE2=lambda())->
        NewEdge=[A, lambda(), B];
        RE1=lambda()-> 
        NewEdge=[A, (RE2), B];
        RE2=lambda()->
        NewEdge=[A, (RE1), B];
        NewEdge=[A, ((RE1):(RE2)), B])
    ).


%delete_state(+State, +Edges, -NewEdges)
%
%deletes edges which contains State

delete_state(State, Edges, NewEdges):-
    bagof(
        Edge,
        (
            member(Edge, Edges),
            Edge \= [State, _, _],
            Edge \= [_, _, State]
        ),
        NewEdges
    ); %if bagof returns false it means we deleted all edges
    NewEdges=[].



%regexp_automaton(+RegularExpression, -TransitionFunction, -StartStates, -EndStates)
%
%converts regular expression to automaton
%
%RegularExpression .... expression consisting of operators(*, :, +, (, )) and atoms
%TransitionFunction ... list of triplets, represented by list [StateFrom, Symbol, StateTo] 
%                       where Symbol is either an atom or predicate lambda() representing lambda transition
%StartStates .......... list of start states
%EndStates ............ list of end states
%StateName ............ base for states names
%
%states are atoms named 'StateName#' where # is number

regexp_automaton(RegExp, TransitionFunction, [StartState], [EndState], StateName):-
    regexp_automaton_(RegExp, TransitionFunction, StartState, EndState, StateName),
    reset_gensym(StateName).
    

regexp_automaton_(RegExp, TransitionFunction, StartState, EndState, Name):-
    RegExp=lambda() -> (
        gensym(Name, StartState), StartState=EndState,
        TransitionFunction=[]
    );
    atom(RegExp) -> (
        gensym(Name, StartState), gensym(Name, EndState),
        TransitionFunction = [[StartState, RegExp, EndState]]
    );
    RegExp=..[:, RE1, RE2] -> (
        regexp_automaton_(RE1, TF1, SS1, ES1, Name),    
        regexp_automaton_(RE2, TF2, SS2, ES2, Name),
        append(TF1, TF2, TF),
        TransitionFunction=[[ES1, lambda(), SS2] |TF],
        StartState=SS1, EndState=ES2
    );
    RegExp=..[*, RE] -> (
        regexp_automaton_(RE, TF, SS, ES, Name),
        gensym(Name, StartState), gensym(Name, EndState),
        TransitionFunction=[[StartState, lambda(), SS], [ES, lambda(), EndState], 
                            [ES, lambda(), SS], [StartState, lambda(), EndState] |TF]
    );
    RegExp=..[+, RE1, RE2] -> (
        regexp_automaton_(RE1, TF1, SS1, ES1, Name),    
        regexp_automaton_(RE2, TF2, SS2, ES2, Name),
        append(TF1, TF2, TF),
        gensym(Name, StartState), gensym(Name, EndState),
        TransitionFunction=[[StartState, lambda(), SS1], [StartState, lambda(), SS2], 
                            [ES1, lambda(), EndState], [ES2, lambda(), EndState] |TF]
    ).