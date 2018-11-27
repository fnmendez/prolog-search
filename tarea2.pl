% Handle warnings
:- discontiguous holds/2, is_negative_effect/2, is_positive_effect/2, poss/2.

%%%%%%%%%%% OBJECTS INSTANCES %%%%%%%%%%%

container(C) :- member(C, [c1, c2, c3, c4, c5, c6]).
pallet(P) :- member(P,[p11, p21, p12, p22, p13]).
surface(S) :-
    container(S); pallet(S).
site(L) :-
    member(L, [cargo1, cargo2, cargo3]).
connected(C1, C2) :-
    member([X1, X2], [[cargo1, cargo2], [cargo2, cargo3]]),
    (C1=X1, C2=X2; C1=X2, C2=X1).
crane(G) :-
    member(G, [g1, g2, g3]).
truck(T) :-
    member(T, [cam1]).

%%%%%%%%%%% INITIAL SITUATION %%%%%%%%%%%
holds(F, s0) :- member(F,[in(cam1,cargo1),
         in(g1,cargo1),in(g2,cargo2),in(g3,cargo3),
         available(g1),available(g2),
         in(p11,cargo1),in(p21,cargo1),
         in(c1,cargo1),in(c2,cargo1),
         on(c1,p11),on(c2,c1),
         clear(c2),clear(p21),
         in(p12,cargo2),in(p22,cargo2),
         in(c3,cargo2),in(c4,cargo2),in(c5,cargo2),
         on(c3,p12),on(c4,c3),on(c5,c4),
         on(c6,p22),
         clear(c5),
         clear(c6),
         in(p13,cargo3),clear(p13)]).

%%%%%%%%%%% ACTIONS CONDITIONS %%%%%%%%%%%

% Drive
poss(drive(Cam, L1, L2), S) :-
    truck(Cam), connected(L1, L2), holds(in(Cam, L1), S).

% Raise
poss(raise(G, C, Sur, L), S) :-
    crane(G), container(C), surface(Sur), site(L),
    holds(available(G), S), holds(on(C, Sur), S), holds(clear(C), S),
    holds(in(G, L), S), holds(in(C, L), S), holds(in(Sur, L), S).

% Drop
poss(drop(G, C, Sur, L), S) :-
    crane(G), container(C), surface(Sur), site(L),
    holds(raising(G, C), S), holds(clear(Sur), S),
    holds(in(G, L), S), holds(in(Sur, L), S).

% Load
poss(load(G, C, T, L), S) :-
    crane(G), container(C), truck(T), site(L),
    holds(raising(G, C), S),
    holds(in(G, L), S), holds(in(T, L), S).

% Unload
poss(unload(G, C, T, L), S) :-
    crane(G), container(C), truck(T), site(L),
    holds(available(G), S), holds(inside(C, T), S),
    holds(in(G, L), S), holds(in(T, L), S).

%%%%%%%%%%% ACTIONS EFFECTS %%%%%%%%%%%

% Drive
is_positive_effect(drive(Cam, _, L2), in(Cam, L2)).
is_negative_effect(drive(Cam, L1, _), in(Cam, L1)).

% Raise
is_positive_effect(raise(G, C, _, _), raising(G, C)).
is_positive_effect(raise(_, _, Sur, _), clear(Sur)).
is_negative_effect(raise(_, C, Sur, _), on(C, Sur)).
is_negative_effect(raise(_, C, _, L), in(C, L)).

% Drop
is_positive_effect(drop(_, C, _, L), in(C, L)).
is_positive_effect(drop(_, C, Sur, _), on(C, Sur)).
is_negative_effect(drop(G, C, _, _), raising(G, C)).
is_negative_effect(drop(_, _, Sur, _), clear(Sur)).

% Load
is_positive_effect(load(_, C, T, _), inside(C, T)).
is_positive_effect(load(G, _, _, _), available(G)).
is_negative_effect(load(G, C, _, _), raising(G, C)).

% Unload
is_positive_effect(unload(G, C, _, _), raising(G, C)).
is_negative_effect(unload(G, _, _, _), available(G)).
is_negative_effect(unload(_, C, T, _), inside(C, T)).

%%%%% Situation Calculus Successor State Axiom a la Reiter (domain-independent)
holds(F, do(A, S)) :-
    holds(F, S),
    \+ is_negative_effect(A, F).

holds(F, do(A, _)) :-
    is_positive_effect(A, F).

%%%%% Legal Situations are those produced by executing
%%%%% generates situations in a breadth-first manner

legal(s0).
legal(do(A, S)) :-
    legal(S),
    poss(A, S).

% If you want to generate a plan use a query like
% legal(S), holds(on(b,a),S).

%%

goal_condition([in(c1, cargo2), in(c2, cargo1)]).

astar_heuristic(State, N) :-
    astar_heuristic1(State, N).
    % N is 3 * N1.

astar_heuristic0(_, 0).

astar_heuristic1(State, N) :-
    goal_condition(Goal),
    findall(C1, (member(in(C1, S1), Goal), \+ member(in(C1, S1), State)), L1),
    findall(C2, (member(in(C2, _), Goal), member(inside(C2, _), State)), L2),
    length(L1, N1),
    length(L2, N2),
    N is N1 + N2.

astar_heuristic2(State, N) :-
    goal_condition(Goal),
    findall(C, (member(in(C, S), Goal), \+ member(in(C, S), State)), L),
    length(L, N).

astar_heuristic3(State, N) :-
    findall(C1, (member(in(C1, S1), Goal), \+ member(in(C1, S1), State)), L1),
    findall(C2, (member(in(C2, _), Goal), member(inside(C2, _), State)), L2),
    findall(C3, (member(in(C3, S2), Goal), member(raising(G1, C3), State), \+ member(in(G1, S2), State)), L3),
    length(L1, N1),
    length(L2, N2),
    length(L3, N3),
    N is N1 + N2 + N3.

%%astar_heuristic4(State, N) :-
