% Handle warnings
:- discontiguous holds/2, is_negative_effect/2, is_positive_effect/2, poss/2.

%%%%%%%%%%% OBJECTS INSTANCES %%%%%%%%%%%

container(C) :- member(C, [c1, c2, c3, c4, c5, c6]).
paleta(P) :- member(P,[p11, p21, p12, p22, p13]).
superficie(S) :-
    container(S); paleta(S).
lugar(L) :-
    member(L, [cargo1, cargo2, cargo3]).
conectada(C1, C2) :-
    member([X1, X2], [[cargo1, cargo2], [cargo2, cargo3]]),
    (C1=X1, C2=X2; C1=X2, C2=X1).
grua(G) :-
    member(G, [g1, g2, g3]).
camion(T) :-
    member(T, [cam1]).

%%%%%%%%%%% INITIAL SITUATION %%%%%%%%%%%
holds(F, s0) :- member(F,[en(cam1,cargo1),
         en(g1,cargo1),en(g2,cargo2),en(g3,cargo3),
         disponible(g1),disponible(g2),
         en(p11,cargo1),en(p21,cargo1),
         en(c1,cargo1),en(c2,cargo1),
         sobre(c1,p11),sobre(c2,c1),
         despejada(c2),despejada(p21),
         en(p12,cargo2),en(p22,cargo2),
         en(c3,cargo2),en(c4,cargo2),en(c5,cargo2),
         sobre(c3,p12),sobre(c4,c3),sobre(c5,c4),
         sobre(c6,p22),
         despejada(c5),
         despejada(c6),
         en(p13,cargo3),despejada(p13)]).

%%%%%%%%%%% ACTIONS CONDITIONS %%%%%%%%%%%

% Drive
poss(manejar(Cam, L1, L2), S) :-
    camion(Cam), conectada(L1, L2), holds(en(Cam, L1), S).

% Raise
poss(levantar(G, C, Sur, L), S) :-
    grua(G), container(C), superficie(Sur), lugar(L),
    holds(disponible(G), S), holds(sobre(C, Sur), S), holds(despejada(C), S),
    holds(en(G, L), S), holds(en(C, L), S), holds(en(Sur, L), S).

% Drop
poss(soltar(G, C, Sur, L), S) :-
    grua(G), container(C), superficie(Sur), lugar(L),
    holds(levantando(G, C), S), holds(despejada(Sur), S),
    holds(en(G, L), S), holds(en(Sur, L), S).

% Load
poss(cargar(G, C, T, L), S) :-
    grua(G), container(C), camion(T), lugar(L),
    holds(levantando(G, C), S),
    holds(en(G, L), S), holds(en(T, L), S).

% Unload
poss(descargar(G, C, T, L), S) :-
    grua(G), container(C), camion(T), lugar(L),
    holds(disponible(G), S), holds(inside(C, T), S),
    holds(en(G, L), S), holds(en(T, L), S).

%%%%%%%%%%% ACTIONS EFFECTS %%%%%%%%%%%

% Drive
is_positive_effect(manejar(Cam, _, L2), en(Cam, L2)).
is_negative_effect(manejar(Cam, L1, _), en(Cam, L1)).

% Raise
is_positive_effect(levantar(G, C, _, _), levantando(G, C)).
is_positive_effect(levantar(_, _, Sur, _), despejada(Sur)).
is_negative_effect(levantar(_, C, Sur, _), sobre(C, Sur)).
is_negative_effect(levantar(_, C, _, L), en(C, L)).

% Drop
is_positive_effect(soltar(_, C, _, L), en(C, L)).
is_positive_effect(soltar(_, C, Sur, _), sobre(C, Sur)).
is_negative_effect(soltar(G, C, _, _), levantando(G, C)).
is_negative_effect(soltar(_, _, Sur, _), despejada(Sur)).

% Load
is_positive_effect(cargar(_, C, T, _), inside(C, T)).
is_positive_effect(cargar(G, _, _, _), disponible(G)).
is_negative_effect(cargar(G, C, _, _), levantando(G, C)).

% Unload
is_positive_effect(descargar(G, C, _, _), levantando(G, C)).
is_negative_effect(descargar(G, _, _, _), disponible(G)).
is_negative_effect(descargar(_, C, T, _), inside(C, T)).

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
% legal(S), holds(sobre(b,a),S).

%%

goal_condition([en(c1, cargo2), en(c5, cargo1)]).

astar_heuristic(State, N) :-
    astar_heuristic1(State, N).
    % N is 3 * N1.

astar_heuristic0(_, 0).

astar_heuristic1(State, N) :-
    goal_condition(Goal),
    findall(C1, (member(en(C1, S1), Goal), \+ member(en(C1, S1), State)), L1),
    findall(C2, (member(en(C2, _), Goal), member(inside(C2, _), State)), L2),
    length(L1, N1),
    length(L2, N2),
    N is N1 + N2.

astar_heuristic2(State, N) :-
    goal_condition(Goal),
    findall(C, (member(en(C, S), Goal), \+ member(en(C, S), State)), L),
    length(L, N).

astar_heuristic3(State, N) :-
    findall(C1, (member(en(C1, S1), Goal), \+ member(en(C1, S1), State)), L1),
    findall(C2, (member(en(C2, _), Goal), member(inside(C2, _), State)), L2),
    findall(C3, (member(en(C3, S2), Goal), member(levantando(G1, C3), State), \+ member(en(G1, S2), State)), L3),
    length(L1, N1),
    length(L2, N2),
    length(L3, N3),
    N is N1 + N2 + N3.

%%astar_heuristic4(State, N) :-
