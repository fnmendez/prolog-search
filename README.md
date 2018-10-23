# El mundo de la grúas

**Author:** Franco Méndez Z.

**Date:** 2018-1

***

- [Heuristic](#heuristic)
- [Worlds](#worlds)
  - [World 1](#world-1)
  - [World 2](#world-2)
  - [World 3](#world-3)
- [A-Star Tests](#a-star-tests)
  - [Test 1](#test-1)
  - [Test 2](#test-2)
  - [Test 3](#test-3)

***

## Heuristic

```prolog
astar_heuristic1(State, N) :-
    goal_condition(Goal),
    findall(C1, (member(in(C1, S1), Goal), \+ member(in(C1, S1), State)), L1),
    findall(C2, (member(in(C2, _), Goal), member(inside(C2, _), State)), L2),
    length(L1, N1),
    length(L2, N2),
    N is N1 + N2.
```

Mi heurística cuenta 2 cantidades y las suma. La primera es cuantos bloques que debiesen estar en cierto lugar no lo están y la segunda es de aquellos bloques que no están en su lugar están en un camión.

Mi heurística es admisible ya que presenta una clara relajación del problema: se cuenta como costo 1 que el bloque no esté en su lugar siendo que puede costar más de 1 movimiento llevarlo a su lugar, al igual que con el camión, que pueden llegar a ser 3.

## Worlds

### World 1

#### Object instances

```prolog
container(C) :- member(C, [c1, c2, c3, c4, c5, c6]).
pallet(P) :- member(P, [p11, p21, p12, p22, p13]).
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
```

#### Initial situation

```prolog
holds(F, s0) :-
    member(F,
        [in(cam1, cargo1), in(g1, cargo1), in(g2, cargo2), in(g3, cargo3),
        available(g1), available(g2), available(g3), in(p11,cargo1),
        in(p21, cargo1), in(c1, cargo1), in(c2, cargo1), on(c1, p11),
        on(c2, c1), clear(c2), clear(p21), in(p12, cargo2), in(p22, cargo2),
        in(c3, cargo2), in(c4, cargo2), in(c5, cargo2), on(c3, p12),
        on(c4, c3), on(c5, c4), on(c6, p22), clear(c5),
        clear(c6), in(p13, cargo3), clear(p13)]
        ).
```

### World 2

#### Object instances

```prolog
container(C) :- member(C, [c1, c2, c3, c4, c5, c6]).
pallet(P) :- member(P, [p11, p21, p12, p22, p13]).
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
    member(T, [cam1, cam2, cam3]).
```

#### Initial situation

```prolog
holds(F, s0) :-
    member(F,
        [in(cam1, cargo1), in(cam2, cargo2), in(cam3, cargo3), in(g1, cargo1), in(g2, cargo2), in(g3, cargo3),
        available(g1), available(g2), available(g3), in(p11,cargo1),
        in(p21, cargo1), in(c1, cargo1), in(c2, cargo1), on(c1, p11),
        on(c2, c1), clear(c2), clear(p21), in(p12, cargo2), in(p22, cargo2),
        in(c3, cargo2), in(c4, cargo2), in(c5, cargo2), on(c3, p12),
        on(c4, c3), on(c5, c4), on(c6, p22), clear(c5),
        clear(c6), in(p13, cargo3), clear(p13)]
        ).
```

### World 3

#### Object instances

```prolog
container(C) :- member(C, [c1, c2, c3]).
pallet(P) :- member(P, [p11, p12]).
surface(S) :-
    container(S); pallet(S).
site(L) :-
    member(L, [cargo1, cargo2]).
connected(C1, C2) :-
    member([X1, X2], [[cargo1, cargo2], [cargo1, cargo2]]),
    (C1=X1, C2=X2; C1=X2, C2=X1).
crane(G) :-
    member(G, [g1, g2]).
truck(T) :-
    member(T, [cam1, cam2]).
```

#### Initial situation

```prolog
holds(F, s0) :-
    member(F,
        [in(cam1, cargo1), in(cam2, cargo2),
        in(g1, cargo1), in(g2, cargo2),
        available(g1), available(g2),
        in(p11,cargo1), in(p12, cargo2),
        in(c1, cargo1), in(c2, cargo2), in(c3, cargo1),
        on(c1, p11), on(c3, c1), on(c2, p12),
        clear(c2), clear(c3)]
        ).
```

***

## A-Star Tests

### Test World 1

```prolog
goal_condition([in(c1, cargo2), in(c5, cargo1)]).
```

#### A-Star weight 1

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=1551
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
raise(g2,c5,c4,cargo2)
load(g2,c5,cam1,cargo2)
unload(g2,c1,cam1,cargo2)
drive(cam1,cargo2,cargo1)
unload(g1,c5,cam1,cargo1)
drop(g2,c1,c4,cargo2)
drop(g1,c5,p21,cargo1)
```

#### A-Star weight 1.5

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=2493
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
raise(g2,c5,c4,cargo2)
load(g2,c5,cam1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
drive(cam1,cargo2,cargo1)
unload(g1,c5,cam1,cargo1)
drop(g1,c5,p21,cargo1)
```

#### A-Star weight 2

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=2660
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
raise(g2,c5,c4,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
load(g2,c5,cam1,cargo2)
drive(cam1,cargo2,cargo1)
unload(g1,c5,cam1,cargo1)
drop(g1,c5,p21,cargo1)
```

#### A-Star weight 3

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=2714
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
raise(g2,c5,c4,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
load(g2,c5,cam1,cargo2)
drive(cam1,cargo2,cargo1)
unload(g1,c5,cam1,cargo1)
drop(g1,c5,p21,cargo1)
```

### Test World 2

```prolog
goal_condition([in(c1, cargo2), in(c5, cargo1)]).
```

#### A-Star weight 1

```prolog
A* expansions=10500
raise(g2,c5,c4,cargo2)
raise(g1,c2,c1,cargo1)
load(g2,c5,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
unload(g1,c5,cam2,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
drop(g1,c5,p21,cargo1)
```

#### A-Star weight 1.5

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=12624
raise(g2,c5,c4,cargo2)
load(g2,c5,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
unload(g1,c5,cam2,cargo1)
drop(g1,c5,p11,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
```

#### A-Star weight 2

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=12808
raise(g2,c5,c4,cargo2)
load(g2,c5,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
unload(g1,c5,cam2,cargo1)
drop(g1,c5,p21,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
```

#### A-Star weight 3

```prolog
Plan length=11
A* expansions=24989
raise(g2,c5,c4,cargo2)
raise(g1,c2,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,c4,cargo2)
load(g2,c5,cam1,cargo2)
drive(cam1,cargo2,cargo1)
unload(g1,c5,cam1,cargo1)
drop(g1,c5,p21,cargo1)
```

### Test World 3

#### A-Star weight 1

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=661
raise(g1,c3,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
raise(g2,c2,p12,cargo2)
load(g2,c2,cam2,cargo2)
unload(g2,c1,cam1,cargo2)
drive(cam2,cargo2,cargo1)
unload(g1,c2,cam2,cargo1)
drop(g2,c1,p12,cargo2)
drop(g1,c2,p11,cargo1)
```

#### A-Star weight 1.5

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=705
raise(g2,c2,p12,cargo2)
load(g2,c2,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c3,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g1,c1,cam1,cargo1)
unload(g1,c2,cam2,cargo1)
drop(g1,c2,p11,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,p12,cargo2)
```

#### A-Star weight 2

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=805
raise(g2,c2,p12,cargo2)
load(g2,c2,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c3,c1,cargo1)
raise(g1,c1,p11,cargo1)
unload(g1,c2,cam2,cargo1)
drop(g1,c2,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,p12,cargo2)
```

#### A-Star weight 3

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=11
A* expansions=556
raise(g2,c2,p12,cargo2)
raise(g1,c3,c1,cargo1)
raise(g1,c1,p11,cargo1)
load(g2,c2,cam2,cargo2)
drive(cam2,cargo2,cargo1)
unload(g1,c2,cam2,cargo1)
drop(g1,c2,p11,cargo1)
load(g1,c1,cam2,cargo1)
drive(cam2,cargo1,cargo2)
unload(g2,c1,cam2,cargo2)
drop(g2,c1,p12,cargo2)
```

#### A-Star weight 50

```prolog
?- astar(P), pretty_print_plan(P).
Plan length=12
A* expansions=592
raise(g2,c2,p12,cargo2)
load(g2,c2,cam2,cargo2)
drive(cam2,cargo2,cargo1)
raise(g1,c3,c1,cargo1)
unload(g1,c2,cam2,cargo1)
load(g1,c3,cam2,cargo1)
raise(g1,c1,p11,cargo1)
drop(g1,c2,p11,cargo1)
load(g1,c1,cam1,cargo1)
drive(cam1,cargo1,cargo2)
unload(g2,c1,cam1,cargo2)
drop(g2,c1,p12,cargo2)
```
