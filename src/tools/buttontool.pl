:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(main)).

:- initialization(main, main).

print_lisp(_-V) :- var(V), !.
print_lisp(K-V) :- format("(~a . ~a)", [K, V]).

slices_distinct(Slices) :-
    append(Slices, Pairs),
    sort(Pairs, DistinctPairs),
    maplist([_-Idx, Idx]>>true, DistinctPairs, Indexes),
    all_different(Indexes).

distinct_in_window(Window, []) :-
    slices_distinct(Window).
distinct_in_window(Window, [Slice | RemainingSlices]) :-
    slices_distinct(Window),
    Window = [_ | Window1],
    append(Window1, [Slice], Window2),
    distinct_in_window(Window2, RemainingSlices).

slices_indices([], []).
slices_indices([Slice | Rest], Indices) :-
    maplist([_-Idx, Idx]>>true, Slice, Indices1),
    slices_indices(Rest, Indices2),
    append(Indices1, Indices2, Indices).

go(Slices, Indices) :-
    slices_indices(Slices, Indices0),
    sort(Indices0, Indices),

    length(Window, 23), % HACK: This can't be right, but it bandaids over the problem for now...
    append(Window, RemainingSlices, Slices),
    distinct_in_window(Window, RemainingSlices).

special_cases([]).
special_cases([999-Idx | T]) :-
    Idx #= -3,
    special_cases(T).
special_cases([998-Idx | T]) :-
    Idx #= -2,
    special_cases(T).
special_cases([997-Idx | T]) :-
    Idx #= -1,
    special_cases(T).
special_cases([K-V|T]) :-
    K #\= 999,
    K #\= 998,
    K #\= 997,
    V in 0..211,
    special_cases(T).

main(_Argv) :-
    read([Slices, Pairs]),
    special_cases(Pairs),
    go(Slices, Indices),
    labeling([ff], Indices),
    maplist(print_lisp, Pairs).
