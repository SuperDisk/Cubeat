:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(main)).

:- initialization(main, main).

grab(_Name-Index, Free0, Free1) :-
    select(Index, Free0, Free1).

grabs([], Free, Free).
grabs([Name-Index | NamesRest], Free0, Free2) :-
    grab(Name-Index, Free0, Free1),
    grabs(NamesRest, Free1, Free2).

union_free(Names, Free, Free1) :-
    maplist([_-Idx, Idx]>>true, Names, Indexes),
    union(Free, Indexes, Free1).

names_indexes([], _Free).
names_indexes([diff(Incoming, Evicted) | DiffRest], Free) :-
    union_free(Evicted, Free, Free1),
    grabs(Incoming, Free1, Free2),
    names_indexes(DiffRest, Free2).

print_lisp([]).
print_lisp(K-V) :-
    format("(~a . ~a)", [K, V]).

main(_Argv) :-
    read([Names, Diffs, Free]),
    names_indexes(Diffs, Free),
    maplist(print_lisp, Names).

%% main(A,B,C,D,E,F) :-
%%     names_indexes(
%%         [
%%             diff([a-A], []),
%%             diff([b-B], []),
%%             diff([], [b-B]),
%%             diff([c-C], []),
%%             diff([d-D, e-E], [c-C]),
%%             diff([f-F], [d-D])
%%         ],
%%         [0,1,2]
%%     ).


%% A A A A A A
%%   B
%%       C
%%         D
%%         E E
%%           F
