#!/usr/bin/env swipl

:- use_module(library(main)).
:- initialization(main, main).
:- use_module(library(pure_input)).

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

opt_type(chr_file, chr_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),
    member(chr_file(InFile), Opts),

    phrase_from_file(seq(Bytes), InFile, [type(binary)]),

    length(ZeroTile, 32), % each tile is 32 bytes
    maplist(=(0), ZeroTile),

    length(FirstTile, 32),

    append([FirstTile, Before, ZeroTile, After], Bytes),
    append([ZeroTile, FirstTile, Before, After], NewBytes),

    open(InFile, write, Stream, [type(binary)]),
    maplist(put_byte(Stream), NewBytes),
    close(Stream).
