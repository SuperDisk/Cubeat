#!/usr/bin/env swipl

:- set_stream(user_output, type(binary)).

:- consult(vgm).
:- use_module(library(main)).
:- initialization(main, main).

gb_command_bytes(gbWrite(Reg, Val),
                 [0x3E, Val,
                  0xE0, NewReg]) :-
    NewReg #= Reg + 0x10.
gb_command_bytes(wait735, [0xD7]).
byteified_gb(GB, GBBytes) :-
    maplist(gb_command_bytes, GB, GBBytes0),
    append(GBBytes0, GBBytes).

opt_type(in_file, in_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),
    member(in_file(InFile), Opts),

    phrase_from_file(seq(Bytes), InFile, [type(binary)]),
    phrase(vgm(_LoopOffset, Commands), Bytes, _),
    writeln(user_error, "Done parsing"),!,

    cook(Commands, CookedCommands),
    writeln(user_error, "Done cooking"),!,

    byteified_gb(CookedCommands, ByteifiedCommands),
    maplist(put_byte, ByteifiedCommands),
    put_byte(0xFF),
    writeln(user_error, "Done!"),!.
