#!/usr/bin/env swipl

:- consult(vgm).
:- use_module(library(main)).
:- initialization(main, main).

opt_type(in_file, in_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),
    member(in_file(InFile), Opts),

    phrase_from_file(seq(Bytes), InFile, [type(binary)]),
    phrase(vgm(LoopOffset, Commands0), Bytes, _),
    writeln(user_error, "Done parsing"),!,

    %% decreased_volume(0, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], Commands0, Commands),
    %% writeln(user_error, "Done decreasing volume"),!,
    Commands0=Commands,

    %% loop_frame(Bytes, LoopOffset, LoopFrame),
    %% format(user_error, "Found loop frame: ~d~n", [LoopFrame]),!,
    LoopFrame=0,

    cook(Commands, CookedCommands),
    writeln(user_error, "Done cooking"),!,

    sound_frames(CookedCommands, Frames),
    writeln(user_error, "Done framing"),!,

    maplist(partitioned_frame, Frames, SortedFrames),
    writeln(user_error, "Done sorting"),!,

    maplist(byteified_frame, SortedFrames, ByteifiedFrames),
    print(LoopFrame), nl, print(ByteifiedFrames),
    writeln(user_error, "Done!"),!.
