#!/usr/bin/env swipl

:- use_module(library(main)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(reif)).

:- initialization(main, main).

bool_rep(true, 1).
bool_rep(false, 0).

in(X, Y, Cond) :-
    X in Y #<==> B, bool_rep(Cond, B).

skipB(N) --> {length(Ls, N)}, Ls.
u16(N) --> [B1, B2],
           {N in 0..65535,
            [B1, B2] ins 0..255,
            N #= (B2 << 8) + B1}.
u32(N) --> [B1, B2, B3, B4],
           {N in 0..4294967295,
            [B1, B2, B3, B4] ins 0..255,
            N #= (B4 << 24) + (B3 << 16) + (B2 << 8) + B1}.

... --> [].
... --> [_], ... .
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

vgm_header(LoopOffset) -->
    skipB(0x1C), u16(LoopOffset0), {LoopOffset #= (LoopOffset0+0x1C) - (DataOffset+0x34)},
    skipB(0x16), u16(DataOffset), {Off #= DataOffset - 2}, skipB(Off).

vgm(LoopOffset, Cmds) -->
    vgm_header(LoopOffset), commands(Cmds).

commands([Cmd|Cmds]) --> command(Cmd), commands(Cmds).
commands([]) --> [].

command(port0Write(Reg, Val)) --> [0x5E, Reg, Val].
command(port1Write(Reg, Val)) --> [0x5F, Reg, Val].
command(gbWrite(Reg, Val)) --> [0xB3, Reg, Val].
command(wait(Samples)) --> [0x61], u16(Samples).
command(wait735) --> [0x62].
command(end) --> [0x66].

sound_frames_([], [], []).
sound_frames_([], CurFrame, [CurFrame]) :-
    CurFrame = [_|_].
sound_frames_([H | T], CurFrame, Frames) :-
    dif(H, wait735),
    sound_frames_(T, [H | CurFrame], Frames).
sound_frames_([wait735 | T], CurFrame, [CurFrame | Frames]) :-
    sound_frames_(T, [], Frames).

sound_frames(Cmds, Frames) :-
    sound_frames_(Cmds, [], Frames0),
    maplist(reverse, Frames0, Frames).

command_code(port0Write(Reg, Val)) -->
    " lb de, $", xinteger(Reg), ", $", xinteger(Val), eol,
    "rst Port0Write", eol.
command_code(port1Write(Reg, Val)) -->
    " lb de, $", xinteger(Reg), ", $", xinteger(Val), eol,
    "rst Port1Write", eol.
command_code(wait735) --> [].
command_code(end) --> [].

commands_code([], []).
commands_code([Cmd | Cmds], [Code | RestCode]) :-
    phrase(command_code(Cmd), Code),
    commands_code(Cmds, RestCode).

cook_(port0Write(Reg, Val), [port0Write(Reg, Val)]).
cook_(port1Write(Reg, Val), [port1Write(Reg, Val)]).
cook_(gbWrite(Reg, _), []) :-
    Reg #>= 0x14.
cook_(gbWrite(Reg, Val), [gbWrite(Reg, Val)]) :-
    Reg #=< 0x14.
cook_(wait735, [wait735]).
cook_(wait(Samples), []) :-
    Samples #=< 0.
cook_(wait(Samples), [wait735 | MoreWaits]) :-
    Samples #> 0,
    RemainingSamples #= Samples - 735,
    cook_(wait(RemainingSamples), MoreWaits).
cook_(end,[]).

cook(Cmds, Cooked) :-
    maplist(cook_, Cmds, NewCmds),
    append(NewCmds, Cooked).

dbg_load(LO, X) :-
    phrase_from_file((vgm(LO, X), ...), "../res/cosmic.vgm", [type(binary)]).

command_sm83(Command, DecompPointer, Code, NewDecompPointer) :-
    (Command = port0Write(Reg, Val), Opcode = 0xF7;
     Command = port1Write(Reg, Val), Opcode = 0xD7),
    BaseCode = [0x11, Reg, Val, Opcode],
    Distance #= 0x1000 - (DecompPointer mod 0x1000),
    if_(Distance in 0..3,
        (length(Nops, Distance),
         maplist(=(0x00), Nops)),
        Nops = []),
    append(Nops, BaseCode, Code),
    length(Code, L),
    NewDecompPointer #= (DecompPointer + L) mod 0x1000.
command_sm83(wait735, []).
command_sm83(end, []).

frame_sm83([], DP0, [[0xC9]], DP1) :-
    DP1 #= DP0 + 1.
frame_sm83([Cmd|Cmds], DP0, [Code|Codes], DP2) :-
    command_sm83(Cmd, DP0, Code, DP1),
    frame_sm83(Cmds, DP1, Codes, DP2).

frames_sm83([], DP, [], DP).
frames_sm83([Frame|Frames], DP0, [Code|Codes], DP2) :-
    frame_sm83(Frame, DP0, Code0, DP1),
    append(Code0, Code),
    frames_sm83(Frames, DP1, Codes, DP2).

print_frames(Frames) :-
    frames_sm83(Frames, 0, SM83Frames, _),
    %% maplist(frame_sm83, Frames, SM83Frames),
    writeln(SM83Frames).

loop_frame(Bytes, LoopOffset, LoopFrame) :-
    length(HeaderBytes, 0x100),
    append(HeaderBytes, DataBytes, Bytes),

    length(BeforeLoopBytes, LoopOffset),
    prefix(BeforeLoopBytes, DataBytes),

    phrase(commands(Cmds), BeforeLoopBytes),
    cook(Cmds, CookedCmds),
    include(=(wait735), CookedCmds, OnlyWaits),
    length(OnlyWaits, LoopFrame0),
    LoopFrame #= LoopFrame0 - 1.

opt_type(in_file, in_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),
    member(in_file(InFile), Opts),

    phrase_from_file(seq(Bytes), InFile, [type(binary)]),
    phrase(vgm(_LoopOffset, Commands), Bytes, _),
    writeln(user_error, "Done parsing"),!,

    %% loop_frame(Bytes, LoopOffset, LoopFrame),
    %% format(user_error, "Found loop frame: ~d~n", [LoopFrame]),!,

    cook(Commands, CookedCommands),
    writeln(user_error, "Done cooking"),!,

    sound_frames(CookedCommands, Frames),
    writeln(user_error, "Done framing"),!,

    print_frames(Frames),
    writeln(user_error, "Done!"),!.
