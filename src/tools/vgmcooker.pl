#!/usr/bin/env swipl

:- use_module(library(main)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).

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

command_code(port0Write(Reg, Val), NextPort) -->
    "ld a, $", xinteger(Reg), eol,
    "ld [hl+], a", eol,
    "ld a, $", xinteger(Val), eol,
    ({NextPort = 1}, "ld [hl+], a", eol;
     {NextPort = 0}, "ld [hl-], a", eol).
command_code(port1Write(Reg, Val), NextPort) -->
    "ld a, $", xinteger(Reg), eol,
    "ld [hl+], a", eol,
    "ld a, $", xinteger(Val), eol,
    "ld [hl-], a", eol,
    ({NextPort = 1};
     {NextPort = 0}, "dec l", eol, "dec l", eol).

command_code(wait735, _) --> [].
command_code(end, _) --> [].

commands_code([], []).
commands_code([Cmd], [Code]) :-
    phrase(command_code(Cmd, 1), Code).
commands_code([Cmd, NextCmd | Cmds], [Code | RestCode]) :-
    (NextCmd = port0Write(_,_), NextPort = 0;
     NextCmd = port1Write(_,_), NextPort = 1),
    phrase(command_code(Cmd, NextPort), Code),
    commands_code([NextCmd | Cmds], RestCode).

cook_(port0Write(Reg, Val), [port0Write(Reg, Val)]).
cook_(port1Write(Reg, Val), [port1Write(Reg, Val)]).
cook_(gbWrite(Reg, Val), [gbWrite(Reg, Val)]).
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

print_frames([], _, _).
print_frames([Frame | Frames], CurFrame, OutFileName) :-
    file_base_name(OutFileName, Basename),
    file_name_extension(NoExt, _, Basename),

    (Frames = [_|_], NextFrame #= CurFrame+1;
     Frames = [], NextFrame #= 0),

    format("SECTION \"~a~d\", ROMX~n", [NoExt, CurFrame]),
    format("~a~d:~n", [NoExt, CurFrame]),
    ([port0Write(_,_)|_] = Frame, format("ld hl, $0001~n");
     [port1Write(_,_)|_] = Frame, format("ld hl, $0003~n");
     [] = Frame),
    commands_code(Frame, FrameCode),
    append(FrameCode, FrameCode1),
    format(FrameCode1),
    format("ld a, BANK(~a~d)~n", [NoExt, NextFrame]),
    format("ld [music_bank], a~n"),
    format("ld a, LOW(~a~d)~n", [NoExt, NextFrame]),
    format("ld [music_pointer], a~n"),
    format("ld a, HIGH(~a~d)~n", [NoExt, NextFrame]),
    format("ld [music_pointer+1], a~n"),
    format("ret~n"),
    print_frames(Frames, NextFrame, OutFileName).

loop_frame(Bytes, LoopOffset, LoopFrame) :-
    length(BeforeLoopBytes, LoopOffset),
    prefix(BeforeLoopBytes, CmdBytes),
    cook(Cmd)

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
    writeln(user_error, "Done cooking"),
    sound_frames(CookedCommands, Frames),
    writeln(user_error, "Done framing"),!,
    print_frames(Frames, 0, InFile).
