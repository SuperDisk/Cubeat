:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(reif)).

bool_rep(true, 1).
bool_rep(false, 0).

replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).

in(X, Y, Cond) :-
    X in Y #<==> B, bool_rep(Cond, B).
#=(X, Y, Cond) :-
    X #= Y #<==> B, bool_rep(Cond, B).
#\=(X, Y, Cond) :-
    X #\= Y #<==> B, bool_rep(Cond, B).

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
    member(Reg, [0x16, 0x15, 0x14]).
cook_(gbWrite(Reg, Val), [gbWrite(Reg, Val)]) :-
    maplist(dif(Reg), [0x16, 0x15, 0x14]).
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

op_offset0(0, 0x00).
op_offset0(1, 0x01).
op_offset0(2, 0x02).
op_offset0(3, 0x03).
op_offset0(4, 0x04).
op_offset0(5, 0x05).
op_offset0(6, 0x08).
op_offset0(7, 0x09).
op_offset0(8, 0x0A).
op_offset0(9, 0x0B).
op_offset0(10, 0x0C).
op_offset0(11, 0x0D).
op_offset0(12, 0x10).
op_offset0(13, 0x11).
op_offset0(14, 0x12).
op_offset0(15, 0x13).
op_offset0(16, 0x14).
op_offset0(17, 0x15).

op_offset1(18, 0x00).
op_offset1(19, 0x01).
op_offset1(20, 0x02).
op_offset1(21, 0x03).
op_offset1(22, 0x04).
op_offset1(23, 0x05).
op_offset1(24, 0x08).
op_offset1(25, 0x09).
op_offset1(26, 0x0A).
op_offset1(27, 0x0B).
op_offset1(28, 0x0C).
op_offset1(29, 0x0D).
op_offset1(30, 0x10).
op_offset1(31, 0x11).
op_offset1(32, 0x12).
op_offset1(33, 0x13).
op_offset1(34, 0x14).
op_offset1(35, 0x15).

fourop_operator_channel(0, 0, 0).
fourop_operator_channel(3, 0, 1).
fourop_operator_channel(6, 0, 2).
fourop_operator_channel(9, 0, 3).

fourop_operator_channel(1, 1, 0).
fourop_operator_channel(4, 1, 1).
fourop_operator_channel(7, 1, 2).
fourop_operator_channel(10, 1, 3).

fourop_operator_channel(2, 2, 0).
fourop_operator_channel(5, 2, 1).
fourop_operator_channel(8, 2, 2).
fourop_operator_channel(11, 2, 3).

fourop_operator_channel(18, 9, 0).
fourop_operator_channel(21, 9, 1).
fourop_operator_channel(24, 9, 2).
fourop_operator_channel(27, 9, 3).

fourop_operator_channel(19, 10, 0).
fourop_operator_channel(22, 10, 1).
fourop_operator_channel(25, 10, 2).
fourop_operator_channel(28, 10, 3).

fourop_operator_channel(20, 11, 0).
fourop_operator_channel(23, 11, 1).
fourop_operator_channel(26, 11, 2).
fourop_operator_channel(29, 11, 3).

twoop_operator_channel(0, 0, 0).
twoop_operator_channel(1, 1, 0).
twoop_operator_channel(2, 2, 0).
twoop_operator_channel(6, 3, 0).
twoop_operator_channel(7, 4, 0).
twoop_operator_channel(8, 5, 0).
twoop_operator_channel(12, 6, 0).
twoop_operator_channel(13, 7, 0).
twoop_operator_channel(14, 8, 0).
twoop_operator_channel(18, 9, 0).
twoop_operator_channel(19, 10, 0).
twoop_operator_channel(20, 11, 0).
twoop_operator_channel(24, 12, 0).
twoop_operator_channel(25, 13, 0).
twoop_operator_channel(26, 14, 0).
twoop_operator_channel(30, 15, 0).
twoop_operator_channel(31, 16, 0).
twoop_operator_channel(32, 17, 0).

twoop_operator_channel(3, 0, 1).
twoop_operator_channel(4, 1, 1).
twoop_operator_channel(5, 2, 1).
twoop_operator_channel(9, 3, 1).
twoop_operator_channel(10, 4, 1).
twoop_operator_channel(11, 5, 1).
twoop_operator_channel(15, 6, 1).
twoop_operator_channel(16, 7, 1).
twoop_operator_channel(17, 8, 1).
twoop_operator_channel(21, 9, 1).
twoop_operator_channel(22, 10, 1).
twoop_operator_channel(23, 11, 1).
twoop_operator_channel(27, 12, 1).
twoop_operator_channel(28, 13, 1).
twoop_operator_channel(29, 14, 1).
twoop_operator_channel(33, 15, 1).
twoop_operator_channel(34, 16, 1).
twoop_operator_channel(35, 17, 1).

% bit, ch1, ch2
fourop_channel_pair(0, 0, 3).
fourop_channel_pair(1, 1, 4).
fourop_channel_pair(2, 2, 5).
fourop_channel_pair(3, 9, 12).
fourop_channel_pair(4, 10, 13).
fourop_channel_pair(5, 11, 14).

always_twoop_channel(6).
always_twoop_channel(7).
always_twoop_channel(8).
always_twoop_channel(15).
always_twoop_channel(16).
always_twoop_channel(17).

synth_type(0, 0, fmfm).
synth_type(1, 0, amfm).
synth_type(0, 1, fmam).
synth_type(1, 1, amam).

level_update(fmfm, ['-', '-', '-', '+']).
level_update(amfm, ['+', '-', '-', '+']).
level_update(fmam, ['-', '+', '-', '+']).
level_update(amam, ['+', '-', '+', '+']).

four_op_algo(FourOpFlags, SynthTypes, Chan, Algo) :-
    (fourop_channel_pair(Bit, Chan, C2), C1 = Chan;
     fourop_channel_pair(Bit, C1, Chan), C2 = Chan),
    FourOpFlags /\ (1 << Bit) #\= 0,
    nth0(C1, SynthTypes, ST1),
    nth0(C2, SynthTypes, ST2),
    synth_type(ST1, ST2, Algo).

parent_channel(FourOpFlags, Op, Channel, Which, fourop) :-
    fourop_operator_channel(Op, Channel, Which),
    (fourop_channel_pair(Bit, Channel, _);
     fourop_channel_pair(Bit, _, Channel)),
    FourOpFlags /\ (1 << Bit) #\= 0.

parent_channel(FourOpFlags, Op, Channel, Which, twoop) :-
    twoop_operator_channel(Op, Channel, Which),
    (
        always_twoop_channel(Channel)
    ;
        (
            (
                fourop_channel_pair(Bit, Channel, _)
            ;
                fourop_channel_pair(Bit, _, Channel)
            ),
            FourOpFlags /\ (1 << Bit) #= 0
        )
    ).

decreased_volume(_,_,[],[]).
decreased_volume(A, B, [wait735 | T1], [wait735 | T2]) :-
    decreased_volume(A, B, T1, T2).
decreased_volume(A, B, [wait(Samples) | T1], [wait(Samples) | T2]) :-
    decreased_volume(A, B, T1, T2).
decreased_volume(A, B, [end | T1], [end | T2]) :-
    decreased_volume(A, B, T1, T2).

decreased_volume(A, B, [port0Write(Reg, Val) | T1], [port0Write(Reg, Val) | T2]) :-
    (Reg in 0x40..0x55 #<==> 0),
    (Reg in 0xC0..0xC8 #<==> 0),
    decreased_volume(A, B, T1, T2).
decreased_volume(A, B, [port1Write(Reg, Val) | T1], [port1Write(Reg, Val) | T2]) :-
    (Reg in 0x40..0x55 #<==> 0),
    (Reg in 0xC0..0xC8 #<==> 0),
    (Reg #\= 0x04),
    decreased_volume(A, B, T1, T2).

decreased_volume(_FourOpFlags, SynthTypes, [port1Write(0x04, Val) | T1], [port1Write(0x04, Val) | T2]) :-
    decreased_volume(Val, SynthTypes, T1, T2).

decreased_volume(FourOpFlags, SynthTypes, [port0Write(Reg, Val) | T1], [port0Write(Reg, Val) | T2]) :-
    Reg in 0xC0..0xC8,
    Chan #= Reg - 0xC0,
    Flag #= (Val /\ 1),
    replace(Chan, SynthTypes, Flag, NewSynthTypes),
    decreased_volume(FourOpFlags, NewSynthTypes, T1, T2).

decreased_volume(FourOpFlags, SynthTypes, [port1Write(Reg, Val) | T1], [port1Write(Reg, Val) | T2]) :-
    Reg in 0xC0..0xC8,
    Chan #= (Reg - 0xC0) + 9,
    Flag #= (Val /\ 1),
    replace(Chan, SynthTypes, Flag, NewSynthTypes),
    decreased_volume(FourOpFlags, NewSynthTypes, T1, T2).

decreased_volume(FourOpFlags, SynthTypes, [port0Write(Reg, Val) | T1], [port0Write(Reg, NewVal) | T2]) :-
    Reg in 0x40..0x55, % key scale / output level command
    Offset #= Reg - 0x40,
    op_offset0(Op, Offset),
    oldvol_newvol(FourOpFlags, SynthTypes, Op, Val, NewVal),
    decreased_volume(FourOpFlags, SynthTypes, T1, T2).

decreased_volume(FourOpFlags, SynthTypes, [port1Write(Reg, Val) | T1], [port1Write(Reg, NewVal) | T2]) :-
    Reg in 0x40..0x55, % key scale / output level command
    Offset #= Reg - 0x40,
    op_offset1(Op, Offset),
    oldvol_newvol(FourOpFlags, SynthTypes, Op, Val, NewVal),
    decreased_volume(FourOpFlags, SynthTypes, T1, T2).

oldvol_newvol(FourOpFlags, SynthTypes, Op, Val, NewVal) :-
    %% determine what channel+mode we're in, from operator+fouropflags
    %% if twoop, set volume conditionally on synthtype+which operator we are
    %% if fourop, get the algo based on synthtypes
    %%   - get the table based on algo, set volume conditionally on algo+which operator we are

    parent_channel(FourOpFlags, Op, Channel, Which, Mode),
    if_(Mode = twoop,
       (
           nth0(Channel, SynthTypes, SynthType),
           if_(SynthType + Which #\= 0, % if in AM mode, or we're the second operator
              (
                  KSL #= (Val /\ 0b11000000),
                  OutputLevel #= (Val /\ 0b00111111),
                  ReducedOutputLevel #= min(OutputLevel + 16, 63),
                  NewVal #= (KSL \/ ReducedOutputLevel)
              ),
              (
                  NewVal = Val
              ))
       ),
       (
           four_op_algo(FourOpFlags, SynthTypes, Channel, Algo),
           level_update(Algo, Table),
           nth0(Which, Table, ShouldSet),
           if_(ShouldSet = '+',
              (
                  KSL #= (Val /\ 0b11000000),
                  OutputLevel #= (Val /\ 0b00111111),
                  ReducedOutputLevel #= min(OutputLevel + 16, 63),
                  NewVal #= (KSL \/ ReducedOutputLevel)
              ),
              (
                  NewVal = Val
              ))
       )).

dbg_load(LO, X) :-
    phrase_from_file((vgm(LO, X), ...), "../res/music/nerve.vgm", [type(binary)]).

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

is_port0_write(port0Write(_,_)).
partitioned_frame(Frame, [PortOne, PortZero]) :-
    partition(is_port0_write, Frame, PortZero, PortOne).

command_bytes(port0Write(A,B), [A,B]).
command_bytes(port1Write(A,B), [A,B]).

byteified_frame([P1,P0], [P1Bytes1, P0Bytes1]) :-
    maplist(command_bytes, P1, P1Bytes),
    maplist(command_bytes, P0, P0Bytes),
    append(P1Bytes, P1Bytes1),
    append(P0Bytes, P0Bytes1).
