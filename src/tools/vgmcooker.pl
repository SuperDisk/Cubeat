#!/usr/bin/env swipl

:- use_module(library(main)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module(library(lists)).
:- use_module(library(reif)).

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

cook(port0Write(Reg, Val), [port0Write(Reg, Val)]).
cook(port1Write(Reg, Val), [port1Write(Reg, Val)]).
cook(gbWrite(Reg, Val), [gbWrite(Reg, Val)]).
cook(wait735, [wait735]).
cook(wait(Samples), []) :-
    Samples #=< 0.
cook(wait(Samples), [wait735 | MoreWaits]) :-
    Samples #> 0,
    RemainingSamples #= Samples - 735,
    cook(wait(RemainingSamples), MoreWaits).
cook(end,[]).

cooked_loop_point(Bytes, OldLoopPoint, NewLoopPoint) :-
    length(IntroBytes, OldLoopPoint),
    prefix(IntroBytes, Bytes),
    phrase(commands(Cmds), IntroBytes),
    maplist(cook, Cmds, CookedCmds0),
    append(CookedCmds0, CookedCmds),
    phrase(commands(CookedCmds), CookedBytes),
    length(CookedBytes, NewLoopPoint).

first_bank([], CurBytes, _, [], BytesOut) :-
    reverse(CurBytes, BytesOut).
first_bank(Cmds, CurBytes, CurLen, Cmds, BytesOut) :-
    CurLen #> (16*1024)-7,
    reverse(CurBytes, BytesOut).
first_bank([Cmd|Cmds], CurBytes, CurLen, CmdsOut, BytesOut) :-
    CurLen #=< (16*1024)-7,
    phrase(command(Cmd), Bytes0),
    reverse(Bytes0, Bytes),
    length(Bytes, BytesLength),
    append(Bytes, CurBytes, NewBytes),
    NewLen #= CurLen + BytesLength,
    first_bank(Cmds, NewBytes, NewLen, CmdsOut, BytesOut).

all_banks([], []).
all_banks(Cmds, [Bank|Banks]) :-
    Cmds = [_|_],
    first_bank(Cmds, [], 0, RemainingCmds, Bank),
    all_banks(RemainingCmds, Banks).

dbg_load(LO, X) :-
    phrase_from_file((vgm(LO, X), ...), "../res/cosmic.vgm", [type(binary)]).

print_sections([], _, _, _, _).
print_sections([Bank | Banks], Offset, CurBank, LoopOffset, OutFileName) :-
    file_base_name(OutFileName, Basename),
    file_name_extension(NoExt,_,Basename),
    length(Bank, BankLen),
    format("SECTION \"~a~d\", ROMX~n", [NoExt, CurBank]),
    format("~a~d:~n", [NoExt, CurBank]),
    format("INCBIN \"~a\", ~d, ~d~n", [OutFileName, Offset, BankLen]),
    (
        Banks = [_|_],
        NextBank #= CurBank+1,
        format("db $FF, BANK(~a~d)~n", [NoExt, NextBank]),
        format("dw ~a~d~n", [NoExt, NextBank]),
        Offset1 #= Offset + BankLen,
        CurBank1 #= CurBank + 1,
        print_sections(Banks, Offset1, CurBank1, LoopOffset, OutFileName)
    ;
        Banks = [],
        LoopStartBank #= LoopOffset div ((16*1024)-7),
        LoopOffsetMod #= LoopOffset mod ((16*1024)-7),
        format("db $FE, BANK(~a~d)~n", [NoExt, LoopStartBank]),
        format("dw ~a~d+$~16r~n", [NoExt, LoopStartBank, LoopOffsetMod])
    ).

opt_type(in_file, in_file, file).
opt_type(out_file, out_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),

    member(in_file(InFile), Opts),
    member(out_file(OutFile), Opts),

    phrase_from_file(seq(Bytes), InFile, [type(binary)]),
    phrase(vgm(LoopOffset, Commands), Bytes, _),
    maplist(cook, Commands, CookedCommands),
    append(CookedCommands, FinalCommands),
    all_banks(FinalCommands, Banks),
    append(Banks, OutData),
    open(OutFile, write, Out, [type(binary)]),
    maplist(put_byte(Out), OutData),

    length(HeaderBytes, 0x100),
    append(HeaderBytes, DataBytes, Bytes),

    cooked_loop_point(DataBytes, LoopOffset, CookedLoopOffset),
    print_sections(Banks, 0, 0, CookedLoopOffset, OutFile).
