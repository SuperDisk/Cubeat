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

... -->
    [].
... -->
    [_],
    ... .

vgm_header -->
    skipB(0x34), u16(DataOffset), {Off #= DataOffset - 2}, skipB(Off).

commands([Cmd|Cmds]) --> command(Cmd), commands(Cmds).
commands([]) --> [].

command(port0Write(Reg, Val)) --> [0x5E, Reg, Val].
command(port1Write(Reg, Val)) --> [0x5F, Reg, Val].
command(gbWrite(Reg, Val)) --> [0xB3, Reg, Val].
command(wait(Samples)) --> [0x61], u16(Samples).
command(wait(735)) --> [0x62].
command(wait(882)) --> [0x63].
command(end) --> [0x66].

zorb(a) --> [1,2].
zorb(b) --> [3].
zorb(c) --> [6,1,2,2].

first_bank([], CurBytes, _, [], BytesOut) :-
    reverse([0xFF | CurBytes], BytesOut).
first_bank(Cmds, CurBytes, CurLen, Cmds, BytesOut) :-
    CurLen #> (16*1024)-5,
    reverse([0xFF | CurBytes], BytesOut).
first_bank([Cmd|Cmds], CurBytes, CurLen, CmdsOut, BytesOut) :-
    CurLen #=< (16*1024)-5,
    phrase(command(Cmd), Bytes),
    length(Bytes, BytesLength),
    append(Bytes, CurBytes, NewBytes),
    NewLen #= CurLen + BytesLength,
    first_bank(Cmds, NewBytes, NewLen, CmdsOut, BytesOut).

all_banks([], []).
all_banks(Cmds, [Bank|Banks]) :-
    Cmds = [_|_],
    first_bank(Cmds, [], 0, RemainingCmds, Bank),
    all_banks(RemainingCmds, Banks).

vgm(Cmds) -->
    vgm_header, commands(Cmds).

print_sections([], _, _).
print_sections([Bank | Banks], Offset, OutFileName) :-
    length(Bank, BankLen),
    gensym(vgm, Sym),
    format(current_output, "SECTION \"~a\", ROMX~n", [Sym]),
    format(current_output, "INCBIN \"~a\", ~d, ~d~n", [OutFileName, Offset, BankLen]),
    Offset1 #= Offset + BankLen,
    print_sections(Banks, Offset1, OutFileName).

opt_type(in_file, in_file, file).
opt_type(out_file, out_file, file).
main([]) :-
    argv_usage(debug).
main(Argv) :-
    argv_options(Argv, _, Opts),

    member(in_file(InFile), Opts),
    member(out_file(OutFile), Opts),
    phrase_from_file((vgm(Commands), ...), InFile, [type(binary)]),
    all_banks(Commands, Banks),
    append(Banks, OutData),
    open(OutFile, write, Out, [type(binary)]),
    maplist(put_byte(Out), OutData),
    print_sections(Banks, 0, OutFile).
