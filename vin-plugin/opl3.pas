unit opl3;

(* Nuked OPL3
 * Copyright (C) 2013-2020 Nuke.YKT
 *
 * This file is part of Nuked OPL3.
 *
 * Nuked OPL3 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Nuked OPL3 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Nuked OPL3. If not, see <https://www.gnu.org/licenses/>.

 *  Nuked OPL3 emulator.
 *  Thanks:
 *      MAME Development Team(Jarek Burczynski, Tatsuyuki Satoh):
 *          Feedback and Rhythm part calculation information.
 *      forums.submarine.org.uk(carbon14, opl3):
 *          Tremolo and phase generator calculation information.
 *      OPLx decapsulated(Matthew Gambrell, Olli Niemitalo):
 *          OPL2 ROMs.
 *      siliconpr0n.org(John McMaster, digshadow):
 *          YMF262 and VRC VII decaps and die shots.
 *
 * version: 1.8
 *
 * Pascal port by Claude
 *)

{$mode objfpc}{$H+}

interface

const
  OPL_WRITEBUF_SIZE = 1024;
  OPL_WRITEBUF_DELAY = 2;
  RSM_FRAC = 10;

type
  (* Forward declarations *)
  p_opl3_slot = ^opl3_slot;
  p_opl3_channel = ^opl3_channel;
  p_opl3_chip = ^opl3_chip;

  opl3_slot = record
    channel: p_opl3_channel;
    chip: p_opl3_chip;
    out_: Int16;
    fbmod: Int16;
    modp: PInt16;
    prout: Int16;
    eg_rout: UInt16;
    eg_out: UInt16;
    eg_inc: UInt8;
    eg_gen: UInt8;
    eg_rate: UInt8;
    eg_ksl: UInt8;
    trem: PUInt8;
    reg_vib: UInt8;
    reg_type: UInt8;
    reg_ksr: UInt8;
    reg_mult: UInt8;
    reg_ksl: UInt8;
    reg_tl: UInt8;
    reg_ar: UInt8;
    reg_dr: UInt8;
    reg_sl: UInt8;
    reg_rr: UInt8;
    reg_wf: UInt8;
    key: UInt8;
    pg_reset: UInt32;
    pg_phase: UInt32;
    pg_phase_out: UInt16;
    slot_num: UInt8;
  end;

  opl3_channel = record
    slotz: array [0..1] of p_opl3_slot;
    pair: p_opl3_channel;
    chip: p_opl3_chip;
    out_: array [0..3] of PInt16;
    chtype: UInt8;
    f_num: UInt16;
    block: UInt8;
    fb: UInt8;
    con: UInt8;
    alg: UInt8;
    ksv: UInt8;
    cha, chb: UInt16;
    chc, chd: UInt16;
    ch_num: UInt8;
  end;

  opl3_writebuf = record
    time: UInt64;
    reg: UInt16;
    data: UInt8;
  end;

  opl3_chip = record
    channel: array [0..17] of opl3_channel;
    slot: array [0..35] of opl3_slot;
    timer: UInt16;
    eg_timer: UInt64;
    eg_timerrem: UInt8;
    eg_state: UInt8;
    eg_add: UInt8;
    eg_timer_lo: UInt8;
    newm: UInt8;
    nts: UInt8;
    rhy: UInt8;
    vibpos: UInt8;
    vibshift: UInt8;
    tremolo: UInt8;
    tremolopos: UInt8;
    tremoloshift: UInt8;
    noise: UInt32;
    zeromod: Int16;
    mixbuff: array [0..3] of Int32;
    rm_hh_bit2: UInt8;
    rm_hh_bit3: UInt8;
    rm_hh_bit7: UInt8;
    rm_hh_bit8: UInt8;
    rm_tc_bit3: UInt8;
    rm_tc_bit5: UInt8;

    (* OPL3L *)
    rateratio: Int32;
    samplecnt: Int32;
    oldsamples: array [0..3] of Int16;
    samples: array [0..3] of Int16;

    writebuf_samplecnt: UInt64;
    writebuf_cur: UInt32;
    writebuf_last: UInt32;
    writebuf_lasttime: UInt64;
    writebuf: array [0..OPL_WRITEBUF_SIZE-1] of opl3_writebuf;
  end;

procedure OPL3_Generate(chip: p_opl3_chip; buf: PInt16);
procedure OPL3_GenerateResampled(chip: p_opl3_chip; buf: PInt16);
procedure OPL3_Reset(chip: p_opl3_chip; samplerate: UInt32);
procedure OPL3_WriteReg(chip: p_opl3_chip; reg: UInt16; v: UInt8);
procedure OPL3_WriteRegBuffered(chip: p_opl3_chip; reg: UInt16; v: UInt8);
procedure OPL3_GenerateStream(chip: p_opl3_chip; sndptr: PInt16; numsamples: UInt32);
procedure OPL3_Generate4Ch(chip: p_opl3_chip; buf4: PInt16);
procedure OPL3_Generate4ChResampled(chip: p_opl3_chip; buf4: PInt16);
procedure OPL3_Generate4ChStream(chip: p_opl3_chip; sndptr1: PInt16; sndptr2: PInt16; numsamples: UInt32);

implementation

const
  (* Channel types *)
  ch_2op = 0;
  ch_4op = 1;
  ch_4op2 = 2;
  ch_drum = 3;

  (* Envelope key types *)
  egk_norm = $01;
  egk_drum = $02;

  (* Envelope generator states *)
  envelope_gen_num_attack = 0;
  envelope_gen_num_decay = 1;
  envelope_gen_num_sustain = 2;
  envelope_gen_num_release = 3;

  (* logsin table *)
  logsinrom: array[0..255] of UInt16 = (
    $859, $6c3, $607, $58b, $52e, $4e4, $4a6, $471,
    $443, $41a, $3f5, $3d3, $3b5, $398, $37e, $365,
    $34e, $339, $324, $311, $2ff, $2ed, $2dc, $2cd,
    $2bd, $2af, $2a0, $293, $286, $279, $26d, $261,
    $256, $24b, $240, $236, $22c, $222, $218, $20f,
    $206, $1fd, $1f5, $1ec, $1e4, $1dc, $1d4, $1cd,
    $1c5, $1be, $1b7, $1b0, $1a9, $1a2, $19b, $195,
    $18f, $188, $182, $17c, $177, $171, $16b, $166,
    $160, $15b, $155, $150, $14b, $146, $141, $13c,
    $137, $133, $12e, $129, $125, $121, $11c, $118,
    $114, $10f, $10b, $107, $103, $0ff, $0fb, $0f8,
    $0f4, $0f0, $0ec, $0e9, $0e5, $0e2, $0de, $0db,
    $0d7, $0d4, $0d1, $0cd, $0ca, $0c7, $0c4, $0c1,
    $0be, $0bb, $0b8, $0b5, $0b2, $0af, $0ac, $0a9,
    $0a7, $0a4, $0a1, $09f, $09c, $099, $097, $094,
    $092, $08f, $08d, $08a, $088, $086, $083, $081,
    $07f, $07d, $07a, $078, $076, $074, $072, $070,
    $06e, $06c, $06a, $068, $066, $064, $062, $060,
    $05e, $05c, $05b, $059, $057, $055, $053, $052,
    $050, $04e, $04d, $04b, $04a, $048, $046, $045,
    $043, $042, $040, $03f, $03e, $03c, $03b, $039,
    $038, $037, $035, $034, $033, $031, $030, $02f,
    $02e, $02d, $02b, $02a, $029, $028, $027, $026,
    $025, $024, $023, $022, $021, $020, $01f, $01e,
    $01d, $01c, $01b, $01a, $019, $018, $017, $017,
    $016, $015, $014, $014, $013, $012, $011, $011,
    $010, $00f, $00f, $00e, $00d, $00d, $00c, $00c,
    $00b, $00a, $00a, $009, $009, $008, $008, $007,
    $007, $007, $006, $006, $005, $005, $005, $004,
    $004, $004, $003, $003, $003, $002, $002, $002,
    $002, $001, $001, $001, $001, $001, $001, $001,
    $000, $000, $000, $000, $000, $000, $000, $000
  );

  (* exp table *)
  exprom: array[0..255] of UInt16 = (
    $7fa, $7f5, $7ef, $7ea, $7e4, $7df, $7da, $7d4,
    $7cf, $7c9, $7c4, $7bf, $7b9, $7b4, $7ae, $7a9,
    $7a4, $79f, $799, $794, $78f, $78a, $784, $77f,
    $77a, $775, $770, $76a, $765, $760, $75b, $756,
    $751, $74c, $747, $742, $73d, $738, $733, $72e,
    $729, $724, $71f, $71a, $715, $710, $70b, $706,
    $702, $6fd, $6f8, $6f3, $6ee, $6e9, $6e5, $6e0,
    $6db, $6d6, $6d2, $6cd, $6c8, $6c4, $6bf, $6ba,
    $6b5, $6b1, $6ac, $6a8, $6a3, $69e, $69a, $695,
    $691, $68c, $688, $683, $67f, $67a, $676, $671,
    $66d, $668, $664, $65f, $65b, $657, $652, $64e,
    $649, $645, $641, $63c, $638, $634, $630, $62b,
    $627, $623, $61e, $61a, $616, $612, $60e, $609,
    $605, $601, $5fd, $5f9, $5f5, $5f0, $5ec, $5e8,
    $5e4, $5e0, $5dc, $5d8, $5d4, $5d0, $5cc, $5c8,
    $5c4, $5c0, $5bc, $5b8, $5b4, $5b0, $5ac, $5a8,
    $5a4, $5a0, $59c, $599, $595, $591, $58d, $589,
    $585, $581, $57e, $57a, $576, $572, $56f, $56b,
    $567, $563, $560, $55c, $558, $554, $551, $54d,
    $549, $546, $542, $53e, $53b, $537, $534, $530,
    $52c, $529, $525, $522, $51e, $51b, $517, $514,
    $510, $50c, $509, $506, $502, $4ff, $4fb, $4f8,
    $4f4, $4f1, $4ed, $4ea, $4e7, $4e3, $4e0, $4dc,
    $4d9, $4d6, $4d2, $4cf, $4cc, $4c8, $4c5, $4c2,
    $4be, $4bb, $4b8, $4b5, $4b1, $4ae, $4ab, $4a8,
    $4a4, $4a1, $49e, $49b, $498, $494, $491, $48e,
    $48b, $488, $485, $482, $47e, $47b, $478, $475,
    $472, $46f, $46c, $469, $466, $463, $460, $45d,
    $45a, $457, $454, $451, $44e, $44b, $448, $445,
    $442, $43f, $43c, $439, $436, $433, $430, $42d,
    $42a, $428, $425, $422, $41f, $41c, $419, $416,
    $414, $411, $40e, $40b, $408, $406, $403, $400
  );

  (* freq mult table multiplied by 2: 1/2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 12, 12, 15, 15 *)
  mt: array[0..15] of UInt8 = (
    1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30
  );

  (* ksl table *)
  kslrom: array[0..15] of UInt8 = (
    0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64
  );

  kslshift: array[0..3] of UInt8 = (
    8, 1, 2, 0
  );

  (* envelope generator constants *)
  eg_incstep: array[0..3, 0..3] of UInt8 = (
    (0, 0, 0, 0),
    (1, 0, 0, 0),
    (1, 0, 1, 0),
    (1, 1, 1, 0)
  );

  (* address decoding *)
  ad_slot: array[0..$1F] of Int8 = (
    0, 1, 2, 3, 4, 5, -1, -1, 6, 7, 8, 9, 10, 11, -1, -1,
    12, 13, 14, 15, 16, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
  );

  ch_slot: array[0..17] of UInt8 = (
    0, 1, 2, 6, 7, 8, 12, 13, 14, 18, 19, 20, 24, 25, 26, 30, 31, 32
  );

(* Forward declarations *)
procedure OPL3_ChannelSetupAlg(channel: p_opl3_channel); forward;

(* Envelope Generator *)

function OPL3_EnvelopeCalcExp(level: UInt32): Int16;
begin
  if level > $1fff then
    level := $1fff;
  Result := Int16((exprom[level and $ff] shl 1) shr (level shr 8));
end;

function OPL3_EnvelopeCalcSin0(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
  neg: UInt16;
begin
  out_ := 0;
  neg := 0;
  phase := phase and $3ff;
  if (phase and $200) <> 0 then
    neg := $ffff;
  if (phase and $100) <> 0 then
    out_ := logsinrom[(phase and $ff) xor $ff]
  else
    out_ := logsinrom[phase and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3)) xor Int16(neg);
end;

function OPL3_EnvelopeCalcSin1(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
begin
  out_ := 0;
  phase := phase and $3ff;
  if (phase and $200) <> 0 then
    out_ := $1000
  else if (phase and $100) <> 0 then
    out_ := logsinrom[(phase and $ff) xor $ff]
  else
    out_ := logsinrom[phase and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3));
end;

function OPL3_EnvelopeCalcSin2(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
begin
  out_ := 0;
  phase := phase and $3ff;
  if (phase and $100) <> 0 then
    out_ := logsinrom[(phase and $ff) xor $ff]
  else
    out_ := logsinrom[phase and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3));
end;

function OPL3_EnvelopeCalcSin3(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
begin
  out_ := 0;
  phase := phase and $3ff;
  if (phase and $100) <> 0 then
    out_ := $1000
  else
    out_ := logsinrom[phase and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3));
end;

function OPL3_EnvelopeCalcSin4(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
  neg: UInt16;
begin
  out_ := 0;
  neg := 0;
  phase := phase and $3ff;
  if (phase and $300) = $100 then
    neg := $ffff;
  if (phase and $200) <> 0 then
    out_ := $1000
  else if (phase and $80) <> 0 then
    out_ := logsinrom[((phase xor $ff) shl 1) and $ff]
  else
    out_ := logsinrom[(phase shl 1) and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3)) xor Int16(neg);
end;

function OPL3_EnvelopeCalcSin5(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
begin
  out_ := 0;
  phase := phase and $3ff;
  if (phase and $200) <> 0 then
    out_ := $1000
  else if (phase and $80) <> 0 then
    out_ := logsinrom[((phase xor $ff) shl 1) and $ff]
  else
    out_ := logsinrom[(phase shl 1) and $ff];
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3));
end;

function OPL3_EnvelopeCalcSin6(phase: UInt16; envelope: UInt16): Int16;
var
  neg: UInt16;
begin
  neg := 0;
  phase := phase and $3ff;
  if (phase and $200) <> 0 then
    neg := $ffff;
  Result := OPL3_EnvelopeCalcExp(envelope shl 3) xor Int16(neg);
end;

function OPL3_EnvelopeCalcSin7(phase: UInt16; envelope: UInt16): Int16;
var
  out_: UInt16;
  neg: UInt16;
begin
  out_ := 0;
  neg := 0;
  phase := phase and $3ff;
  if (phase and $200) <> 0 then
  begin
    neg := $ffff;
    phase := (phase and $1ff) xor $1ff;
  end;
  out_ := phase shl 3;
  Result := OPL3_EnvelopeCalcExp(out_ + (envelope shl 3)) xor Int16(neg);
end;

type
  TEnvelopeSinFunc = function(phase: UInt16; envelope: UInt16): Int16;

const
  envelope_sin: array[0..7] of TEnvelopeSinFunc = (
    @OPL3_EnvelopeCalcSin0,
    @OPL3_EnvelopeCalcSin1,
    @OPL3_EnvelopeCalcSin2,
    @OPL3_EnvelopeCalcSin3,
    @OPL3_EnvelopeCalcSin4,
    @OPL3_EnvelopeCalcSin5,
    @OPL3_EnvelopeCalcSin6,
    @OPL3_EnvelopeCalcSin7
  );

procedure OPL3_EnvelopeUpdateKSL(slot: p_opl3_slot);
var
  ksl: Int16;
begin
  ksl := Int16((kslrom[slot^.channel^.f_num shr 6] shl 2) -
               ((8 - slot^.channel^.block) shl 5));
  if ksl < 0 then
    ksl := 0;
  slot^.eg_ksl := UInt8(ksl);
end;

procedure OPL3_EnvelopeCalc(slot: p_opl3_slot);
var
  nonzero: UInt8;
  rate: UInt8;
  rate_hi: UInt8;
  rate_lo: UInt8;
  reg_rate: UInt8;
  ks: UInt8;
  eg_shift, shift: UInt8;
  eg_rout: UInt16;
  eg_inc: Int16;
  eg_off: UInt8;
  reset: UInt8;
begin
  reg_rate := 0;
  reset := 0;
  slot^.eg_out := slot^.eg_rout + (slot^.reg_tl shl 2) +
                  (slot^.eg_ksl shr kslshift[slot^.reg_ksl]) + slot^.trem^;

  if (slot^.key <> 0) and (slot^.eg_gen = envelope_gen_num_release) then
  begin
    reset := 1;
    reg_rate := slot^.reg_ar;
  end
  else
  begin
    case slot^.eg_gen of
      envelope_gen_num_attack:
        reg_rate := slot^.reg_ar;
      envelope_gen_num_decay:
        reg_rate := slot^.reg_dr;
      envelope_gen_num_sustain:
        if slot^.reg_type = 0 then
          reg_rate := slot^.reg_rr;
      envelope_gen_num_release:
        reg_rate := slot^.reg_rr;
    end;
  end;

  slot^.pg_reset := reset;
  ks := slot^.channel^.ksv shr ((slot^.reg_ksr xor 1) shl 1);
  if reg_rate <> 0 then nonzero := 1 else nonzero := 0;
  rate := ks + (reg_rate shl 2);
  rate_hi := rate shr 2;
  rate_lo := rate and $03;
  if (rate_hi and $10) <> 0 then
    rate_hi := $0f;

  eg_shift := rate_hi + slot^.chip^.eg_add;
  shift := 0;

  if nonzero <> 0 then
  begin
    if rate_hi < 12 then
    begin
      if slot^.chip^.eg_state <> 0 then
      begin
        case eg_shift of
          12: shift := 1;
          13: shift := (rate_lo shr 1) and $01;
          14: shift := rate_lo and $01;
        end;
      end;
    end
    else
    begin
      shift := (rate_hi and $03) + eg_incstep[rate_lo, slot^.chip^.eg_timer_lo];
      if (shift and $04) <> 0 then
        shift := $03;
      if shift = 0 then
        shift := slot^.chip^.eg_state;
    end;
  end;

  eg_rout := slot^.eg_rout;
  eg_inc := 0;
  eg_off := 0;

  (* Instant attack *)
  if (reset <> 0) and (rate_hi = $0f) then
    eg_rout := $00;

  (* Envelope off *)
  if (slot^.eg_rout and $1f8) = $1f8 then
    eg_off := 1;

  if (slot^.eg_gen <> envelope_gen_num_attack) and (reset = 0) and (eg_off <> 0) then
    eg_rout := $1ff;

  case slot^.eg_gen of
    envelope_gen_num_attack:
    begin
      if slot^.eg_rout = 0 then
        slot^.eg_gen := envelope_gen_num_decay
      else if (slot^.key <> 0) and (shift > 0) and (rate_hi <> $0f) then
        eg_inc := (not Int16(slot^.eg_rout)) shr (4 - shift);
    end;
    envelope_gen_num_decay:
    begin
      if (slot^.eg_rout shr 4) = slot^.reg_sl then
        slot^.eg_gen := envelope_gen_num_sustain
      else if (eg_off = 0) and (reset = 0) and (shift > 0) then
        eg_inc := 1 shl (shift - 1);
    end;
    envelope_gen_num_sustain,
    envelope_gen_num_release:
    begin
      if (eg_off = 0) and (reset = 0) and (shift > 0) then
        eg_inc := 1 shl (shift - 1);
    end;
  end;

  slot^.eg_rout := (eg_rout + UInt16(eg_inc)) and $1ff;

  (* Key off *)
  if reset <> 0 then
    slot^.eg_gen := envelope_gen_num_attack;
  if slot^.key = 0 then
    slot^.eg_gen := envelope_gen_num_release;
end;

procedure OPL3_EnvelopeKeyOn(slot: p_opl3_slot; typ: UInt8);
begin
  slot^.key := slot^.key or typ;
end;

procedure OPL3_EnvelopeKeyOff(slot: p_opl3_slot; typ: UInt8);
begin
  slot^.key := slot^.key and (not typ);
end;

(* Phase Generator *)

procedure OPL3_PhaseGenerate(slot: p_opl3_slot);
var
  chip: p_opl3_chip;
  f_num: UInt16;
  basefreq: UInt32;
  rm_xor, n_bit: UInt8;
  noise: UInt32;
  phase: UInt16;
  range: Int8;
  vibpos: UInt8;
begin
  chip := slot^.chip;
  f_num := slot^.channel^.f_num;

  if slot^.reg_vib <> 0 then
  begin
    range := Int8((f_num shr 7) and 7);
    vibpos := slot^.chip^.vibpos;

    if (vibpos and 3) = 0 then
      range := 0
    else if (vibpos and 1) <> 0 then
      range := range shr 1;
    range := range shr slot^.chip^.vibshift;

    if (vibpos and 4) <> 0 then
      range := -range;
    f_num := UInt16(Int16(f_num) + range);
  end;

  basefreq := (UInt32(f_num) shl slot^.channel^.block) shr 1;
  phase := UInt16(slot^.pg_phase shr 9);

  if slot^.pg_reset <> 0 then
    slot^.pg_phase := 0;

  slot^.pg_phase := slot^.pg_phase + ((basefreq * mt[slot^.reg_mult]) shr 1);

  (* Rhythm mode *)
  noise := chip^.noise;
  slot^.pg_phase_out := phase;

  if slot^.slot_num = 13 then (* hh *)
  begin
    chip^.rm_hh_bit2 := (phase shr 2) and 1;
    chip^.rm_hh_bit3 := (phase shr 3) and 1;
    chip^.rm_hh_bit7 := (phase shr 7) and 1;
    chip^.rm_hh_bit8 := (phase shr 8) and 1;
  end;

  if (slot^.slot_num = 17) and ((chip^.rhy and $20) <> 0) then (* tc *)
  begin
    chip^.rm_tc_bit3 := (phase shr 3) and 1;
    chip^.rm_tc_bit5 := (phase shr 5) and 1;
  end;

  if (chip^.rhy and $20) <> 0 then
  begin
    rm_xor := ((chip^.rm_hh_bit2 xor chip^.rm_hh_bit7) or
               (chip^.rm_hh_bit3 xor chip^.rm_tc_bit5) or
               (chip^.rm_tc_bit3 xor chip^.rm_tc_bit5));
    case slot^.slot_num of
      13: (* hh *)
      begin
        slot^.pg_phase_out := UInt16(rm_xor) shl 9;
        if (rm_xor xor (noise and 1)) <> 0 then
          slot^.pg_phase_out := slot^.pg_phase_out or $d0
        else
          slot^.pg_phase_out := slot^.pg_phase_out or $34;
      end;
      16: (* sd *)
        slot^.pg_phase_out := (UInt16(chip^.rm_hh_bit8) shl 9) or
                              (UInt16(chip^.rm_hh_bit8 xor (noise and 1)) shl 8);
      17: (* tc *)
        slot^.pg_phase_out := (UInt16(rm_xor) shl 9) or $80;
    end;
  end;

  n_bit := ((noise shr 14) xor noise) and $01;
  chip^.noise := (noise shr 1) or (UInt32(n_bit) shl 22);
end;

(* Slot *)

procedure OPL3_SlotWrite20(slot: p_opl3_slot; data: UInt8);
begin
  if ((data shr 7) and $01) <> 0 then
    slot^.trem := @slot^.chip^.tremolo
  else
    slot^.trem := PUInt8(@slot^.chip^.zeromod);
  slot^.reg_vib := (data shr 6) and $01;
  slot^.reg_type := (data shr 5) and $01;
  slot^.reg_ksr := (data shr 4) and $01;
  slot^.reg_mult := data and $0f;
end;

procedure OPL3_SlotWrite40(slot: p_opl3_slot; data: UInt8);
begin
  slot^.reg_ksl := (data shr 6) and $03;
  slot^.reg_tl := data and $3f;
  OPL3_EnvelopeUpdateKSL(slot);
end;

procedure OPL3_SlotWrite60(slot: p_opl3_slot; data: UInt8);
begin
  slot^.reg_ar := (data shr 4) and $0f;
  slot^.reg_dr := data and $0f;
end;

procedure OPL3_SlotWrite80(slot: p_opl3_slot; data: UInt8);
begin
  slot^.reg_sl := (data shr 4) and $0f;
  if slot^.reg_sl = $0f then
    slot^.reg_sl := $1f;
  slot^.reg_rr := data and $0f;
end;

procedure OPL3_SlotWriteE0(slot: p_opl3_slot; data: UInt8);
begin
  slot^.reg_wf := data and $07;
  if slot^.chip^.newm = $00 then
    slot^.reg_wf := slot^.reg_wf and $03;
end;

procedure OPL3_SlotGenerate(slot: p_opl3_slot);
begin
  slot^.out_ := envelope_sin[slot^.reg_wf](slot^.pg_phase_out + UInt16(slot^.modp^), slot^.eg_out);
end;

procedure OPL3_SlotCalcFB(slot: p_opl3_slot);
begin
  if slot^.channel^.fb <> $00 then
    slot^.fbmod := (slot^.prout + slot^.out_) shr ($09 - slot^.channel^.fb)
  else
    slot^.fbmod := 0;
  slot^.prout := slot^.out_;
end;

(* Channel *)

procedure OPL3_ChannelUpdateRhythm(chip: p_opl3_chip; data: UInt8);
var
  channel6: p_opl3_channel;
  channel7: p_opl3_channel;
  channel8: p_opl3_channel;
  chnum: UInt8;
begin
  chip^.rhy := data and $3f;

  if (chip^.rhy and $20) <> 0 then
  begin
    channel6 := @chip^.channel[6];
    channel7 := @chip^.channel[7];
    channel8 := @chip^.channel[8];

    channel6^.out_[0] := @channel6^.slotz[1]^.out_;
    channel6^.out_[1] := @channel6^.slotz[1]^.out_;
    channel6^.out_[2] := @chip^.zeromod;
    channel6^.out_[3] := @chip^.zeromod;

    channel7^.out_[0] := @channel7^.slotz[0]^.out_;
    channel7^.out_[1] := @channel7^.slotz[0]^.out_;
    channel7^.out_[2] := @channel7^.slotz[1]^.out_;
    channel7^.out_[3] := @channel7^.slotz[1]^.out_;

    channel8^.out_[0] := @channel8^.slotz[0]^.out_;
    channel8^.out_[1] := @channel8^.slotz[0]^.out_;
    channel8^.out_[2] := @channel8^.slotz[1]^.out_;
    channel8^.out_[3] := @channel8^.slotz[1]^.out_;

    for chnum := 6 to 8 do
      chip^.channel[chnum].chtype := ch_drum;

    OPL3_ChannelSetupAlg(channel6);
    OPL3_ChannelSetupAlg(channel7);
    OPL3_ChannelSetupAlg(channel8);

    (* hh *)
    if (chip^.rhy and $01) <> 0 then
      OPL3_EnvelopeKeyOn(channel7^.slotz[0], egk_drum)
    else
      OPL3_EnvelopeKeyOff(channel7^.slotz[0], egk_drum);

    (* tc *)
    if (chip^.rhy and $02) <> 0 then
      OPL3_EnvelopeKeyOn(channel8^.slotz[1], egk_drum)
    else
      OPL3_EnvelopeKeyOff(channel8^.slotz[1], egk_drum);

    (* tom *)
    if (chip^.rhy and $04) <> 0 then
      OPL3_EnvelopeKeyOn(channel8^.slotz[0], egk_drum)
    else
      OPL3_EnvelopeKeyOff(channel8^.slotz[0], egk_drum);

    (* sd *)
    if (chip^.rhy and $08) <> 0 then
      OPL3_EnvelopeKeyOn(channel7^.slotz[1], egk_drum)
    else
      OPL3_EnvelopeKeyOff(channel7^.slotz[1], egk_drum);

    (* bd *)
    if (chip^.rhy and $10) <> 0 then
    begin
      OPL3_EnvelopeKeyOn(channel6^.slotz[0], egk_drum);
      OPL3_EnvelopeKeyOn(channel6^.slotz[1], egk_drum);
    end
    else
    begin
      OPL3_EnvelopeKeyOff(channel6^.slotz[0], egk_drum);
      OPL3_EnvelopeKeyOff(channel6^.slotz[1], egk_drum);
    end;
  end
  else
  begin
    for chnum := 6 to 8 do
    begin
      chip^.channel[chnum].chtype := ch_2op;
      OPL3_ChannelSetupAlg(@chip^.channel[chnum]);
      OPL3_EnvelopeKeyOff(chip^.channel[chnum].slotz[0], egk_drum);
      OPL3_EnvelopeKeyOff(chip^.channel[chnum].slotz[1], egk_drum);
    end;
  end;
end;

procedure OPL3_ChannelWriteA0(channel: p_opl3_channel; data: UInt8);
begin
  if (channel^.chip^.newm <> 0) and (channel^.chtype = ch_4op2) then
    Exit;

  channel^.f_num := (channel^.f_num and $300) or data;
  channel^.ksv := (channel^.block shl 1) or
                  ((channel^.f_num shr ($09 - channel^.chip^.nts)) and $01);

  OPL3_EnvelopeUpdateKSL(channel^.slotz[0]);
  OPL3_EnvelopeUpdateKSL(channel^.slotz[1]);

  if (channel^.chip^.newm <> 0) and (channel^.chtype = ch_4op) then
  begin
    channel^.pair^.f_num := channel^.f_num;
    channel^.pair^.ksv := channel^.ksv;
    OPL3_EnvelopeUpdateKSL(channel^.pair^.slotz[0]);
    OPL3_EnvelopeUpdateKSL(channel^.pair^.slotz[1]);
  end;
end;

procedure OPL3_ChannelWriteB0(channel: p_opl3_channel; data: UInt8);
begin
  if (channel^.chip^.newm <> 0) and (channel^.chtype = ch_4op2) then
    Exit;

  channel^.f_num := (channel^.f_num and $ff) or (UInt16(data and $03) shl 8);
  channel^.block := (data shr 2) and $07;
  channel^.ksv := (channel^.block shl 1) or
                  ((channel^.f_num shr ($09 - channel^.chip^.nts)) and $01);

  OPL3_EnvelopeUpdateKSL(channel^.slotz[0]);
  OPL3_EnvelopeUpdateKSL(channel^.slotz[1]);

  if (channel^.chip^.newm <> 0) and (channel^.chtype = ch_4op) then
  begin
    channel^.pair^.f_num := channel^.f_num;
    channel^.pair^.block := channel^.block;
    channel^.pair^.ksv := channel^.ksv;
    OPL3_EnvelopeUpdateKSL(channel^.pair^.slotz[0]);
    OPL3_EnvelopeUpdateKSL(channel^.pair^.slotz[1]);
  end;
end;

procedure OPL3_ChannelSetupAlg(channel: p_opl3_channel);
begin
  if channel^.chtype = ch_drum then
  begin
    if (channel^.ch_num = 7) or (channel^.ch_num = 8) then
    begin
      channel^.slotz[0]^.modp := @channel^.chip^.zeromod;
      channel^.slotz[1]^.modp := @channel^.chip^.zeromod;
      Exit;
    end;
    case channel^.alg and $01 of
      $00:
      begin
        channel^.slotz[0]^.modp := @channel^.slotz[0]^.fbmod;
        channel^.slotz[1]^.modp := @channel^.slotz[0]^.out_;
      end;
      $01:
      begin
        channel^.slotz[0]^.modp := @channel^.slotz[0]^.fbmod;
        channel^.slotz[1]^.modp := @channel^.chip^.zeromod;
      end;
    end;
    Exit;
  end;

  if (channel^.alg and $08) <> 0 then
    Exit;

  if (channel^.alg and $04) <> 0 then
  begin
    channel^.pair^.out_[0] := @channel^.chip^.zeromod;
    channel^.pair^.out_[1] := @channel^.chip^.zeromod;
    channel^.pair^.out_[2] := @channel^.chip^.zeromod;
    channel^.pair^.out_[3] := @channel^.chip^.zeromod;

    case channel^.alg and $03 of
      $00:
      begin
        channel^.pair^.slotz[0]^.modp := @channel^.pair^.slotz[0]^.fbmod;
        channel^.pair^.slotz[1]^.modp := @channel^.pair^.slotz[0]^.out_;
        channel^.slotz[0]^.modp := @channel^.pair^.slotz[1]^.out_;
        channel^.slotz[1]^.modp := @channel^.slotz[0]^.out_;
        channel^.out_[0] := @channel^.slotz[1]^.out_;
        channel^.out_[1] := @channel^.chip^.zeromod;
        channel^.out_[2] := @channel^.chip^.zeromod;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
      $01:
      begin
        channel^.pair^.slotz[0]^.modp := @channel^.pair^.slotz[0]^.fbmod;
        channel^.pair^.slotz[1]^.modp := @channel^.pair^.slotz[0]^.out_;
        channel^.slotz[0]^.modp := @channel^.chip^.zeromod;
        channel^.slotz[1]^.modp := @channel^.slotz[0]^.out_;
        channel^.out_[0] := @channel^.pair^.slotz[1]^.out_;
        channel^.out_[1] := @channel^.slotz[1]^.out_;
        channel^.out_[2] := @channel^.chip^.zeromod;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
      $02:
      begin
        channel^.pair^.slotz[0]^.modp := @channel^.pair^.slotz[0]^.fbmod;
        channel^.pair^.slotz[1]^.modp := @channel^.chip^.zeromod;
        channel^.slotz[0]^.modp := @channel^.pair^.slotz[1]^.out_;
        channel^.slotz[1]^.modp := @channel^.slotz[0]^.out_;
        channel^.out_[0] := @channel^.pair^.slotz[0]^.out_;
        channel^.out_[1] := @channel^.slotz[1]^.out_;
        channel^.out_[2] := @channel^.chip^.zeromod;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
      $03:
      begin
        channel^.pair^.slotz[0]^.modp := @channel^.pair^.slotz[0]^.fbmod;
        channel^.pair^.slotz[1]^.modp := @channel^.chip^.zeromod;
        channel^.slotz[0]^.modp := @channel^.pair^.slotz[1]^.out_;
        channel^.slotz[1]^.modp := @channel^.chip^.zeromod;
        channel^.out_[0] := @channel^.pair^.slotz[0]^.out_;
        channel^.out_[1] := @channel^.slotz[0]^.out_;
        channel^.out_[2] := @channel^.slotz[1]^.out_;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
    end;
  end
  else
  begin
    case channel^.alg and $01 of
      $00:
      begin
        channel^.slotz[0]^.modp := @channel^.slotz[0]^.fbmod;
        channel^.slotz[1]^.modp := @channel^.slotz[0]^.out_;
        channel^.out_[0] := @channel^.slotz[1]^.out_;
        channel^.out_[1] := @channel^.chip^.zeromod;
        channel^.out_[2] := @channel^.chip^.zeromod;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
      $01:
      begin
        channel^.slotz[0]^.modp := @channel^.slotz[0]^.fbmod;
        channel^.slotz[1]^.modp := @channel^.chip^.zeromod;
        channel^.out_[0] := @channel^.slotz[0]^.out_;
        channel^.out_[1] := @channel^.slotz[1]^.out_;
        channel^.out_[2] := @channel^.chip^.zeromod;
        channel^.out_[3] := @channel^.chip^.zeromod;
      end;
    end;
  end;
end;

procedure OPL3_ChannelUpdateAlg(channel: p_opl3_channel);
begin
  channel^.alg := channel^.con;
  if channel^.chip^.newm <> 0 then
  begin
    if channel^.chtype = ch_4op then
    begin
      channel^.pair^.alg := $04 or (channel^.con shl 1) or channel^.pair^.con;
      channel^.alg := $08;
      OPL3_ChannelSetupAlg(channel^.pair);
    end
    else if channel^.chtype = ch_4op2 then
    begin
      channel^.alg := $04 or (channel^.pair^.con shl 1) or channel^.con;
      channel^.pair^.alg := $08;
      OPL3_ChannelSetupAlg(channel);
    end
    else
      OPL3_ChannelSetupAlg(channel);
  end
  else
    OPL3_ChannelSetupAlg(channel);
end;

procedure OPL3_ChannelWriteC0(channel: p_opl3_channel; data: UInt8);
begin
  channel^.fb := (data and $0e) shr 1;
  channel^.con := data and $01;
  OPL3_ChannelUpdateAlg(channel);

  if channel^.chip^.newm <> 0 then
  begin
    if ((data shr 4) and $01) <> 0 then
      channel^.cha := $ffff
    else
      channel^.cha := 0;
    if ((data shr 5) and $01) <> 0 then
      channel^.chb := $ffff
    else
      channel^.chb := 0;
    if ((data shr 6) and $01) <> 0 then
      channel^.chc := $ffff
    else
      channel^.chc := 0;
    if ((data shr 7) and $01) <> 0 then
      channel^.chd := $ffff
    else
      channel^.chd := 0;
  end
  else
  begin
    channel^.cha := $ffff;
    channel^.chb := $ffff;
    channel^.chc := 0;
    channel^.chd := 0;
  end;
end;

procedure OPL3_ChannelKeyOn(channel: p_opl3_channel);
begin
  if channel^.chip^.newm <> 0 then
  begin
    if channel^.chtype = ch_4op then
    begin
      OPL3_EnvelopeKeyOn(channel^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOn(channel^.slotz[1], egk_norm);
      OPL3_EnvelopeKeyOn(channel^.pair^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOn(channel^.pair^.slotz[1], egk_norm);
    end
    else if (channel^.chtype = ch_2op) or (channel^.chtype = ch_drum) then
    begin
      OPL3_EnvelopeKeyOn(channel^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOn(channel^.slotz[1], egk_norm);
    end;
  end
  else
  begin
    OPL3_EnvelopeKeyOn(channel^.slotz[0], egk_norm);
    OPL3_EnvelopeKeyOn(channel^.slotz[1], egk_norm);
  end;
end;

procedure OPL3_ChannelKeyOff(channel: p_opl3_channel);
begin
  if channel^.chip^.newm <> 0 then
  begin
    if channel^.chtype = ch_4op then
    begin
      OPL3_EnvelopeKeyOff(channel^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOff(channel^.slotz[1], egk_norm);
      OPL3_EnvelopeKeyOff(channel^.pair^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOff(channel^.pair^.slotz[1], egk_norm);
    end
    else if (channel^.chtype = ch_2op) or (channel^.chtype = ch_drum) then
    begin
      OPL3_EnvelopeKeyOff(channel^.slotz[0], egk_norm);
      OPL3_EnvelopeKeyOff(channel^.slotz[1], egk_norm);
    end;
  end
  else
  begin
    OPL3_EnvelopeKeyOff(channel^.slotz[0], egk_norm);
    OPL3_EnvelopeKeyOff(channel^.slotz[1], egk_norm);
  end;
end;

procedure OPL3_ChannelSet4Op(chip: p_opl3_chip; data: UInt8);
var
  bit: UInt8;
  chnum: UInt8;
begin
  for bit := 0 to 5 do
  begin
    chnum := bit;
    if bit >= 3 then
      chnum := chnum + 9 - 3;

    if ((data shr bit) and $01) <> 0 then
    begin
      chip^.channel[chnum].chtype := ch_4op;
      chip^.channel[chnum + 3].chtype := ch_4op2;
      OPL3_ChannelUpdateAlg(@chip^.channel[chnum]);
    end
    else
    begin
      chip^.channel[chnum].chtype := ch_2op;
      chip^.channel[chnum + 3].chtype := ch_2op;
      OPL3_ChannelUpdateAlg(@chip^.channel[chnum]);
      OPL3_ChannelUpdateAlg(@chip^.channel[chnum + 3]);
    end;
  end;
end;

function OPL3_ClipSample(sample: Int32): Int16;
begin
  if sample > 32767 then
    sample := 32767
  else if sample < -32768 then
    sample := -32768;
  Result := Int16(sample);
end;

procedure OPL3_ProcessSlot(slot: p_opl3_slot);
begin
  OPL3_SlotCalcFB(slot);
  OPL3_EnvelopeCalc(slot);
  OPL3_PhaseGenerate(slot);
  OPL3_SlotGenerate(slot);
end;

procedure OPL3_Generate4Ch(chip: p_opl3_chip; buf4: PInt16);
var
  channel: p_opl3_channel;
  writebuf: ^opl3_writebuf;
  mix: array[0..1] of Int32;
  ii: UInt8;
  accm: Int16;
  shift: UInt8;
begin
  buf4[1] := OPL3_ClipSample(chip^.mixbuff[1]);
  buf4[3] := OPL3_ClipSample(chip^.mixbuff[3]);

  for ii := 0 to 35 do
    OPL3_ProcessSlot(@chip^.slot[ii]);

  mix[0] := 0;
  mix[1] := 0;
  for ii := 0 to 17 do
  begin
    channel := @chip^.channel[ii];
    accm := channel^.out_[0]^ + channel^.out_[1]^ + channel^.out_[2]^ + channel^.out_[3]^;
    mix[0] := mix[0] + Int16(accm and channel^.cha);
    mix[1] := mix[1] + Int16(accm and channel^.chc);
  end;
  chip^.mixbuff[0] := mix[0];
  chip^.mixbuff[2] := mix[1];

  buf4[0] := OPL3_ClipSample(chip^.mixbuff[0]);
  buf4[2] := OPL3_ClipSample(chip^.mixbuff[2]);

  mix[0] := 0;
  mix[1] := 0;
  for ii := 0 to 17 do
  begin
    channel := @chip^.channel[ii];
    accm := channel^.out_[0]^ + channel^.out_[1]^ + channel^.out_[2]^ + channel^.out_[3]^;
    mix[0] := mix[0] + Int16(accm and channel^.chb);
    mix[1] := mix[1] + Int16(accm and channel^.chd);
  end;
  chip^.mixbuff[1] := mix[0];
  chip^.mixbuff[3] := mix[1];

  if (chip^.timer and $3f) = $3f then
    chip^.tremolopos := (chip^.tremolopos + 1) mod 210;

  if chip^.tremolopos < 105 then
    chip^.tremolo := chip^.tremolopos shr chip^.tremoloshift
  else
    chip^.tremolo := (210 - chip^.tremolopos) shr chip^.tremoloshift;

  if (chip^.timer and $3ff) = $3ff then
    chip^.vibpos := (chip^.vibpos + 1) and 7;

  Inc(chip^.timer);

  shift := 0;
  if chip^.eg_state <> 0 then
  begin
    while (shift < 13) and (((chip^.eg_timer shr shift) and 1) = 0) do
      Inc(shift);

    if shift > 12 then
      chip^.eg_add := 0
    else
      chip^.eg_add := shift + 1;

    chip^.eg_timer_lo := UInt8(chip^.eg_timer and $3);
  end;

  if (chip^.eg_timerrem <> 0) or (chip^.eg_state <> 0) then
  begin
    if chip^.eg_timer = UInt64($fffffffff) then
    begin
      chip^.eg_timer := 0;
      chip^.eg_timerrem := 1;
    end
    else
    begin
      Inc(chip^.eg_timer);
      chip^.eg_timerrem := 0;
    end;
  end;

  chip^.eg_state := chip^.eg_state xor 1;

  writebuf := @chip^.writebuf[chip^.writebuf_cur];
  while writebuf^.time <= chip^.writebuf_samplecnt do
  begin
    if (writebuf^.reg and $200) = 0 then
      Break;
    writebuf^.reg := writebuf^.reg and $1ff;
    OPL3_WriteReg(chip, writebuf^.reg, writebuf^.data);
    chip^.writebuf_cur := (chip^.writebuf_cur + 1) mod OPL_WRITEBUF_SIZE;
    writebuf := @chip^.writebuf[chip^.writebuf_cur];
  end;
  Inc(chip^.writebuf_samplecnt);
end;

procedure OPL3_Generate(chip: p_opl3_chip; buf: PInt16);
var
  samples: array[0..3] of Int16;
begin
  OPL3_Generate4Ch(chip, @samples[0]);
  buf[0] := samples[0];
  buf[1] := samples[1];
end;

procedure OPL3_Generate4ChResampled(chip: p_opl3_chip; buf4: PInt16);
begin
  while chip^.samplecnt >= chip^.rateratio do
  begin
    chip^.oldsamples[0] := chip^.samples[0];
    chip^.oldsamples[1] := chip^.samples[1];
    chip^.oldsamples[2] := chip^.samples[2];
    chip^.oldsamples[3] := chip^.samples[3];
    OPL3_Generate4Ch(chip, @chip^.samples[0]);
    chip^.samplecnt := chip^.samplecnt - chip^.rateratio;
  end;

  buf4[0] := Int16((Int32(chip^.oldsamples[0]) * (chip^.rateratio - chip^.samplecnt) +
                    Int32(chip^.samples[0]) * chip^.samplecnt) div chip^.rateratio);
  buf4[1] := Int16((Int32(chip^.oldsamples[1]) * (chip^.rateratio - chip^.samplecnt) +
                    Int32(chip^.samples[1]) * chip^.samplecnt) div chip^.rateratio);
  buf4[2] := Int16((Int32(chip^.oldsamples[2]) * (chip^.rateratio - chip^.samplecnt) +
                    Int32(chip^.samples[2]) * chip^.samplecnt) div chip^.rateratio);
  buf4[3] := Int16((Int32(chip^.oldsamples[3]) * (chip^.rateratio - chip^.samplecnt) +
                    Int32(chip^.samples[3]) * chip^.samplecnt) div chip^.rateratio);

  chip^.samplecnt := chip^.samplecnt + (1 shl RSM_FRAC);
end;

procedure OPL3_GenerateResampled(chip: p_opl3_chip; buf: PInt16);
var
  samples: array[0..3] of Int16;
begin
  OPL3_Generate4ChResampled(chip, @samples[0]);
  buf[0] := samples[0];
  buf[1] := samples[1];
end;

procedure OPL3_Reset(chip: p_opl3_chip; samplerate: UInt32);
var
  slot: p_opl3_slot;
  channel: p_opl3_channel;
  slotnum: UInt8;
  channum: UInt8;
  local_ch_slot: UInt8;
begin
  FillChar(chip^, SizeOf(opl3_chip), 0);

  for slotnum := 0 to 35 do
  begin
    slot := @chip^.slot[slotnum];
    slot^.chip := chip;
    slot^.modp := @chip^.zeromod;
    slot^.eg_rout := $1ff;
    slot^.eg_out := $1ff;
    slot^.eg_gen := envelope_gen_num_release;
    slot^.trem := PUInt8(@chip^.zeromod);
    slot^.slot_num := slotnum;
  end;

  for channum := 0 to 17 do
  begin
    channel := @chip^.channel[channum];
    local_ch_slot := ch_slot[channum];
    channel^.slotz[0] := @chip^.slot[local_ch_slot];
    channel^.slotz[1] := @chip^.slot[local_ch_slot + 3];
    chip^.slot[local_ch_slot].channel := channel;
    chip^.slot[local_ch_slot + 3].channel := channel;

    if (channum mod 9) < 3 then
      channel^.pair := @chip^.channel[channum + 3]
    else if (channum mod 9) < 6 then
      channel^.pair := @chip^.channel[channum - 3];

    channel^.chip := chip;
    channel^.out_[0] := @chip^.zeromod;
    channel^.out_[1] := @chip^.zeromod;
    channel^.out_[2] := @chip^.zeromod;
    channel^.out_[3] := @chip^.zeromod;
    channel^.chtype := ch_2op;
    channel^.cha := $ffff;
    channel^.chb := $ffff;
    channel^.ch_num := channum;
    OPL3_ChannelSetupAlg(channel);
  end;

  chip^.noise := 1;
  chip^.rateratio := Int32((UInt32(samplerate) shl RSM_FRAC) div 49716);
  chip^.tremoloshift := 4;
  chip^.vibshift := 1;
end;

procedure OPL3_WriteReg(chip: p_opl3_chip; reg: UInt16; v: UInt8);
var
  high: UInt8;
  regm: UInt8;
begin
  high := (reg shr 8) and $01;
  regm := reg and $ff;

  case regm and $f0 of
    $00:
    begin
      if high <> 0 then
      begin
        case regm and $0f of
          $04: OPL3_ChannelSet4Op(chip, v);
          $05: chip^.newm := v and $01;
        end;
      end
      else
      begin
        case regm and $0f of
          $08: chip^.nts := (v shr 6) and $01;
        end;
      end;
    end;
    $20, $30:
    begin
      if ad_slot[regm and $1f] >= 0 then
        OPL3_SlotWrite20(@chip^.slot[18 * high + ad_slot[regm and $1f]], v);
    end;
    $40, $50:
    begin
      if ad_slot[regm and $1f] >= 0 then
        OPL3_SlotWrite40(@chip^.slot[18 * high + ad_slot[regm and $1f]], v);
    end;
    $60, $70:
    begin
      if ad_slot[regm and $1f] >= 0 then
        OPL3_SlotWrite60(@chip^.slot[18 * high + ad_slot[regm and $1f]], v);
    end;
    $80, $90:
    begin
      if ad_slot[regm and $1f] >= 0 then
        OPL3_SlotWrite80(@chip^.slot[18 * high + ad_slot[regm and $1f]], v);
    end;
    $e0, $f0:
    begin
      if ad_slot[regm and $1f] >= 0 then
        OPL3_SlotWriteE0(@chip^.slot[18 * high + ad_slot[regm and $1f]], v);
    end;
    $a0:
    begin
      if (regm and $0f) < 9 then
        OPL3_ChannelWriteA0(@chip^.channel[9 * high + (regm and $0f)], v);
    end;
    $b0:
    begin
      if (regm = $bd) and (high = 0) then
      begin
        chip^.tremoloshift := (((v shr 7) xor 1) shl 1) + 2;
        chip^.vibshift := ((v shr 6) and $01) xor 1;
        OPL3_ChannelUpdateRhythm(chip, v);
      end
      else if (regm and $0f) < 9 then
      begin
        OPL3_ChannelWriteB0(@chip^.channel[9 * high + (regm and $0f)], v);
        if (v and $20) <> 0 then
          OPL3_ChannelKeyOn(@chip^.channel[9 * high + (regm and $0f)])
        else
          OPL3_ChannelKeyOff(@chip^.channel[9 * high + (regm and $0f)]);
      end;
    end;
    $c0:
    begin
      if (regm and $0f) < 9 then
        OPL3_ChannelWriteC0(@chip^.channel[9 * high + (regm and $0f)], v);
    end;
  end;
end;

procedure OPL3_WriteRegBuffered(chip: p_opl3_chip; reg: UInt16; v: UInt8);
var
  time1, time2: UInt64;
  writebuf: ^opl3_writebuf;
  writebuf_last: UInt32;
begin
  writebuf_last := chip^.writebuf_last;
  writebuf := @chip^.writebuf[writebuf_last];

  if (writebuf^.reg and $200) <> 0 then
  begin
    OPL3_WriteReg(chip, writebuf^.reg and $1ff, writebuf^.data);
    chip^.writebuf_cur := (writebuf_last + 1) mod OPL_WRITEBUF_SIZE;
    chip^.writebuf_samplecnt := writebuf^.time;
  end;

  writebuf^.reg := reg or $200;
  writebuf^.data := v;
  time1 := chip^.writebuf_lasttime + OPL_WRITEBUF_DELAY;
  time2 := chip^.writebuf_samplecnt;

  if time1 < time2 then
    time1 := time2;

  writebuf^.time := time1;
  chip^.writebuf_lasttime := time1;
  chip^.writebuf_last := (writebuf_last + 1) mod OPL_WRITEBUF_SIZE;
end;

procedure OPL3_Generate4ChStream(chip: p_opl3_chip; sndptr1: PInt16; sndptr2: PInt16; numsamples: UInt32);
var
  i: UInt32;
  samples: array[0..3] of Int16;
begin
  for i := 0 to numsamples - 1 do
  begin
    OPL3_Generate4ChResampled(chip, @samples[0]);
    sndptr1[0] := samples[0];
    sndptr1[1] := samples[1];
    sndptr2[0] := samples[2];
    sndptr2[1] := samples[3];
    Inc(sndptr1, 2);
    Inc(sndptr2, 2);
  end;
end;

procedure OPL3_GenerateStream(chip: p_opl3_chip; sndptr: PInt16; numsamples: UInt32);
var
  i: UInt32;
begin
  for i := 0 to numsamples - 1 do
  begin
    OPL3_GenerateResampled(chip, sndptr);
    Inc(sndptr, 2);
  end;
end;

end.
