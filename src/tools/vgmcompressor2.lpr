program vgmcompressor2;

uses
  sysutils, fpjson, jsonparser, classes, GVector;

const
  MIN_LENGTH  = 3;          // minimum reference size
  MAX_LENGTH  = $7F;        // maximum literals / reference size
  WINDOW_SIZE = $FFF;       // sliding window size
  FLAG_LTRLS  = 0;          // indicates literals
  FLAG_PAIR   = $80;        // indicates length,offset pair
  FLAG_EOF    = 0;          // end of compressed data
  FLAG_MEGAFRAME = $80;
  FLAG_RESET_DECOMPRESSOR = $81;

type
  TSEQ = record
    Offset: Integer;
    Length: Integer;
  end;

  TLZBuffer = array[0..MAX_LENGTH-1] of Byte;
  TByteVector = specialize TVector<Byte>;
  TBankVector = specialize TVector<TByteVector>;

function Find(Data: array of Byte; Offset: Integer; Size: Integer; MaxLen: Integer): TSEQ;
var
  Match: TSEQ = (Offset: 0; Length: 0);
  Length, Window: Integer;
begin
  // initialize sliding window position and loop count
  if Offset < WINDOW_SIZE then
    Window := 0
  else
    Window := Offset - WINDOW_SIZE;

  // scan the window
  while Window < Offset do begin
    Length := 0;
    if Data[Window] = Data[Offset] then begin
      Inc(Length);
      while Data[Window + Length] = Data[Offset + Length] do begin
        // next byte
        Inc(Length);
        // avoid match size overflow
        if Length = MAX_LENGTH then
          Break;
        // avoid generating too many bytes
        if Length = MaxLen then
          Break;
        // stay in bounds
        if (window + length) > size then
          Break;
      end;
    end;
    // update if match is found and it's better then previous one
    if (Length <= MaxLen) and (Length >= MIN_LENGTH) and (Length > Match.Length) then begin
      Match.Offset := Window;
      Match.Length := Length;
    end;
    // advance to next byte in window
    Inc(Window);
  end;
  Result := Match;
end;

var
  F: Text;
  S: String;
  LoopFrame, LoopBank, LoopByte: Integer;
  Frames: TJSONArray;
  Data: TByteVector;
  CurBank: TByteVector;
  OutBanks: TBankVector;
  FProcessed: Integer;
  I, J: Integer;
  D, B: Integer;
  Size: Integer = 0;
  Best: TSEQ = (Offset: 0; Length: 0);
  FrameNeed, ThisBank: Integer;
  Buffer: TLZBuffer;

  procedure WriteBytes(B: array of Byte);
  var
    X: Byte;
  begin
    Inc(ThisBank, Length(B));
    for X in B do
      CurBank.PushBack(X);
  end;

  procedure FlushBuffer;
  begin
    WriteBytes([FLAG_LTRLS or B]);
    WriteBytes(Slice(Buffer, B));
    B := 0;
  end;

begin
  if paramstr(1) = '' then exit;

  Assign(F, ParamStr(1));
  Rewrite(F);

  ReadLn(LoopFrame);
  ReadLn(S);
  Frames := GetJSON(S) as TJSONArray;
  WriteLn(Frames.Count, ' frames in total');

  Data := TByteVector.Create;
  for I := 0 to Frames.Count-1 do
    for J := 0 to Frames.Arrays[I].Count-2 do
      Data.PushBack(Frames.Arrays[I].Integers[J]);

  D := 0;
  B := 0;
  Buffer := Default(TLZBuffer);
  Size := Data.Size;
  WriteLn('Original file size ', Size, ' bytes');

  FrameNeed := 0;
  ThisBank := 0;

  OutBanks := TBankVector.Create;
  CurBank := TByteVector.Create;

  LoopBank := -1;
  LoopByte := -1;
  FProcessed := -1;

  while D < Size do begin
    if FrameNeed = 0 then begin
      Inc(FProcessed);
      if B <> 0 then
        FlushBuffer;

      if Frame
    end;
  end;
end.

