program OPL3Vin;

uses sysutils, strutils, opl3;

var
  Command: Char;
  hexstr: string[4];
  Bstr: string[2];
  chip: p_opl3_chip;
  buf3: array[0..1] of Int16;
  savedval: Byte;
  Q: Integer;
  Samp: Integer;
  Pos: Double;

procedure sendsamples(value: integer);
begin
  write(Format('%0.4X', [value]));
end;

{$R *.res}

begin
  New(chip);
  OPL3_Reset(chip, 44100);

  Pos := 0;

  while True do begin
    Read(Command);

    case Command of
      'a': begin
        Read(hexstr);
        Q := Hex2Dec(hexstr);

        pos += (Q * 44100.0) / 4194304;
        while Int(pos) >= 1 do begin
          OPL3_GenerateResampled(chip, buf3);
          pos -= 1.0;
        end;

        Samp := (((Int64(buf3[0]) + Int64(buf3[1])) div 2) + (10502)) * 80 div (10502*2);

        if Samp < 0 then begin
          sendsamples(0);
          continue;
        end;

        Samp *= Q;

        sendsamples(Samp);
      end;
      'w': begin
        Read(hexstr);
        Read(Bstr);

        Q := Hex2Dec(hexstr);

        case Hex2Dec(hexstr) of
          $0001: SavedVal := Hex2Dec(Bstr);
          $0002: OPL3_WriteRegBuffered(chip, savedval, hex2dec(bstr));
          $0003: SavedVal := Hex2Dec(Bstr);
          $0004: OPL3_WriteRegBuffered(chip, $F00 or savedval, hex2dec(bstr));
        end;

        write('0');
      end;
      #10, #13: continue;
      else begin
        Writeln(StdErr, 'Unknown command received: ', Command);
        Halt;
      end;
    end;
  end;
end.
