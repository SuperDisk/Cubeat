unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, bgrabitmap, BGRABitmapTypes;


type

  TSprite = record
    X, Y: Integer;
    Image: Integer;
    FlipX: Boolean;
    FlipY: Boolean;
  end;

  TAnimationFrame = array[0..10] of TSprite;

  TAnimation = array of TAnimationFrame;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    FlipXBox: TCheckBox;
    FlipYBox: TCheckBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    PaintBox1: TPaintBox;
    SpriteIdEdit: TSpinEdit;
    XSpinEdit: TSpinEdit;
    YSpinEdit: TSpinEdit;
    TileSpinEdit: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FlipXBoxChange(Sender: TObject);
    procedure FlipYBoxChange(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpriteIdEditChange(Sender: TObject);
    procedure TileSpinEditChange(Sender: TObject);
    procedure XSpinEditChange(Sender: TObject);
    procedure YSpinEditChange(Sender: TObject);
  private
    Frames: TAnimation;
    MDown: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bmp: TBitmap;
  spr: TSprite;
  b: TBGRABitmap;
begin
  if (listbox1.itemindex < 0) then exit;

  bmp := TBitmap.Create;

  with PaintBox1.Canvas do begin
    Brush.Color := clSkyBlue;
    FillRect(0, 0, Width, Height);

    for spr in Frames[ListBox1.ItemIndex] do begin
      b := TBGRABitmap.Create(32,64);
      imagelist1.GetBitmap(spr.Image, bmp);


      b.Canvas.StretchDraw(Rect(0, 0, bmp.width*4, bmp.height*4), bmp);
      if spr.FlipX then b.HorizontalFlip;
      if spr.Flipy then b.VerticalFlip;
      b.draw(paintbox1.canvas, spr.x*4, spr.y*4, False);
      b.free;
    end;
  end;
  bmp.free;
end;

procedure TForm1.SpriteIdEditChange(Sender: TObject);
var
  Frame: TAnimationFrame;
  Sprite: TSprite;
begin
  Frame := Frames[ListBox1.ItemIndex];
  Sprite := Frame[SpriteIdEdit.Value];

  XSpinEdit.Value := Sprite.X;
  YSpinEdit.Value:=Sprite.Y;
  TileSpinEdit.Value:=Sprite.Image;
  FlipXBox.Checked:=Sprite.FlipX;
  FlipYBox.Checked:=Sprite.FlipY;
  PaintBox1.Invalidate;
end;

procedure TForm1.TileSpinEditChange(Sender: TObject);
begin
  Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].Image := TileSpinEdit.Value;
  PaintBox1.Invalidate;
end;

procedure TForm1.XSpinEditChange(Sender: TObject);
begin
  Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].X := XSpinEdit.Value;
  PaintBox1.Invalidate;
end;

procedure TForm1.YSpinEditChange(Sender: TObject);
begin
  Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].Y := YSpinEdit.Value;
  PaintBox1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: integer;
begin
  SetLength(Frames, Length(Frames)+1);


  for i := low(frames[high(Frames)]) to high(frames[high(Frames)]) do begin
    frames[high(Frames)][i].x := -8;
  end;

  ListBox1.Items.Add('Frame '+IntToStr(Length(Frames)));
  PaintBox1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetLength(Frames, Length(Frames)-1);
  ListBox1.Items.Delete(ListBox1.Items.Count-1);
  PaintBox1.Invalidate;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  F: file of TAnimationFrame;
  Fr: TAnimationFrame;
begin
  AssignFile(F, 'myfile.dat');
  Rewrite(F);
  for Fr in Frames do begin
    Write(F, Fr);
  end;
  CloseFile(F);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  F: file of TAnimationFrame;
  I: Integer;
begin
  assignfile(F, 'myfile.dat');
  Reset(F);
  for I := 0 to High(frames) do
    Read(F, frames[i]);
  closefile(f);
  SpriteIdEditChange(nil);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  I: integer;
begin
  SetLength(Frames, Length(Frames)+1);

  for i := low(frames[high(Frames)]) to high(frames[high(Frames)]) do begin
    frames[high(Frames)][i] := frames[high(Frames)-1][i]
  end;

  ListBox1.Items.Add('Frame '+IntToStr(Length(Frames)));
  PaintBox1.Invalidate;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  F: TAnimationFrame;
  Spr: TSprite;
  I: Integer = 0;
  visi: array[0..8] of boolean = (False, False, False, False, False, False, False, False, False);
begin
  for F in Frames do begin
    I := 0;
    for Spr in f do begin
      if spr.x >= 0 then begin
        visi[I] := True;
        writeln('anim_sprite ', I, ',', spr.x, ',', spr.y, ',', spr.image, ',', integer(spr.FlipX), ',', integer(spr.flipY));
      end else begin
        if visi[I] then begin
          writeln('anim_sprite ', I, ',', spr.x, ',', -120, ',', spr.image, ',', integer(spr.FlipX), ',', integer(spr.flipY));
          visi[I] := False;
        end;
      end;
      Inc(I);
    end;
    writeln('anim_frame_end');
    writeln;
  end;
  writeln('anim_end');
end;

procedure TForm1.FlipXBoxChange(Sender: TObject);
begin
  Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].FlipX:=FlipXBox.Checked;
  PaintBox1.Invalidate;
end;

procedure TForm1.FlipYBoxChange(Sender: TObject);
begin
  Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].flipy := FlipYBox.Checked;
  PaintBox1.Invalidate;
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  SpriteIdEditChange(nil);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdown := true;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if mdown then begin
    Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].X := x div 4;
    Frames[ListBox1.ItemIndex][SpriteIdEdit.Value].y := y div 4;
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdown := false;
end;

end.

