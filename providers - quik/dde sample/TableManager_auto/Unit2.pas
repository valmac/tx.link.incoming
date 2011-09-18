unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DdeExlUnt, Math;

type
  TForm2 = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    procedure OnUpdate(Topic: string; NewCells: TVariantTable; NewAddr: TRect);
    procedure Clear;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TForm2.OnUpdate;
var
  i, j: integer;
begin
  StringGrid1.RowCount := max(StringGrid1.RowCount, NewAddr.Bottom+1);
  StringGrid1.ColCount := max(StringGrid1.ColCount, NewAddr.Right+1);
  For i := NewAddr.Left to NewAddr.Right do
    For j := NewAddr.Top to NewAddr.Bottom do
      StringGrid1.Cells[i, j] := NewCells.Cells[j-NewAddr.Top, i-NewAddr.Left]
end;

procedure TForm2.Clear;
var
  i, j: integer;
begin
  For i := 0 to StringGrid1.RowCount-1 do
    For j := 0  to StringGrid1.ColCount-1 do
      StringGrid1.Cells[j, i] := '';
  StringGrid1.RowCount := 2;
  StringGrid1.ColCount := 2;
end;


procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
