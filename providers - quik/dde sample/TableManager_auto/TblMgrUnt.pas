unit TblMgrUnt;

interface

uses
  DdeExlUnt, Types, Classes, Math, Controls, Sysutils;

type
  TTableChangeEvent = procedure(Topic: string; NewCells: TVariantTable; NewAddr: TRect) of object;

  TqkTable = class
    SubScriber: TTableChangeEvent;
    NewAddr: TRect;
    Active: Boolean;
  end;


  TTableManager = class(TWinControl)
  private
    fTables: TStringList;
    procedure DdePoke(Topic: string; var Action: TPokeAction);
    procedure DdeData(Topic: string; Cells: TRect; Data: TVariantTable);
    function GetActive(index: string): boolean;
    procedure SetActive(index: string; Value: boolean);
  protected

  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy;override;
    function    AddTable(AName: string; ASubscriber: TTableChangeEvent): integer;
    procedure   DelTable(AName: string);
    property    Active[TableName: string]: boolean read GetActive write SetActive;
  published

  end;

var
  TableMgr: TTableManager;

implementation

constructor TTableManager.Create;
begin
  inherited;
  fTables := TStringList.Create;
  DdeExcel := TDdeExcel.Create(AOwner);
  DdeExcel.OnPoke := DdePoke;
  DdeExcel.OnData := DdeData;
end;

destructor TTablemanager.Destroy;
var
  i: integer;
begin
  fTables.Clear;
  inherited;
end;

function TTableManager.AddTable(AName: string; ASubscriber: TTableChangeEvent): integer;
var
  t: TqkTable;
  i: integer;
begin
  If fTables.IndexOf(AName) > -1 then
  begin
    result := -1;
    Exit;
  end;
  t := TqkTable.Create;
  t.Active := true;
  t.SubScriber := ASubscriber;
  i := fTables.AddObject(AName, t);
  If i < 0 then
  begin
    t.Free;
    result := -2;
  end
  else
    result := i
end;

procedure TTableManager.DelTable(AName: string);
var
  i: integer;
begin
  i := fTables.IndexOf(AName);
  If i > -1 then
  begin
    fTables.Delete(i);
  end;
end;


procedure TTableManager.DdePoke(Topic: string; var Action: TPokeAction);
var
  i: integer;
begin
  i := fTables.IndexOf(Topic);
  If i < 0 then Action := paReject
  else If TqkTable(fTables.Objects[i]).Active then
    Action := paAccept
  else
    Action := paPass;
end;

procedure TTableManager.DdeData(Topic: string; Cells: TRect; Data: TVariantTable);
var
  t: TqkTable;
  i, j: integer;
  rc, cc: integer;
begin
  i := fTables.IndexOf(Topic);
  If i < 0 then Exit;
  t := TqkTable(fTables.Objects[i]);
  // Вызываем подписчика
  If Assigned(t.Subscriber) then t.SubScriber(Topic, Data, Cells);
end;

function TTableManager.GetActive(index: string): boolean;
var n: integer;
begin
  n := fTables.IndexOf(index);
  If n < 0 then Exit;
  result := TqkTable(fTables.Objects[n]).Active;
end;

procedure TTableManager.SetActive(index: string; Value: boolean);
var n: integer;
begin
  n := fTables.IndexOf(index);
  If n < 0 then Exit;
  TqkTable(fTables.Objects[n]).Active := Value;
end;

end.
