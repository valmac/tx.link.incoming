unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, TblMgrUnt, DdeExlUnt, Math, Menus;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    miTable: TMenuItem;
    miTableCreate: TMenuItem;
    miTableDelete: TMenuItem;
    miTableClear: TMenuItem;
    miTabledeleteAll: TMenuItem;
    N5: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure miTableCreateClick(Sender: TObject);
    procedure OnChildClose(Sender: TObject; var Action: TCloseAction);
    procedure miTableClearClick(Sender: TObject);
    procedure miTableDeleteClick(Sender: TObject);
    procedure miTabledeleteAllClick(Sender: TObject);
  private
    { Private declarations }
    Tablemgr: TTableManager;
  public
    Tables: TList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Tables := TList.Create;
  TableMgr := TTableManager.Create(self);
end;


procedure TForm1.miTableCreateClick(Sender: TObject);
var
  f: TForm2;
begin
  If Form3.ShowModal = mrOk then
  begin
    f := TForm2.Create(Application);
//    f.Parent := self;
    f.OnClose := OnChildClose;
    f.Caption := Form3.Edit1.Text;
    f.Show;
    Tables.Add(Pointer(f));
    TableMgr.AddTable(Form3.Edit1.Text, f.OnUpdate)
  end;

end;

procedure TForm1.OnChildClose;
begin
  TableMgr.DelTable((Sender as TForm2).Caption);
  Tables.Delete(Tables.IndexOf(Pointer(Sender)));
  (Sender as TForm2).Free
end;


procedure TForm1.miTableClearClick(Sender: TObject);
begin
  If MDIChildCount > 0  then
    (MDIChildren[0] as TForm2).Clear;
end;

procedure TForm1.miTableDeleteClick(Sender: TObject);
begin
  If MDIChildCount > 0  then
    If MessageDlg('¬ы действительно хотите удалить таблицу '''+(MDIChildren[0] as TForm2).Caption+
                  '''?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      (MDIChildren[0] as TForm2).Close;

end;

procedure TForm1.miTabledeleteAllClick(Sender: TObject);
var i: integer;
begin
  If MessageDlg('¬ы действительно хотите удалить все таблицы?', mtConfirmation,
                [mbYes, mbNo], 0) = mrYes then
    For i := MDIChildCount -1 downto 0 do
      (MDIChildren[i] as TForm2).Close;
end;

end.
