unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormShow(Sender: TObject);
begin
  Edit1.Text := '[]';
  Edit1.SetFocus;
end;

procedure TForm3.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  Case ord(Key) of
    13: ModalResult := mrOk;
    27: ModalResult := mrCancel;
  end;
end;

end.
