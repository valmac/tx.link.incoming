program DdeExcelServer;

uses
  Forms,
  unit1 in 'unit1.pas' {Form1},
  DdeExlUnt in 'DdeExlUnt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
