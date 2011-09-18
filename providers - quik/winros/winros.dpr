program winros;

uses
  Forms,
  winros_main in 'winros_main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
