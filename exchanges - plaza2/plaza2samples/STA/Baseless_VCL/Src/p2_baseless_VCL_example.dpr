program p2_baseless_VCL_example;

uses
  Forms,
  p2_baseless_VCL_example_main in 'p2_baseless_VCL_example_main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
