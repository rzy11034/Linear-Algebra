program LA;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  LA.Utils in 'Source\LA.Utils.pas',
  LA.Main in 'Source\LA.Main.pas',
  LA.Vector in 'Source\LA.Vector.pas';

begin
  try
    Run;
    TLAUtils.DrawLine;
    Writeln(END_OF_PROGRAM_EN);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
