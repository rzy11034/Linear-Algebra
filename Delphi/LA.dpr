program LA;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  LA.Main in 'Source\LA.Main.pas',
  LA.Utils in 'Source\LA.Utils.pas',
  LA.Vector.Main in 'Source\Vector\LA.Vector.Main.pas',
  LA.Vector in 'Source\Vector\LA.Vector.pas',
  LA.ArrayList in 'Source\Library\LA.ArrayList.pas',
  LA.Globals in 'Source\Library\LA.Globals.pas',
  LA.LinearSystem.Main in 'Source\LinearSystem\LA.LinearSystem.Main.pas',
  LA.LinearSystem in 'Source\LinearSystem\LA.LinearSystem.pas',
  LA.LU.Main in 'Source\LU\LA.LU.Main.pas',
  LA.Matrix.Main in 'Source\Matrix\LA.Matrix.Main.pas',
  LA.Matrix in 'Source\Matrix\LA.Matrix.pas',
  LA.LU in 'Source\LU\LA.LU.pas',
  LA.GramSchmidtProcess in 'Source\GramSchmidtProcess\LA.GramSchmidtProcess.pas',
  LA.GramSchmidtProcess.Main in 'Source\GramSchmidtProcess\LA.GramSchmidtProcess.Main.pas',
  LA.QR.Main in 'Source\GramSchmidtProcess\LA.QR.Main.pas';

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
