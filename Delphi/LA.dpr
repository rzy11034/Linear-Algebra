program LA;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  LA.Utils in 'Source\LA.Utils.pas',
  LA.Main in 'Source\LA.Main.pas',
  LA.Vector in 'Source\Vector\LA.Vector.pas',
  LA.Vector.Main in 'Source\Vector\LA.Vector.Main.pas',
  LA.Matrix in 'Source\Matrix\LA.Matrix.pas',
  LA.Matrix.Main in 'Source\Matrix\LA.Matrix.Main.pas';

{$REGION 'MyRegion'}

{$ENDREGION}
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
