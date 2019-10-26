﻿program LA;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  LA.Utils in 'Source\LA.Utils.pas',
  LA.Main in 'Source\LA.Main.pas',
  LA.Vector in 'Source\Vector\LA.Vector.pas',
  LA.Vector.Main in 'Source\Vector\LA.Vector.Main.pas',
  LA.Matrix in 'Source\Matrix\LA.Matrix.pas',
  LA.Matrix.Main in 'Source\Matrix\LA.Matrix.Main.pas',
  LA.LinearSystem in 'Source\LinearSystem\LA.LinearSystem.pas',
  LA.LinearSystem.Main in 'Source\LinearSystem\LA.LinearSystem.Main.pas';

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
