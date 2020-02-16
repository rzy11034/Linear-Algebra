unit LA.LU.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix, LA.LU;

procedure Main;

implementation

procedure Main;
var
  a1, a2, a3: TMatrix;
  lu1, lu2, lu3: T_LU;
begin
  a1 := TMatrix.Create([[1, 2, 3], [4, 5, 6], [3, -3, 5]]);
  lu1 := Lu(a1);
  Writeln(lu1.L^.ToString);
  Writeln(lu1.U^.ToString);
  Writeln(lu1.L^.Dot(lu1.U^).ToString);

  WriteLn;

  a2 := TMatrix.Create([[1, 4, 5, 3], [5, 22, 27, 11], [6, 19, 27, 31], [5, 28, 35, -8]]);
  lu2 := Lu(a2);
  Writeln(lu2.L^.ToString);
  Writeln(lu2.U^.ToString);
  Writeln(lu2.L^.Dot(lu2.U^).ToString);

  WriteLn;

  a3 := TMatrix.Create([[1, 2, 3], [3, 7, 14], [4, 13, 38]]);
  lu3 := Lu(a3);
  Writeln(lu3.L^.ToString);
  Writeln(lu3.U^.ToString);
  Writeln(lu3.L^.Dot(lu3.U^).ToString);
end;

end.
