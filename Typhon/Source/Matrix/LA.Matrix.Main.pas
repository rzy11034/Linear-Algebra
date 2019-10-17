unit LA.Matrix.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix;

procedure Main;

implementation

procedure Main;
var
  mtx: TMatrix;
begin
  mtx := TMatrix.Create([[1, 2], [3, 4]]);

  WriteLn(Format('matrix = %s', [mtx.ToString]));
  WriteLn(Format('matrix.shape = (%d, %d)', [mtx.Shape[0], mtx.Shape[1]]));
  WriteLn(Format('matrix.size = %d', [mtx.Size]));
  WriteLn(Format('matrix.len = %d', [mtx.len]));
  WriteLn(Format('matrix[0][0] = %s', [mtx[0, 0].ToString]));

  WriteLn(Format('matrix.Col_vector[0] = %s', [mtx.Col_vector(0).ToString]));
  WriteLn(Format('matrix.Row_vector[0] = %s', [mtx.Row_vector(0).ToString]));
end;

end.
