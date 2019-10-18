unit LA.Matrix.Main;

interface

uses
  System.SysUtils,
  LA.Matrix, LA.Vector;

procedure Main;

implementation

procedure Main;
var
  mtx1, mtx2, mtx3: TMatrix;
  vec: TVector;
begin
  mtx1 := TMatrix.Create([[1, 2, 0], [3, 4, 0]]);

  WriteLn(Format('matrix = %s', [mtx1.ToString]));
  WriteLn(Format('matrix.shape = %s', [mtx1.Shape.ToString]));
  WriteLn(Format('matrix.size = %d', [mtx1.Size]));
  WriteLn(Format('matrix.len = %d', [mtx1.len]));
  WriteLn(Format('matrix[0][0] = %s', [mtx1[0, 0].ToString]));

  WriteLn(Format('matrix.Col_vector[0] = %s', [mtx1.Col_vector(0).ToString]));
  WriteLn(Format('matrix.Row_vector[0] = %s', [mtx1.Row_vector(0).ToString]));

  mtx2 := TMatrix.Create([[5, 6], [7, 8]]);

  Write('mtx1.Shape.EqualTo(mtx2.Shape) = ');
  WriteLn(mtx1.Shape.EqualTo(mtx2.Shape));
end;

end.
