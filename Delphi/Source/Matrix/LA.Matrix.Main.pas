unit LA.Matrix.Main;

interface

uses
  System.SysUtils,
  LA.Matrix,
  LA.Vector;

procedure Main;

implementation

procedure Main;
var
  mtx1, mtx2, mtx3, mtx4: TMatrix;
  vec: TVector;
begin
  mtx1 := TMatrix.Create([[1, 2], [3, 4]]);


  mtx2 := TMatrix.Create([[5, 6], [7, 8]]);

  Write('mtx1.Shape.EqualTo(mtx2.Shape) = ');
  WriteLn(mtx1.Shape.EqualTo(mtx2.Shape));

  

  WriteLn(Format('zero = %s', [(TMatrix.Zero(2, 3)).ToString]));

  mtx3 := TMatrix.Create([[1.5, 0], [0, 2]]);
  vec := TVector.Create([5, 3]);
  WriteLn(Format('T.dot(p) = %s ', [mtx3.Dot(vec).ToString]));

  mtx4 := TMatrix.Create([[0, 4, 5], [0, 0, 3]]);
  WriteLn(Format('mtx3.dot(mtx4) = %s ', [mtx3.Dot(mtx4).ToString]));

  WriteLn(Format('A dot B = %s ', [mtx1.Dot(mtx2).ToString]));
  WriteLn(Format('B dot A = %s ', [mtx2.Dot(mtx1).ToString]));

end;

end.                           
