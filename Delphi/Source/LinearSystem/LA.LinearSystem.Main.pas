unit LA.LinearSystem.Main;

interface

uses
  System.SysUtils;

procedure Main;

implementation

uses
  LA.LinearSystem,
  LA.Matrix,
  LA.Vector;

procedure Main;
var
  A1, A2, A3, A4, A5, A6, A7, A8, a, invA: TMatrix;
  b1, b2, b3, b4, b5, b6, b7, b8: TVector;
  ls1, ls2, ls3, ls4, ls5, ls6, ls7, ls8: TLinearSystem;
begin
  A1 := TMatrix.Create([[1, 2, 4], [3, 7, 2], [2, 3, 3]]);
  b1 := TVector.Create([7, -11, 1]);
  ls1 := TLinearSystem.Create(A1, b1);
  ls1.Gauss_Jordan_Elimination;
  WriteLn(ls1.ToString);
  WriteLn;

  A2 := TMatrix.Create([[1, -3, 5], [2, -1, -3], [3, 1, 4]]);
  b2 := TVector.Create([-9, 19, -13]);
  ls2 := TLinearSystem.Create(A2, b2);
  ls2.Gauss_Jordan_Elimination;
  WriteLn(ls2.ToString);
  WriteLn;

  A3 := TMatrix.Create([[1, 2, -2], [2, -3, 1], [3, -1, 3]]);
  b3 := TVector.Create([6, -10, -16]);
  ls3 := TLinearSystem.Create(A3, b3);
  ls3.Gauss_Jordan_Elimination;
  WriteLn(ls3.ToString);
  WriteLn;

  A4 := TMatrix.Create([[3, 1, -2], [5, -3, 10], [7, 4, 16]]);
  b4 := TVector.Create([4, 32, 13]);
  ls4 := TLinearSystem.Create(A4, b4);
  ls4.Gauss_Jordan_Elimination;
  WriteLn(ls4.ToString);
  WriteLn;

  A5 := TMatrix.Create([[6, -3, 2], [5, 1, 12], [8, 5, 1]]);
  b5 := TVector.Create([31, 36, 11]);
  ls5 := TLinearSystem.Create(A5, b5);
  ls5.Gauss_Jordan_Elimination;
  WriteLn(ls5.ToString);
  WriteLn;

  A6 := TMatrix.Create([[1, 1, 1], [1, -1, -1], [2, 1, 5]]);
  b6 := TVector.Create([3, -1, 8]);
  ls6 := TLinearSystem.Create(A6, b6);
  ls6.Gauss_Jordan_Elimination;
  WriteLn(ls6.ToString);
  WriteLn;

  A7 := TMatrix.Create([[1, -1, 2, 0, 3], [-1, 1, 0, 2, -5], [1, -1, 4, 2, 4], [-2, 2, -5, -1, -3]]);
  b7 := TVector.Create([1, 5, 13, -1]);
  ls7 := TLinearSystem.Create(A7, b7);
  ls7.Gauss_Jordan_Elimination;
  WriteLn(ls7.ToString);
  WriteLn;

  A8 := TMatrix.Create([[2, 2], [2, 1], [1, 2]]);
  b8 := TVector.Create([3, 2.5, 7]);
  ls8 := TLinearSystem.Create(A8, b8);
  if not ls8.Gauss_Jordan_Elimination then
    WriteLn('No Solution!');
  WriteLn(ls8.ToString);
  WriteLn;

  a := TMatrix.Create([[1, 2], [3, 4]]);
  invA := Inv(a)^;
  WriteLn(invA.ToString);
  WriteLn(a.Dot(invA).ToString);
  WriteLn(invA.Dot(a).ToString);
  WriteLn;

  WriteLn(Format('rank A8 = %d', [Rank(A8)]));
  WriteLn(Format('rank A7 = %d', [Rank(A7)]));
  WriteLn(Format('rank A6 = %d', [Rank(A6)]));
end;

end.
