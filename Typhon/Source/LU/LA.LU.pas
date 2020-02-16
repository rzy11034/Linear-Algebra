unit LA.LU;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix, LA.Vector, LA.Globals;

type
  T_LU = record
    L, U: PMatrix;
  end;

/// <summary> 返回LU分解 </summary>
function Lu(mtx: TMatrix): T_LU;

implementation

function Lu(mtx: TMatrix): T_LU;
var
  n, i, j: integer;
  a: array of TVector;
  l: TMatrix;
  p: double;
  tmpL, tmpU: TMatrix;
begin
  n := mtx.Row_num;
  SetLength(a, n);
  for i := 0 to n - 1 do
    a[i] := mtx.Get_Row_vector(i);
  l := TMatrix.Identity(n);

  for i := 0 to n - 1 do
  begin
    // A[i][i]位置是否可以是主元
    if Is_zero(a[i][i]) then
    begin
      Result.L := nil;
      Result.U := nil;
      Exit;
    end
    else
    begin
      for j := i + 1 to n - 1 do
      begin
        p := a[j][i] / a[i][i];
        a[j] := a[j] - p * a[i];
        l[j, i] := p;
      end;
    end;
  end;

  new(Result.L);
  new(Result.U);
  tmpL := TMatrix.Zero(n, n);
  tmpU := TMatrix.Zero(n, n);


  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
    begin
      tmpL[i, j] := l[i, j];
      tmpU[i, j] := a[i][j];
    end;
  end;

  Result.L^ := tmpL;
  Result.U^ := tmpU;
end;

end.
