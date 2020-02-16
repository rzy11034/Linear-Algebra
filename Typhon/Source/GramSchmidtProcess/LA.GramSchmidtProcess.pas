unit LA.GramSchmidtProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix,
  LA.Vector,
  LA.ArrayList,
  LA.LinearSystem;

type
  TVecList = specialize TArrayList<TVector>;

  TQR = record
    Q, R: PMatrix;
  end;

function GramSchmidtProcess(basis: TVectorArr): TVecList;

function QR(a: TMatrix): TQR;

implementation

function GramSchmidtProcess(basis: TVectorArr): TVecList;
var
  ret: TVecList;
  mtx: TMatrix;
  i, j: integer;
  p: TVector;
begin
  mtx := TMatrix.Create(basis);
  if Rank(mtx) <> Length(basis) then
    raise Exception.Create('Rank(mtx) <> Length(basis)');

  ret := TVecList.Create(Length(basis));
  ret.AddLast(basis[0]);
  for i := 1 to Length(basis) - 1 do
  begin
    p := basis[i];

    for j := 0 to ret.GetSize - 1 do
      p := p - basis[i].Dot(ret[j]) / ret[j].Dot(ret[j]) * ret[j];

    ret.AddLast(p);
  end;

  Result := ret;
end;

function QR(a: TMatrix): TQR;
var
  p: TVecList;
  basis, vecArr: TVectorArr;
  ret: TQR;
  i: integer;
  Q, R: TMatrix;
begin
  if a.Row_num <> a.Col_num then
    raise Exception.Create('A must be square');

  SetLength(basis, a.Col_num);
  for i := 0 to a.Col_num - 1 do
    basis[i] := a.Get_Col_vector(i);

  p := GramSchmidtProcess(basis);

  new(ret.Q);
  new(ret.R);

  SetLength(vecArr, p.GetSize);

  for i := 0 to p.GetSize - 1 do
    vecArr[i] := p[i] / p[i].Norm;

  Q := TMatrix.Create(vecArr).Transpose;
  R := Q.Transpose.Dot(a);

  ret.Q^ := Q;
  ret.R^ := R;
  Result := ret;
end;

end.
