unit LA.LinearSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix,
  LA.Vector,
  LA.Globals,
  LA.ArrayList;

type
  TLinearSystem = class(TObject)
    type
    TVectorArr = array of TVector;
    TList = specialize TArrayList<integer>;

  private
    __row, __col: integer;

    procedure __forward;
    procedure __backward;
    function __max_row(index_i, index_j: integer): integer;
    procedure __swapRow(var a, b: TVector);

  public
    Ab: TVectorArr;
    Pivots: TList;

    constructor Create(mtx: TMatrix);
    constructor Create(mtx: TMatrix; vec: TVector);
    constructor Create(mtxA: TMatrix; mtxB: TMatrix);
    destructor Destroy; override;

    ///<summary> 高斯约旦消元法 </summary>
    function Gauss_Jordan_Elimination: boolean;

    function ToString: string; override;
  end;

/// <summary> 返回逆矩阵 </summary>
function Inv(mtx: TMatrix): PMatrix;
/// <summary> 返回矩阵的秩 </summary>
function Rank(mtx: TMatrix): integer;

implementation

function Inv(mtx: TMatrix): PMatrix;
var
  ls: TLinearSystem;
  n, i, j: integer;
  invMtx: LA.Matrix.TList2D;
begin
  if mtx.Row_num <> mtx.Col_num then
    Exit(nil);

  n := mtx.Row_num;
  ls := TLinearSystem.Create(mtx, TMatrix.Identity(n));
  if not ls.Gauss_Jordan_Elimination then
    Exit(nil);

  SetLength(invMtx, n, n);
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
    begin
      invMtx[i, j] := ls.Ab[i][j + n];
    end;
  end;

  new(Result);
  Result^ := TMatrix.Create(invMtx);
end;

function Rank(mtx: TMatrix): integer;
var
  ls: TLinearSystem;
  zeroVec, row: TVector;
  Count: integer;
begin
  ls := TLinearSystem.Create(mtx);
  zeroVec := TVector.Zero(mtx.Col_num);
  Count := 0;

  for row in ls.Ab do
  begin
    if row <> zeroVec then
      Count += 1;
  end;

  Result := Count;
end;

{ TLinearSystem }

constructor TLinearSystem.Create(mtx: TMatrix);
var
  tmpList: LA.Vector.TLists;
  tmpVecArr: TVectorArr;
  i: integer;
begin
  __row := mtx.Row_num;
  __col := mtx.Col_num;

  SetLength(tmpVecArr, __row);
  for i := 0 to __row - 1 do
  begin
    tmpList := Copy(mtx.Get_Row_vector(i).UnderlyingList);
    tmpVecArr[i] := TVector.Create(tmpList);
  end;

  Ab := tmpVecArr;
end;

constructor TLinearSystem.Create(mtxA: TMatrix; mtxB: TMatrix);
var
  tmpList: LA.Vector.TLists;
  tmpVecArr: TVectorArr;
  i: integer;
begin
  if mtxA.Row_num <> mtxB.Row_num then
    raise Exception.Create('row number of Matrix must be equal to the length of Row');

  __row := mtxA.Row_num;
  __col := mtxA.Col_num;

  SetLength(tmpVecArr, __row);
  for i := 0 to __row - 1 do
  begin
    tmpList := Copy(mtxA.Get_Row_vector(i).UnderlyingList);
    tmpList := Concat(tmpList, mtxB.Get_Row_vector(i).UnderlyingList);
    tmpVecArr[i] := TVector.Create(tmpList);
  end;

  Ab := tmpVecArr;
  Pivots := TList.Create;
end;

constructor TLinearSystem.Create(mtx: TMatrix; vec: TVector);
var
  tmpList: LA.Vector.TLists;
  tmpVecArr: TVectorArr;
  i: integer;
begin
  if mtx.Row_num <> vec.Len then
    raise Exception.Create('row number of Matrix must be equal to the length of Vector');

  __row := mtx.Row_num;
  __col := mtx.Col_num;

  SetLength(tmpVecArr, __row);
  for i := 0 to __row - 1 do
  begin
    tmpList := Copy(mtx.Get_Row_vector(i).UnderlyingList);
    tmpList := Concat(tmpList, [vec[i]]);
    tmpVecArr[i] := TVector.Create(tmpList);
  end;

  Ab := tmpVecArr;
  Pivots := TList.Create;
end;

destructor TLinearSystem.Destroy;
begin
  FreeAndNil(Pivots);
  inherited Destroy;
end;

function TLinearSystem.Gauss_Jordan_Elimination: boolean;
var
  i: integer;
begin
  // 如果有解，返回True；如果没有解，返回False
  __forward;
  __backward;

  for i := Pivots.GetSize to __row - 1 do
  begin

    if not Is_zero(Ab[i][Ab[i].Len - 1]) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;
end;

function TLinearSystem.ToString: string;
var
  sb: TAnsiStringBuilder;
  i, j: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    for i := 0 to __row - 1 do
    begin
      for j := 0 to __col - 1 do
      begin
        sb.Append(Ab[i][j]).Append(#9);
      end;

      sb.Append(' |'#9).Append(Ab[i][__col]).Append(#10);
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TLinearSystem.__backward;
var
  i, j, n, k: integer;
begin
  n := Pivots.GetSize;

  for i := n - 1 downto 0 do
  begin
    k := Pivots[i];
    // Ab[i][k]为主元
    for j := i - 1 downto 0 do
      Ab[j] := Ab[j] - Ab[j][k] * Ab[i];
  end;

end;

procedure TLinearSystem.__forward;
var
  i, j, k, max_row: integer;
begin
  i := 0;
  k := 0;

  while (i < __row) and (k < __col) do
  begin
    // Ab[i][k]位置是否可以是主元
    max_row := __max_row(i, k);
    __swapRow(Ab[i], Ab[max_row]);

    if Is_zero(Ab[i][k]) then
      k += 1
    else
    begin
      // 将主元归为一
      Ab[i] := Ab[i] / Ab[i][k];
      for j := i + 1 to __row - 1 do
        Ab[j] := Ab[j] - Ab[j][k] * Ab[i];

      Pivots.AddLast(k);
      i += 1;
    end;
  end;
end;

function TLinearSystem.__max_row(index_i, index_j: integer): integer;
var
  best: double;
  ret, i: integer;
begin
  best := Ab[index_i][index_j];
  ret := index_i;

  for i := index_i + 1 to __row - 1 do
  begin
    if Ab[i][index_j] > best then
    begin
      best := Ab[i][index_j];
      ret := i;
    end;
  end;

  Result := ret;
end;

procedure TLinearSystem.__swapRow(var a, b: TVector);
var
  tmp: TVector;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

end.
