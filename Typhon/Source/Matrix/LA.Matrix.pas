﻿unit LA.Matrix;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  LA.Vector;

type
  TShape = record
    Row, Col: integer;
    function EqualTo(x: TShape): boolean;
    function ToString: string;
  end;

  TList2D = array of array of double;
  TVectorArr = array of TVector;

  PMatrix = ^TMatrix;
  TMatrix = record
  private
    __data: TList2D;

  public
    class function Create(list2D: TList2D): TMatrix; static;
    class function Create(vecArr: TVectorArr): TMatrix; static;
  /// <summary> 零矩阵 </summary>
    class function Zero(row, col: integer): TMatrix; static;
  /// <summary> 返回一个n行n列的单位矩阵 </summary>
    class function Identity(n: integer): TMatrix; static;


    /// <summary> 返回矩阵 pos 位置的元素 </summary>
    function __getItem(i, j: integer): double;
    /// <summary> 设置矩阵 pos 位置元素的值 </summary>
    procedure __setItem(i, j: integer; val: double);
    /// <summary> 返回矩阵的第index个行向量 </summary>
    function Get_Row_vector(index: integer): TVector;
    /// <summary> 返回矩阵的第index个列向量 </summary>
    function Get_Col_vector(index: integer): TVector;
    /// <summary> 设置矩阵的第index个行向量 </summary>
    procedure Set_Row(index: integer; vec: TVector);
    /// <summary> 设置矩阵的第index个列向量 </summary>
    procedure Set_Col(index: integer; Vec: TVector);
    /// <summary> 返回矩阵的元素个数 </summary>
    function Size: integer;
    /// <summary> 返回矩阵的行数 </summary>
    function Row_num: integer;
    /// <summary> 返回矩阵的列数 </summary>
    function Col_num: integer;
    /// <summary> 返回矩阵的形状: (行数， 列数) </summary>
    function Shape: TShape;
    /// <summary> 返回矩阵乘法的结果 </summary>
    function Dot(const a: TVector): TVector;
    /// <summary> 返回矩阵乘法的结果 </summary>
    function Dot(const a: TMatrix): TMatrix;
    /// <summary> 矩阵转置 </summary>
    function Transpose: TMatrix;
    function ToString: string;
    property Item[row, col: integer]: double read __getItem write __setItem; default;

    class operator +(const a, b: TMatrix): TMatrix;
    class operator -(const a, b: TMatrix): TMatrix;
    class operator * (const a: double; const b: TMatrix): TMatrix;
    class operator * (const a: TMatrix; const b: double): TMatrix;
    class operator / (const a: TMatrix; const b: double): TMatrix;
    class operator +(const a: TMatrix): TMatrix;
    class operator -(const a: TMatrix): TMatrix;
  end;

implementation

{ TShape }

function TShape.EqualTo(x: TShape): boolean;
begin
  Result := True;

  if Self.Col <> x.Col then
    Result := False;

  if Self.Row <> x.Row then
    Result := False;
end;

function TShape.ToString: string;
var
  sb: TAnsiStringBuilder;
begin
  sb := TAnsiStringBuilder.Create;
  try
    sb.Append('(');
    sb.Append(Self.Col);
    sb.Append(', ');
    sb.Append(Self.Row);
    sb.Append(')');

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

{ TMatrix }

class operator TMatrix. +(const a, b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  if a.Shape.EqualTo(b.Shape) = False then
    raise Exception.Create('Error in adding. Shape of matrix must be same.');

  ret := TMatrix.Zero(a.Row_num, a.Col_num);

  for i := 0 to ret.Shape.Col - 1 do
  begin
    for j := 0 to ret.Shape.Row - 1 do
    begin
      ret[i, j] := a[i, j] + b[i, j];
    end;
  end;

  Result := ret;
end;

class operator TMatrix. -(const a, b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  if a.Shape.EqualTo(b.Shape) = False then
    raise Exception.Create('Error in subtracting. Shape of matrix must be same.');

  ret := TMatrix.Zero(a.Row_num, a.Col_num);

  for i := 0 to ret.Shape.Col - 1 do
  begin
    for j := 0 to ret.Shape.Row - 1 do
    begin
      ret.__data[i, j] := a[i, j] - b[i, j];
    end;
  end;

  Result := ret;
end;

class operator TMatrix. * (const a: double; const b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  ret := TMatrix.Zero(b.Row_num, b.Col_num);

  for i := 0 to ret.Row_num - 1 do
  begin
    for j := 0 to ret.Col_num - 1 do
    begin
      ret[i, j] := a * b[i, j];
    end;
  end;

  Result := ret;
end;

class operator TMatrix. * (const a: TMatrix; const b: double): TMatrix;
begin
  Result := b * a;
end;

class operator TMatrix. / (const a: TMatrix; const b: double): TMatrix;
begin
  Result := (1 / b) * a;
end;

class operator TMatrix. +(const a: TMatrix): TMatrix;
begin
  Result := 1 * a;
end;

class operator TMatrix. -(const a: TMatrix): TMatrix;
begin
  Result := -1 * a;
end;

function TMatrix.Col_num: integer;
begin
  Result := Self.Shape.Col;
end;

class function TMatrix.Create(list2D: TList2D): TMatrix;
begin
  Result.__data := Copy(list2D);
end;

class function TMatrix.Create(vecArr: TVectorArr): TMatrix;
var
  n, i: integer;
  ret: LA.Matrix.TList2D;
begin
  n := Length(vecArr);
  SetLength(ret, n);

  for i := 0 to n - 1 do
  begin
    ret[i] := vecArr[i].UnderlyingList;
  end;

  Result.__data := Copy(ret);
end;

function TMatrix.Dot(const a: TMatrix): TMatrix;
var
  ret: TMatrix;
  tmp: TVector;
  i, j: integer;
begin
  if Self.Col_num <> a.Row_num then
    raise Exception.Create('Error in Matrix-Matrix Multiplication.');

  ret := TMatrix.Zero(Self.Row_num, a.Col_num);

  for j := 0 to a.Col_num - 1 do
  begin
    tmp := Self.Dot(a.Get_Col_vector(j));

    for i := 0 to tmp.Len - 1 do
      ret[i, j] := tmp[i];
  end;

  Result := ret;
end;

function TMatrix.Dot(const a: TVector): TVector;
var
  ret: TVector;
  i: integer;
begin
  if Self.Col_num <> a.Len then
    raise Exception.Create('Error in Matrix-Vector Multiplication.');

  ret := TVector.Zero(Self.Row_num);

  for i := 0 to ret.Len - 1 do
  begin
    ret[i] := Self.Get_Row_vector(i).Dot(a);
  end;

  Result := ret;
end;

function TMatrix.Get_Col_vector(index: integer): TVector;
var
  ret: TVector;
  i: integer;
begin
  ret := TVector.Zero(Self.Row_num);

  for i := 0 to ret.Len - 1 do
    ret[i] := Self[i, index];

  Result := ret;
end;

function TMatrix.Get_Row_vector(index: integer): TVector;
var
  tmp: TVector;
  i: integer;
begin
  tmp := TVector.Zero(Self.Col_num);

  for i := 0 to tmp.Len - 1 do
    tmp[i] := Self.__data[index, i];

  Result := tmp;
end;

class function TMatrix.Identity(n: integer): TMatrix;
var
  tmp: TList2D;
  i: integer;
begin
  SetLength(tmp, n, n);
  for i := 0 to Length(tmp) - 1 do
    tmp[i, i] := 1;

  Result := TMatrix.Create(tmp);
end;

function TMatrix.Row_num: integer;
begin
  Result := Self.Shape.Row;
end;

procedure TMatrix.Set_Col(index: integer; Vec: TVector);
var
  tmp: TVector;
  i: integer;
begin
  tmp := Get_Col_vector(index);
  if tmp.Len <> Vec.Len then
    raise Exception.Create('Error. Length of vectors must be same.');

  for i := 0 to Col_num - 1 do
  begin
    __data[i, index] := Vec[i];
  end;
end;

procedure TMatrix.Set_Row(index: integer; vec: TVector);
var
  tmp: TVector;
  i: integer;
begin
  tmp := Get_Row_vector(index);
  if tmp.Len <> Vec.Len then
    raise Exception.Create('Error. Length of vectors must be same.');

  for i := 0 to Row_num - 1 do
  begin
    __data[index, i] := Vec[i];
  end;
end;

function TMatrix.Shape: TShape;
begin
  Result.Row := Length(__data);
  Result.col := Length(__data[0]);
end;

function TMatrix.Size: integer;
begin
  Result := Self.Row_num * Self.Col_num;
end;

function TMatrix.ToString: string;
var
  sb: TAnsiStringBuilder;
  i, j: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    for i := 0 to High(__data) do
    begin
      sb.Append('[');

      for j := 0 to High(__data[0]) do
      begin
        sb.Append(__data[i, j]);

        if (j = High(__data[0])) and (i <> High(__data)) then
          sb.Append('],')
        else if (j = High(__data[0])) and (i = High(__data)) then
          sb.Append(']')
        else
          sb.Append(', ');
      end;
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TMatrix.Transpose: TMatrix;
var
  tmp: TList2D;
  i, j: integer;
begin
  SetLength(tmp, Self.Col_num, Row_num);

  for i := 0 to self.Row_num - 1 do
  begin
    for j := 0 to self.Col_num - 1 do
      tmp[j, i] := Self[i, j];
  end;

  Result := TMatrix.Create(tmp);
end;

class function TMatrix.Zero(row, col: integer): TMatrix;
var
  ret: TMatrix;
begin
  SetLength(ret.__data, row, col);
  Result := ret;
end;

function TMatrix.__getItem(i, j: integer): double;
begin
  Result := __data[i, j];
end;

procedure TMatrix.__setItem(i, j: integer; val: double);
begin
  __data[i, j] := val;
end;

end.
