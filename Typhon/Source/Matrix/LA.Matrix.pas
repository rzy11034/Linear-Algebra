unit LA.Matrix;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //Math,
  LA.Vector;

type
  TList2D = array of array of double;

  TShape = object
    Row, Col: integer;
    function EqualTo(x: TShape): boolean;
    function ToString: string;
  end;

  TMatrix = object
  private
    __data: TList2D;

    /// <summary> 返回矩阵pos位置的元素 </summary>
    function __getItem(i, j: integer): double;
    /// <summary> 设置矩阵 i, j位置元素的值 </summary>
    procedure __setItem(i, j: integer; val: double);

  public
    class function Create(list2D: TList2D): TMatrix; static;
    class function Zero(col, row: integer): TMatrix; static;

    /// <summary> 返回矩阵的第index个行向量 </summary>
    function Row_vector(index: integer): TVector;
    /// <summary> 返回矩阵的第index个列向量 </summary>
    function Col_vector(index: integer): TVector;
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

    function Len: integer;

    function ToString: string;
    property Item[row, col: integer]: double read __getItem write __setItem; default;
  end;

operator +(const a, b: TMatrix): TMatrix;
operator -(const a, b: TMatrix): TMatrix;
operator * (const a: double; const b: TMatrix): TMatrix;
operator * (const a: TMatrix; const b: double): TMatrix;
operator / (const a: TMatrix; const b: double): TMatrix;
//operator * (const a, b: TVector): TVector;
operator +(const a: TMatrix): TMatrix;
operator -(const a: TMatrix): TMatrix;

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

operator +(const a, b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  if a.Shape.EqualTo(b.Shape) = False then
    raise Exception.Create('Error in adding. Shape of matrix must be same.');

  SetLength(ret.__data, a.Shape.Col, a.Shape.Row);

  for i := 0 to ret.Shape.Col - 1 do
  begin
    for j := 0 to ret.Shape.Row - 1 do
    begin
      ret.__data[i, j] := a[i, j] + b[i, j];
    end;
  end;

  Result := ret;
end;

operator -(const a, b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  if a.Shape.EqualTo(b.Shape) = False then
    raise Exception.Create('Error in subtracting. Shape of matrix must be same.');

  SetLength(ret.__data, a.Shape.Col, a.Shape.Row);

  for i := 0 to ret.Shape.Col - 1 do
  begin
    for j := 0 to ret.Shape.Row - 1 do
    begin
      ret.__data[i, j] := a[i, j] - b[i, j];
    end;
  end;

  Result := ret;
end;

operator * (const a: double; const b: TMatrix): TMatrix;
var
  i, j: integer;
  ret: TMatrix;
begin
  SetLength(ret.__data, b.Shape.Col, b.Shape.Row);

  for i := 0 to ret.Shape.Col - 1 do
  begin
    for j := 0 to ret.Shape.Row - 1 do
    begin
      ret.__data[i, j] := a * b[i, j];
    end;
  end;

  Result := ret;
end;

operator * (const a: TMatrix; const b: double): TMatrix;
begin
  Result := b * a;
end;

operator / (const a: TMatrix; const b: double): TMatrix;
begin
  Result := (1 / b) * a;
end;

operator +(const a: TMatrix): TMatrix;
begin
  Result := 1 * a;
end;

operator -(const a: TMatrix): TMatrix;
begin
  Result := -1 * a;
end;

function TMatrix.Col_num: integer;
begin
  Result := Self.Shape.Col;
end;

function TMatrix.Col_vector(index: integer): TVector;
var
  ret: TVector;
  i: integer;
begin
  ret := TVector.Zero(Self.Col_num);

  for i := 0 to Self.Col_num - 1 do
    ret[i] := self[i, index];

  Result := ret;
end;

class function TMatrix.Create(list2D: TList2D): TMatrix;
begin
  Result.__data := Copy(list2D);
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
  tmp := TVector.Zero(ret.Row_num);

  for j := 0 to Self.Col_num - 1 do
  begin
    //tmp := a.Row_vector(j);
    tmp := Self.Dot(a.Col_vector(j));


    for i := 0 to ret.Row_num - 1 do
      ret[i, j] := tmp[i];
  end;

  Result := ret;
end;

function TMatrix.Dot(const a: TVector): TVector;
var
  ret: TVector;
  i: integer;
begin
  if Self.Row_num <> a.Len then
    raise Exception.Create('Error in Matrix-Vector Multiplication.');

  ret := TVector.Zero(Self.Col_num);

  for i := 0 to Self.Col_num - 1 do
  begin
    ret[i] := a.Dot(Self.Row_vector(i));
  end;

  Result := ret;
end;

function TMatrix.Len: integer;
begin
  Result := Self.Row_num;
end;

function TMatrix.Row_num: integer;
begin
  Result := Self.Shape.Row;
end;

function TMatrix.Row_vector(index: integer): TVector;
begin
  Result := TVector.Create(__data[index]);
end;

function TMatrix.Shape: TShape;
begin
  Result.Row := Length(__data);
  Result.col := Length(__data[0]);
end;

function TMatrix.Size: integer;
begin
  Result := Self.Shape.Row * Self.Shape.Col;
end;

function TMatrix.ToString: string;
var
  sb: TAnsiStringBuilder;
  i, j: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    sb.AppendLine;

    for i := 0 to High(__data) do
    begin
      sb.Append(#9'[');

      for j := 0 to High(__data[0]) do
      begin
        //sb.Append('[');
        sb.Append(__data[i, j]);

        if j = High(__data[0]) then
          sb.Append(']'#10)
        else
          sb.Append(', ');
      end;
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TMatrix.Zero(col, row: integer): TMatrix;
var
  ret: TMatrix;
begin
  SetLength(ret.__data, col, row);
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
