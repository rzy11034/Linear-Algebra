unit LA.Matrix;

interface

uses
  System.SysUtils,
  System.Math,
  LA.Vector;

type
  TList2D = array of array of double;
  TShape = array [0 .. 1] of integer;

  TMatrix = record
  private
    __data: TList2D;

    /// <summary> 返回矩阵pos位置的元素 </summary>
    function __getItem(i, j: integer): double;

  public
    class function Create(list2D: TList2D): TMatrix; static;

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
    function Len: integer;

    function ToString: string;
    property Item[row, col: integer]: double read __getItem; default;
  end;

implementation

{ TMatrix }

function TMatrix.Col_num: integer;
begin
  Result := Self.Shape[0];
end;

function TMatrix.Col_vector(index: integer): TVector;
var
  tmp: TLists;
  i: integer;
begin
  SetLength(tmp, Self.Col_num);

  for i := 0 to Self.Col_num - 1 do
    tmp[i] := Self[i, index];

  Result := TVector.Create(tmp);
end;

class function TMatrix.Create(list2D: TList2D): TMatrix;
begin
  Result.__data := Copy(list2D);
end;

function TMatrix.Len: integer;
begin
  Result := Self.Row_num;
end;

function TMatrix.Row_num: integer;
begin
  Result := Self.Shape[1];
end;

function TMatrix.Row_vector(index: integer): TVector;
var
  tmp: TLists;
  i: integer;
begin
  SetLength(tmp, Self.Row_num);

  for i := 0 to Self.Row_num - 1 do
    tmp[i] := Self[index, i];

  Result := TVector.Create(tmp);
end;

function TMatrix.Shape: TShape;
begin
  Result[0] := Length(__data);
  Result[1] := Length(__data[0]);
end;

function TMatrix.Size: integer;
begin
  Result := Self.Shape[0] * Self.Shape[1];
end;

function TMatrix.ToString: string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendLine;

    for i := 0 to high(__data) do
    begin
      sb.Append(#9'[');

      for j := 0 to high(__data[0]) do
      begin
        //sb.Append('[');
        sb.Append(__data[i, j]);

        if j = high(__data[0]) then
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

function TMatrix.__getItem(i, j: integer): double;
begin
  Result := __data[i, j];
end;

end.
