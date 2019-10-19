unit LA.Vector;

interface

uses
  System.SysUtils,
  System.Math;

const
  EPSILON = 1E-8;

type

  TVector = record
  private type
    TLists = Tarray<double>;

  var
    __data: TLists;

    /// <summary> 取向量的第index个元素 </summary>
    function __getItem(index: integer): double;
    /// <summary> 设置index个元素的值 </summary>
    procedure __setItem(index: integer; val: double);

  public
    class function Create(list: TLists): TVector; static;
    class function Zero(dim: integer): TVector; static;

    /// <summary> 返回向量长度（有多少个元素） </summary>
    function Len: integer;
    /// <summary> 返回向量的模 </summary>
    function Norm: double;
    /// <summary> 返回向量的单位向量 </summary>
    function Normalize: TVector;
    /// <summary> 向量点乘，返回结果标量 </summary>
    function Dot(const v: TVector): double;

    function ToString: string;
    property Item[index: integer]: double read __getItem write __setItem; default;

    class operator Add(const a, b: TVector): TVector;
    class operator Subtract(const a, b: TVector): TVector;
    class operator Multiply(const a: double; const b: TVector): TVector;
    class operator Multiply(const a: TVector; const b: double): TVector;
    class operator Multiply(const a, b: TVector): TVector;
    class operator Divide(const a: TVector; const b: double): TVector;
    class operator Positive(const a: TVector): TVector;
    class operator Negative(const a: TVector): TVector;
  end;

implementation

{ TVector }

class operator TVector.Add(const a, b: TVector): TVector;
var
  i: integer;
  ret: TVector;
begin
  if a.Len <> b.Len then
    raise Exception.Create('Error in adding. Length of vectors must be same.');

  ret := TVector.Zero(a.Len);

  for i := 0 to a.Len - 1 do
  begin
    ret.__data[i] := a[i] + b[i];
  end;

  Result := ret;
end;

class function TVector.Create(list: TLists): TVector;
begin
  Result.__data := copy(list);
end;

class operator TVector.Divide(const a: TVector; const b: double): TVector;
begin
  Result := 1 / b * a;
end;

function TVector.Dot(const v: TVector): double;
var
  i: integer;
  tmp: TVector;
begin
  if Self.Len <> v.Len then
    raise Exception.Create('Error in Dot-Product. Length of vectors must be same.');

  tmp := TVector.Zero(Self.Len);

  for i := 0 to Self.Len - 1 do
  begin
    tmp[i] := Self[i] * v[i];
  end;

  Result := Sum(tmp.__data);
end;

function TVector.__getItem(index: integer): double;
begin
  Result := __data[index];
end;

procedure TVector.__setItem(index: integer; val: double);
begin
  __data[index] := val;
end;

function TVector.Len: integer;
begin
  Result := System.Length(__data);
end;

class operator TVector.Multiply(const a, b: TVector): TVector;
var
  i: integer;
  ret: TVector;
begin
  if a.Len <> b.Len then
    raise Exception.Create('Error in Dot-Product. Length of vectors must be same.');

  ret := TVector.Zero(a.Len);

  for i := 0 to ret.Len - 1 do
  begin
    ret.__data[i] := a[i] * b[i];
  end;

  Result := ret;
end;

class operator TVector.Multiply(const a: TVector; const b: double): TVector;
begin
  Result := b * a;
end;

class operator TVector.Multiply(const a: double; const b: TVector): TVector;
var
  i: integer;
  ret: TVector;
begin
  ret := TVector.Zero(b.Len);

  for i := 0 to b.Len - 1 do
  begin
    ret.__data[i] := a * b[i];
  end;

  Result := ret;
end;

class operator TVector.Negative(const a: TVector): TVector;
begin
  Result := -1 * a;
end;

function TVector.Norm: double;
var
  tmp: TLists;
  i: integer;
begin
  tmp := copy(__data);

  for i := 0 to Self.Len - 1 do
  begin
    tmp[i] := sqr(__data[i]);
  end;

  Result := Sqrt(Sum(tmp));
end;

function TVector.Normalize: TVector;
var
  ret: TVector;
begin
  if Self.Norm < EPSILON then
    raise Exception.Create('Normalize error! norm is zero.');

  ret.__data := copy(__data);
  Result := ret / Self.Norm;
end;

class operator TVector.Positive(const a: TVector): TVector;
begin
  Result := 1 * a;
end;

class operator TVector.Subtract(const a, b: TVector): TVector;
var
  i: integer;
  ret: TVector;
begin
  if a.Len <> b.Len then
    raise Exception.Create('Error in subtracting. Length of vectors must be same.');

  SetLength(ret.__data, a.Len);

  for i := 0 to a.Len - 1 do
  begin
    ret.__data[i] := a[i] - b[i];
  end;

  Result := ret;
end;

function TVector.ToString: string;
var
  sb: TStringBuilder;
  i: integer;
begin

  sb := TStringBuilder.Create;
  try
    sb.Append('(');

    for i := 0 to high(__data) do
    begin
      sb.Append(__data[i]);

      if i = high(__data) then
        sb.Append(')')
      else
        sb.Append(', ');
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TVector.Zero(dim: integer): TVector;
var
  ret: TVector;
begin
  SetLength(ret.__data, dim);
  Result := ret;
end;

end.
