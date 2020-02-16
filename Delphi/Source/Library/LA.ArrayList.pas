unit LA.ArrayList;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Defaults;

type
  TArrayList<T> = class
  private type
    TArray_T = TArray<T>;
    TComparer_T = TComparer<T>;

  var
    __data: TArray_T;
    __size: integer;

    procedure __reSize(newCapacity: integer);

  public
    /// <summary> 获取数组中的元数个数 </summary>
    function GetSize: integer;
    /// <summary> 获取数组的容量 </summary>
    function GetCapacity: integer;
    /// <summary> 返回数组是否有空 </summary>
    function IsEmpty: boolean;
    /// <summary> 获取index索引位置元素 </summary>
    function Get(index: integer): T;
    /// <summary> 获取第一个元素</summary>
    function GetFirst: T;
    /// <summary> 获取最后一个元素</summary>
    function GetLast: T;
    /// <summary> 修改index索引位置元素 </summary>
    procedure Set_(index: integer; e: T);
    /// <summary> 向所有元素后添加一个新元素 </summary>
    procedure AddLast(e: T);
    /// <summary> 在第index个位置插入一个新元素e </summary>
    procedure Add(index: integer; e: T);
    /// <summary> 在所有元素前添加一个新元素 </summary>
    procedure AddFirst(e: T);
    /// <summary> 查找数组中是否有元素e </summary>
    function Contains(e: T): boolean;
    /// <summary> 查找数组中元素e忆的索引，如果不存在元素e，则返回-1 </summary>
    function Find(e: T): integer;
    /// <summary> 从数组中删除index位置的元素，返回删除的元素 </summary>
    function Remove(index: integer): T;
    /// <summary> 从数组中删除第一个元素，返回删除的元素 </summary>
    function RemoveFirst: T;
    /// <summary> 从数组中删除i最后一个元素，返回删除的元素 </summary>
    function RemoveLast: T;
    /// <summary> 从数组中删除元素e </summary>
    procedure RemoveElement(e: T);
    /// <summary> 返回一个数组 </summary>
    function ToArray: TArray_T;
    function ToString: string; override;

    property Items[i: integer]: T read Get write Set_; default;

    /// <summary> 构造函数，传入数组构造Array </summary>
    constructor Create(const arr: array of T); overload;
    /// <summary>
    /// 构造函数，传入数组的容量capacity构造Array。
    /// 默认数组的容量capacity:=10
    /// </summary>
    constructor Create(capacity: integer = 10); overload;
  end;

implementation

{ TArrayList<T> }

procedure TArrayList<T>.Add(index: integer; e: T);
var
  i: integer;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Add failed. Require index >= 0 and index <= Size.');

  if (__size = Length(__data)) then
    __reSize(2 * Length(Self.__data));

  for i := __size - 1 downto index do
    __data[i + 1] := __data[i];

  __data[index] := e;
  inc(__size);

end;

procedure TArrayList<T>.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TArrayList<T>.AddLast(e: T);
begin
  Add(__size, e);
end;

function TArrayList<T>.Contains(e: T): boolean;
var
  i: integer;
  comparer: IComparer<T>;
begin
  comparer := TComparer_T.Default;

  for i := 0 to __size - 1 do
  begin
    if comparer.Compare(__data[i], e) = 0 then
      Exit(True);
  end;

  Result := False;
end;

constructor TArrayList<T>.Create(capacity: integer);
begin
  SetLength(__data, capacity);
end;

constructor TArrayList<T>.Create(const arr: array of T);
var
  i: integer;
begin
  SetLength(Self.__data, Length(arr));

  for i := 0 to Length(arr) - 1 do
    __data[i] := arr[i];

  __size := Length(arr);

end;

function TArrayList<T>.Find(e: T): integer;
var
  i: integer;
  comparer: IComparer<T>;
begin
  comparer := TComparer_T.Default;

  for i := 0 to __size - 1 do
  begin
    if comparer.Compare(__data[i], e) = 0 then
      Exit(i);
  end;

  Result := -1;
end;

function TArrayList<T>.Get(index: integer): T;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Get failed. Index is illegal.');

  Result := __data[index];
end;

function TArrayList<T>.GetCapacity: integer;
begin
  Result := Length(Self.__data);
end;

function TArrayList<T>.GetFirst: T;
begin
  Result := Get(0);
end;

function TArrayList<T>.GetLast: T;
begin
  Result := Get(__size - 1);
end;

function TArrayList<T>.GetSize: integer;
begin
  Result := Self.__size;
end;

function TArrayList<T>.IsEmpty: boolean;
begin
  Result := Self.__size = 0;
end;

function TArrayList<T>.Remove(index: integer): T;
var
  i: integer;
  res: T;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Remove failed. Index is illegal.');

  res := __data[index];

  for i := index + 1 to __size - 1 do
    __data[i - 1] := __data[i];
  Dec(Self.__size);

  if (__size = Length(Self.__data) div 4) and (Length(Self.__data) div 2 <> 0)
  then
    __reSize(Length(Self.__data) div 2);

  Result := res;
end;

procedure TArrayList<T>.RemoveElement(e: T);
var
  index, i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    index := Find(e);

    if index <> -1 then
      Remove(index);
  end;
end;

function TArrayList<T>.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TArrayList<T>.RemoveLast: T;
begin
  Result := Remove(__size - 1);
end;

procedure TArrayList<T>.Set_(index: integer; e: T);
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  __data[index] := e;
end;

function TArrayList<T>.ToArray: TArray_T;
var
  i: integer;
  arr: TArray_T;
begin
  SetLength(arr, __size);

  for i := 0 to __size - 1 do
    arr[i] := __data[i];

  Result := arr;
end;

function TArrayList<T>.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  value: TValue;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Array: Size = %d, capacity = %d',
      [Self.__size, Length(Self.__data)]);
    res.AppendLine;
    res.Append('  [');

    for i := 0 to __size - 1 do
    begin
      TValue.Make(@__data[i], TypeInfo(T), value);

      if (value.IsOrdinal) then
        res.Append(value.ToString)
      else
        res.Append(value.AsObject.ToString);

      if i <> __size - 1 then
        res.Append(', ');
    end;

    res.Append(']');
    Result := res.ToString;

  finally
    res.Free;
  end;
end;

procedure TArrayList<T>.__reSize(newCapacity: integer);
begin
  SetLength(Self.__data, newCapacity);
end;

end.
