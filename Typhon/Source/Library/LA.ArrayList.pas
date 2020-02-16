unit LA.ArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  generic TArrayList<T> = class
  private
    type
    TArray_T = array of T;

  var
    __data: TArray_T;
    __size: integer;

    procedure __reSize(newCapacity: integer);

  public
    ///<summary> 获取数组中的元数个数 </summary>
    function GetSize: integer;
    ///<summary> 获取数组的容量 </summary>
    function GetCapacity: integer;
    ///<summary> 返回数组是否有空 </summary>
    function IsEmpty: boolean;
    ///<summary> 获取index索引位置元素 </summary>
    function Get(index: integer): T;
    ///<summary> 获取第一个元素</summary>
    function GetFirst: T;
    ///<summary> 获取最后一个元素</summary>
    function GetLast: T;
    ///<summary> 修改index索引位置元素 </summary>
    procedure Set_(index: integer; e: T);
    ///<summary> 向所有元素后添加一个新元素 </summary>
    procedure AddLast(e: T);
    ///<summary> 在第index个位置插入一个新元素e </summary>
    procedure Add(index: integer; e: T);
    ///<summary> 在所有元素前添加一个新元素 </summary>
    procedure AddFirst(e: T);
    ///<summary> 查找数组中是否有元素e </summary>
    function Contains(e: T): boolean;
    ///<summary> 查找数组中元素e忆的索引，如果不存在元素e，则返回-1 </summary>
    function Find(e: T): integer;
    ///<summary> 从数组中删除index位置的元素，返回删除的元素 </summary>
    function Remove(index: integer): T;
    ///<summary> 从数组中删除第一个元素，返回删除的元素 </summary>
    function RemoveFirst: T;
    ///<summary> 从数组中删除i最后一个元素，返回删除的元素 </summary>
    function RemoveLast: T;
    ///<summary> 从数组中删除元素e </summary>
    procedure RemoveElement(e: T);
    ///<summary> 返回一个数组 </summary>
    function ToArray: TArray_T;
    function ToString: string; override;
    property Items[i: integer]: T read Get write Set_; default;

    ///<summary> 构造函数，传入数组构造Array </summary>
    constructor Create(const arr: array of T); overload;
    ///<summary>
    ///构造函数，传入数组的容量capacity构造Array。
    ///默认数组的容量capacity:=10
    ///</summary>
    constructor Create(capacity: integer = 10); overload;
  end;

implementation

{ TArrayList }

procedure TArrayList.Add(index: integer; e: T);
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
  Inc(__size);
end;

procedure TArrayList.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TArrayList.AddLast(e: T);
begin
  Add(__size, e);
end;

function TArrayList.Contains(e: T): boolean;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    if __data[i] = e then
      Exit(True);
  end;

  Result := False;
end;

constructor TArrayList.Create(capacity: integer);
begin
  SetLength(__data, capacity);
end;

constructor TArrayList.Create(const arr: array of T);
var
  i: integer;
begin
  SetLength(Self.__data, Length(arr));

  for i := 0 to Length(arr) - 1 do
    __data[i] := arr[i];

  __size := Length(arr);

end;

function TArrayList.Find(e: T): integer;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    if __data[i] = e then
      Exit(i);
  end;

  Result := -1;
end;

function TArrayList.Get(index: integer): T;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Get failed. Index is illegal.');

  Result := __data[index];
end;

function TArrayList.GetCapacity: integer;
begin
  Result := Length(Self.__data);
end;

function TArrayList.GetFirst: T;
begin
  Result := Get(0);
end;

function TArrayList.GetLast: T;
begin
  Result := Get(__size - 1);
end;

function TArrayList.GetSize: integer;
begin
  Result := Self.__size;
end;

function TArrayList.IsEmpty: boolean;
begin
  Result := Self.__size = 0;
end;

function TArrayList.Remove(index: integer): T;
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

  if (__size = Length(Self.__data) div 4) and (Length(Self.__data) div 2 <> 0) then
    __reSize(Length(Self.__data) div 2);

  Result := res;
end;

procedure TArrayList.RemoveElement(e: T);
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

function TArrayList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TArrayList.RemoveLast: T;
begin
  Result := Remove(__size - 1);
end;

procedure TArrayList.Set_(index: integer; e: T);
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  __data[index] := e;
end;

function TArrayList.ToArray: TArray_T;
var
  i: integer;
  arr: TArray_T;
begin
  SetLength(arr, __size);

  for i := 0 to __size - 1 do
    arr[i] := __data[i];

  Result := arr;
end;

function TArrayList.ToString: string;
var
  ret: TAnsiStringBuilder;
  i: integer;
begin
  ret := TStringBuilder.Create;
  try
    ret.AppendFormat('Array: Size = %d, capacity = %d',
      [Self.__size, Length(Self.__data)]);
    ret.AppendLine;
    ret.Append('  [');

    for i := 0 to __size - 1 do
    begin
      ret.Append(__data[i].ToString);

      if i <> __size - 1 then
        ret.Append(', ');
    end;

    ret.Append(']');
    Result := ret.ToString;

  finally
    ret.Free;
  end;
end;

procedure TArrayList.__reSize(newCapacity: integer);
begin
  SetLength(Self.__data, newCapacity);
end;

end.
